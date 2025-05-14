# new crm
# 1. predict total stem wood volume
# 2. can then use ratio equation to get wood volume for any stem section (stump, merch, top)
# 3. convert wood volume to mass using wood density
# 4. predict total stem bark volume
# 5. directly predict branch + stem bark biomass
# 6. separate into components using beta regression
# 7. determine difference between wood mass + bark mass + branch mass and total mass
# 8. proportionally distribute difference across the three components

# library(data.table)
# library(minpack.lm)
# library(betareg)

# setwd("~/Documents/Work/R_Data/FIA/fia_nsbe_4/")

tree <- data.table::fread(file.path(nsbe_repo_path, "Output/NSBE_FORMAT/NSBE_TREE.csv.gz"))
tree$k <- ifelse(tree$SPCD < 300, 9, 11)

# add puerto rico indicator - don't want them for this
tree$PR <- grepl('PR_', tree$AUTHOR)
tree <- tree[tree$PR == FALSE, ]
tree$PR <- NULL

stem <- data.table::fread(file.path(nsbe_repo_path, "Output/NSBE_FORMAT/NSBE_STEM.csv.gz"))
ref  <- data.table::fread("../Shared/Files/REF_SPECIES.csv",
                          select= c("SPCD", "GENUS", "JENKINS_SPGRPCD",
                                    "WOOD_SPGR_GREENVOL_DRYWT",
                                    "BARK_SPGR_GREENVOL_DRYWT"))
names(ref) <- c("SPCD", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

ref$SPCD <- as.character(ref$SPCD)

add_ref <- ref[ref$SPCD %in% c(111, 131),]
add_ref$SPCD <- paste0("1_", add_ref$SPCD)

ref <- rbind(ref, add_ref)

N_SPCD_VOL <- 80 # minimum SS for species
N_GENUS_VOL <- 80 # minimum SS for Genus

N_SPCD_BIO <- 50
N_GENUS_BIO <- 50

# ecodivisions
ecodiv <- read.csv(file.path(nsbe_repo_path,
                             "Work/Shared/CRM2/Coefs/NSBE_Ecodivisions.csv"),
                   as.is= TRUE)

tree <- merge(x= tree,
              y= ecodiv,
              by= c("AUTHOR", "LOC"))

# make planted loblolly, slash own group
# make same as FIADB: 0= natural, 1= planted
tree$STDORGCD <- ifelse(tree$TR_ORIGIN > 1, 1, 0)
# if missing make natural
tree[is.na(tree$STDORGCD), ]$STDORGCD <- 0

tree$SPCD2 <- ifelse(tree$SPCD %in% c(111, 131) & tree$STDORGCD == 1,
                     paste0("1_", tree$SPCD),
                     tree$SPCD)

ids <- tree[,c("AUTHOR", "LOC", "SPCD", "TREENO", "SPCD2")]

stem <- merge(x= stem,
              y= ids,
              by= c("AUTHOR", "LOC", "SPCD", "TREENO"))

stem$SPCD <- stem$SPCD2
tree$SPCD <- tree$SPCD2

stem$SPCD2 <- NULL
tree$SPCD2 <- NULL

# load equation guide
eq_guide2 <- read.csv("Files/crm2_equation_guide_update.csv", as.is= TRUE)

eq_guide <- eq_guide2

for (i in 2:ncol(eq_guide)) {
  
  eq_guide[,i] <- ifelse(eq_guide[,i] < 25,
                         eq_guide[,i] - 2,
                         eq_guide[,i])
  
}

# equation forms and starting values
eq_forms <- list(NUMBER= c(1, 2, 3, 4, 50, 1.1),
                 RHS= c("~ a * DBH^b * THT^c",
                        "~ ifelse(DBH < k,
                              a0 * DBH^b0 * THT^c,
                              a0 * k^(b0 - b1) * DBH^b1 * THT^c)",
                        "~ a * DBH^(a1 * (1 - exp(-b1 * (DBH)))^c1) * THT^c",
                        "~ (1 - (1 - X)^alpha)^beta",
                        "~ a * DBH^b * THT^c * exp(-(b2 * DBH))",
                        "~ a * DBH^b * THT^c * WDSG"),
                 START= list(c(a= 0.1, b= 2, c= 1),
                             c(a0= 0.05, b0= 2, b1= 2, c= 1),
                             c(a= 0.1, a1= 2, b1= 0.01, c1= 0.409, c= 1.062),
                             c(alpha= 2.2787, beta= 0.9171),
                             c(a= -6, b= 2, c= 1, b2= 0.001),
                             c(a= 0.1, b= 2, c= 1)))

makeVars <- function(data, levels){
  
  for (i in levels) {
    
    call <- paste0(i, "_DIVISION <- paste(", i, ", DIVISION)")
    
    data <- within(data, eval(parse(text= call)))
    
  }
  
  return(data)
  
}

tree <- makeVars(tree, "SPCD")

#
# helper funs------------------------------------------------------------------
getCoefs <- function(model) {
  
  mm <- summary(model)$coef
  
  # betareg has coefficients stored differently
  if(any(names(mm) == "mean")) {
    
    mm <- mm$mean
    
  }
  
  out <- as.data.frame(t(mm))
  
  out[1,]
  
}

fitModel <- function(data, formula, start, ...) {
  
  #cat("fitting for group", unique(data[,group]), "\n")
  
  for (ll in 1:100) {
    
    fit <- NULL
    
    # attempts to fit a nonlinear model and catches the error if it fails
    fit <- try(nlsLM(data= data,
                     formula= formula,
                     start= start,
                     weights= weights,
                     control= list(maxiter= 1024,
                                   maxfev= 1024)))
    
    # if no error break out of the loop
    if (class(fit) != "try-error") {
      
      # print("success")
      
      break
      
    } else {
      
      if(ll %% 2 == 1) {
        
        start= abs(start + jitter(start))
        
      } else {
        
        start= abs(start - jitter(start))
        
      }
      
      
    }
    
  }
  
  return(fit)
  
}

fitBeta <- function(data, formula) {
  
  betareg(data= data,
          formula= formula)
  
}

# data= total_by[[1]]
# group= "SPCD_DIVISION"
# funName= "fitModel"
# lhs= "TT_WDBK_DW_ADJ"
fitByGroup <- function(data, group, funName, lhs) {
  
  data <- as.data.frame(data)
  
  # split by group
  dataS <- split(data, data[,group])
  
  # what var
  eqs <- eq_guide[,c("SPCD", lhs)]
  
  if (group == 'SPCD') {
    
    eqs <- eqs[eqs$SPCD %in% names(dataS),]
    dataS <- dataS[eqs$SPCD]
    
  } else if (group == 'SPCD_DIVISION') {
    
    spcd_division <- data.frame(tstrsplit(names(dataS), " "))
    names(spcd_division) <- c("SPCD", "DIVISION")
    
    spcd_division <- merge(x= spcd_division,
                           y= eqs)
    
    spcd_division$SPCD_DIVISION <- names(dataS)
    
    dataS <- dataS[paste(spcd_division$SPCD,
                         spcd_division$DIVISION)]
   
    eqs <- spcd_division
     
  } else {
    
    eqs <- data.frame(GENUS= names(dataS),
                      stringsAsFactors= FALSE)
    
    eqs[,lhs] <- ifelse(lhs %in% c("RCUMIB", "RCUMOB"),
                        4, 1)
    
  }
  
  if (group == 'JENKINS_SPGRPCD' &
      lhs %in% c("TT_WDBK_DW_ADJ", "BRT_WDBK_DW_TOT")) {
    
    eqs[,lhs] <- 1.1
    
  }
  
  # get the function form and starting coefs
  form_list <- eq_forms[[2]]
  names(form_list) <- eq_forms[[1]]
  
  start_list <- eq_forms[[3]]
  names(start_list) <- eq_forms[[1]]
  
  forms  <- form_list[as.character(eqs[,lhs])]
  starts <- start_list[as.character(eqs[,lhs])]
  
  # construct formula
  forms <- lapply(forms, function(x) as.formula(paste(lhs, x)))
  
  fun <- match.fun(funName)
  
  if (is.null(start)) {
    
    lapply(FUN= fun, dataS, formula= formula)
    
  } else {
    
    #fun(data = dataS[[1]], forms[[1]], starts[[1]])
    
    Map(f= fun, data= dataS, formula= forms, start= starts)
    #lapply(FUN= fun, dataS, formula= formula, start= start)
    
  }
  
}

# list= all_total_mods[1]
# group= "SPCD"

getAllCoefs <- function(list, group) {
  
  # mod= list[[1]]
  # grp= group
  getThem <- function(mod, grp) {
    
    out <- rbindlist(lapply(mod, getCoefs), use.names = TRUE, fill= TRUE)
    out[,grp] <- names(mod)
    row.names(out) <- NULL
    return(out)
    
  }
  
  out <- rbindlist(Map(getThem, list, group), fill= TRUE, use.names= TRUE)
  
  return(as.data.frame(out))
  
}

replicate <- function(expr, n) {
  base::replicate(n, expr, simplify= FALSE)
}

sumBy <- function(data, group, N_MIN) {
  
  keepNames <- names(data)
  
  data$group <- data[,..group]
  
  data_sum <- data[,.(N= .N), "group"]
  data_sum <- data_sum[data_sum$N >= N_MIN, ]
  
  data_out <- data[data$group %in% data_sum$group, ]
  
  data_out[,..keepNames]
  
}

myMerge <- function(x, y) {
  
  cat("merging on", paste(intersect(names(x), names(y)), collapse= ", "), "\n")
  
  merge(x= x, y= y, by= intersect(names(x), names(y)))
  
}
#
# total biomass----------------------------------------------------------------
cat("fitting total biomass models\n")

total <- tree[tree$DO_BH >= 1.0 & !is.na(tree$TT_WDBK_DW_ADJ),
              c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
                "TT_WDBK_DW_ADJ")]
names(total) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "TT_WDBK_DW_ADJ")

total <- merge(x= total,
               y= ref,
               by= "SPCD")
total$weights <- 1 / total$DBH^2

# need data by spcd, then genus, then jenkins group
total_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= total, N_MIN= 50)

# fit by each level
all_total_mods <- Map(fitByGroup,
                      data= total_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                      lhs= replicate("TT_WDBK_DW_ADJ", 3))


# group assigned as a row name, make variable
total_biomass_coefs <- getAllCoefs(list= all_total_mods,
                                   group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# branch biomass---------------------------------------------------------------
cat("fitting branch biomass models\n")

branch <- tree[tree$DO_BH >= 1.0 & !is.na(tree$BRT_WDBK_DW_TOT),
               c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                 "k", "BRT_WDBK_DW_TOT")]

names(branch) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT",
                   "k", "BRT_WDBK_DW_TOT")

branch <- merge(x= branch,
                y= ref,
                by= "SPCD")
branch$weights <- 1 / branch$DBH^2

branch_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                    sumBy,
                    data= branch, N_MIN= 50)

all_branch_mods <- Map(fitByGroup,
                       data= branch_by,
                       group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                       funName= replicate("fitModel", 3),
                       lhs= replicate("BRT_WDBK_DW_TOT", 3))

# group assigned as a row name, make variable
branch_biomass_coefs <- getAllCoefs(list= all_branch_mods,
                                    group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# total stem bark weight-------------------------------------------------------
cat("fitting total stem bark weight models\n")

bkwt <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_BK_DW_TOT),
              c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
                "ST_BK_DW_TOT")]
names(bkwt) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "ST_BK_DW_TOT")

bkwt <- merge(x= bkwt,
               y= ref,
               by= "SPCD")
bkwt$weights <- 1 / bkwt$DBH^2

# need data by spcd, then genus, then jenkins group
bkwt_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= bkwt, N_MIN= 50)

# fit by each level
all_bkwt_mods <- Map(fitByGroup,
                      data= bkwt_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                     lhs= replicate("ST_BK_DW_TOT", 3))

# group assigned as a row name, make variable
bark_biomass_coefs <- getAllCoefs(list= all_bkwt_mods,
                                   group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# total wood volume------------------------------------------------------------
cat("fitting total wood volume models\n")

volib <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_WD_CV_TOT),
              c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
                "ST_WD_CV_TOT")]

names(volib) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "ST_WD_CV_TOT")

volib <- merge(x= volib,
               y= ref,
               by= "SPCD")
volib$weights <- 1 / volib$DBH^2

volib_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= volib, N_MIN= 80)

all_volib_mods <- Map(fitByGroup,
                      data= volib_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                      lhs= replicate("ST_WD_CV_TOT", 3))


# group assigned as a row name, make variable
volib_coefs <- getAllCoefs(list= all_volib_mods,
                           group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# total bark volume------------------------------------------------------------
cat("fitting total bark volume models\n")

volbk <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_BK_CV_TOT),
              c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
                "ST_BK_CV_TOT")]

names(volbk) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "ST_BK_CV_TOT")

volbk <- merge(x= volbk,
               y= ref,
               by= "SPCD")
volbk$weights <- 1 / volbk$DBH^2

volbk_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= volbk, N_MIN= 80)

all_volbk_mods <- Map(fitByGroup,
                      data= volbk_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                      lhs= replicate("ST_BK_CV_TOT", 3))

# group assigned as a row name, make variable
volbk_coefs <- getAllCoefs(list= all_volbk_mods,
                           group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# total stem volume------------------------------------------------------------
cat("fitting total stem volume models\n")

volob <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_WDBK_CV_TOT),
              c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
                "ST_WDBK_CV_TOT")]

names(volob) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "ST_WDBK_CV_TOT")

volob <- merge(x= volob,
               y= ref,
               by= "SPCD")
volob$weights <- 1 / volob$DBH^2

volob_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= volob, N_MIN= 80)

all_volob_mods <- Map(fitByGroup,
                      data= volob_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                      lhs= replicate("ST_WDBK_CV_TOT", 3))

# group assigned as a row name, make variable
volob_coefs <- getAllCoefs(list= all_volob_mods,
                           group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# volume ratio inside bark-----------------------------------------------------
cat("fitting volume ratio outside bark models\n")

rcumib <- merge(x= stem[,c("AUTHOR", "LOC", "SPCD", "TREENO", "ST_HT", "ST_WD_CV_CUM")],
                y= volib,
                by= c("AUTHOR", "LOC", "SPCD", "TREENO"))
rcumib$weights <- NULL

names(rcumib) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "HTAG", "VCUMIB", "SPCD_DIVISION",
                  "DBH", "THT", "k", "VOLIB", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

rcumib$VCUMIB <- round(rcumib$VCUMIB, 3)
rcumib$VOLIB <- round(rcumib$VOLIB, 3)
rcumib$X <- rcumib$HTAG / rcumib$THT
rcumib$RCUMIB <- rcumib$VCUMIB / rcumib$VOLIB

# can't have 1 ratios
rcumib <- rcumib[rcumib$X < 1, ]
rcumib$weights <- 1 / (rcumib$X * (1 - rcumib$X))
rcumib[is.infinite(rcumib$weights), "weights"] <- 75

# have to subset by tree, not profile
rcumib_tree <- unique(rcumib[,c("AUTHOR", "LOC", "SPCD", "TREENO", "SPCD_DIVISION", "JENKINS_SPGRPCD")])

rcumib_tree_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                         sumBy,
                         data= rcumib_tree, N_MIN= 80)

rcumib_by <- lapply(rcumib_tree_by, myMerge, x= rcumib)

all_rcumib_mods <- Map(fitByGroup,
                       data= rcumib_by,
                       group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                       funName= replicate("fitModel", 3),
                       lhs= replicate("RCUMIB", 3))

# group assigned as a row name, make variable
rcumib_coefs <- getAllCoefs(list= all_rcumib_mods,
                           group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))

#
# volume ratio outside bark----------------------------------------------------
cat("fitting volume ratio outside bark models\n")

rcumob <- merge(x= stem[,c("AUTHOR", "LOC", "SPCD", "TREENO", "ST_HT", "ST_WDBK_CV_CUM")],
                y= volob,
                by= c("AUTHOR", "LOC", "SPCD", "TREENO"))
rcumob$weights <- NULL

names(rcumob) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "HTAG", "VCUMOB", "SPCD_DIVISION",
                   "DBH", "THT","k", "VOLOB", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

rcumob$VCUMOB <- round(rcumob$VCUMOB, 3)
rcumob$VOLIB <- round(rcumob$VOLOB, 3)
rcumob$X <- rcumob$HTAG / rcumob$THT
rcumob$RCUMOB <- rcumob$VCUMOB / rcumob$VOLOB

# can't have 1 ratios
rcumob <- rcumob[rcumob$X < 1, ]

rcumob$weights <- 1 / (rcumob$X * (1 - rcumob$X))
rcumob[is.infinite(rcumob$weights), "weights"] <- 75

# have to subset by tree, not profile
rcumob_tree <- unique(rcumob[,c("AUTHOR", "LOC", "SPCD", "TREENO", "SPCD_DIVISION", "JENKINS_SPGRPCD")])

rcumob_tree_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                         sumBy,
                         data= rcumob_tree, N_MIN= 80)

rcumob_by <- lapply(rcumob_tree_by, myMerge, x= rcumob)

all_rcumob_mods <- Map(fitByGroup,
                       data= rcumob_by,
                       group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                       funName= replicate("fitModel", 3),
                       lhs= replicate("RCUMOB", 3))

# group assigned as a row name, make variable
rcumob_coefs <- getAllCoefs(list= all_rcumob_mods,
                            group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
# foliage biomass--------------------------------------------------------------
cat("fitting foliage biomass models\n")

fol <- tree[tree$DO_BH >= 1.0 & !is.na(tree$FOL_DW),
            c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DO_BH", "HT_TOT","k",
              "FOL_DW")]
names(fol) <- c("AUTHOR", "LOC", "SPCD_DIVISION", "SPCD", "TREENO", "DBH", "THT","k",
                  "FOL_DW")

fol <- merge(x= fol,
               y= ref,
               by= "SPCD")
fol$weights <- 1 / fol$DBH^2

# need data by spcd, then genus, then jenkins group
fol_by <- lapply(c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                 sumBy,
                 data= fol, N_MIN= 50)

# fit by each level
all_fol_mods <- Map(fitByGroup,
                      data= fol_by,
                      group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"),
                      funName= replicate("fitModel", 3),
                    lhs= replicate("FOL_DW", 3))

# group assigned as a row name, make variable
foliage_coefs <- getAllCoefs(list= all_fol_mods,
                         group= c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD"))
#
#
# end work---------------------------------------------------------------------
coef_list <- ls(pattern= "_coefs")

writeIt <- function(name) {
  
  obj <- get(name)
  
  # only want spcd_division and spcd
  # june 24 - group level model now w/ wdsg
  # obj <- obj[!is.na(obj$JENKINS_SPGRPCD), ]
  obj$GENUS <- NULL
  
  fn <- paste0("Coefs/variable_model_forms/", name, ".csv")
  
  cat("saving", paste0(name, ".csv"), "\n")
  
  write.csv(obj, fn, row.names= FALSE)
  
  
}

lapply(coef_list, writeIt)

# save the model objects
if (as.logical(Sys.info()["user"] == 'walkedm')) {
  
  mod_list <- ls(pattern= "_mods")
  
  getMod <- function(name) {
    
    obj <- get(name)
    # just want division models
    obj <- obj[[1]]
    
    fn <- paste0("~/Documents/Work/R_Data/CRM2/Division_Models/",
                 name,
                 "_division.Rdata")
    
    cat("saving", paste0(name, "_division.Rdata"), "\n")
    
    saveRDS(object= obj, file= fn)
    
  }
  
  lapply(mod_list, getMod)
  
}
