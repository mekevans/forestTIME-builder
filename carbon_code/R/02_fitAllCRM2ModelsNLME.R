
# library(data.table)
# library(minpack.lm)
# library(nlme)

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

#
# helper funs------------------------------------------------------------------
fitNLME <- function(data, formula, start, random) {
  
  for (ll in 1:100) {
    
    cat("attempt", ll, "for group", unique(data$JENKINS_SPGRPCD), "\n")
    
    fit <- NULL
    
    # attempts to fit a nonlinear model and catches the error if it fails
    fit <- try(nlme(model= as.formula(formula),
                    data= data,
                    fixed= a+b+c~1,
                    # random = a+b~1|SPCD2/ECO_DIVISION,     #mod2 40 seconds
                    # random = list(SPCD2=a~1,ECO_DIVISION=b~1),         #mod3 54 seconds
                    random = list(SPCD= b~1),         #mod3
                    start= start,
                    weights = varPower(form=~DBH|SPCD),      # Want greater weight on larger dob, lower height, but not necessarily larger dbh?
                    verbose = F, 
                    control=nlmeControl(msMaxIter = 200,
                                        maxIter=100,
                                        minScale = 10e-20,
                                        tol=1)))
    
    # if no error break out of the loop
    if (any(class(fit) != "try-error")) {
      
      # print("success")
      
      break
      
    } else {
      
      if(ll %% 2 == 1) {
        
        start= start + (runif(1))
        
      } else {
        
        start= start - (runif(1))
        
      }
      
      
    }
    
  }
  
  return(fit)
  
}

fitNLME2 <- function(data, formula, start) {
  
  for (ll in 1:100) {
    
    cat("attempt", ll, "for group", unique(data$JENKINS_SPGRPCD), "\n")
    
    fit <- NULL
    
    # attempts to fit a nonlinear model and catches the error if it fails
    fit <- try(nlme(model= as.formula(formula),
                    data= data,
                    fixed= alpha+beta~1,
                    random = list(SPCD= alpha~1),
                    start= start,
                    weights = varPower(form= ~X|SPCD),
                    verbose = F, 
                    control=nlmeControl(msMaxIter = 200,
                                        maxIter=100,
                                        minScale = 10e-20,
                                        tol=1)))
    
    # if no error break out of the loop
    if (any(class(fit) != "try-error")) {
      
      # print("success")
      
      break
      
    } else {
      
      if(ll %% 2 == 1) {
        
        start= start + (runif(1))
        
      } else {
        
        start= start - (runif(1))
        
      }
      
      
    }
    
  }
  
  return(fit)
  
}

fitNLS <- function(data, formula, start) {
  
  
  for (ll in 1:100) {
    
  cat("attempt", ll, "for group", unique(data$SPCD), "\n")
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

# data= rcumib_by[[2]]
# group= "SPCD"
# funName= "fitNLME2"
# formula= "RCUMIB ~ (1 - (1 - X)^alpha)^beta"

fitByGroup <- function(data, group, funName, formula) {
  
  data <- as.data.frame(data)
  
  # split by group
  dataS <- split(data, data[,group])
  
  fun <- match.fun(funName)
  
  # get overall starting values
  if (any(grepl("alpha", formula))) {
  
    start <- coef(nlsLM(data= data,
                        formula= formula,
                        start= c(alpha= 1.898364, beta= 0.849592)))
    
  } else {
    
    start <- coef(nlsLM(data= data,
                        formula= formula,
                        start= c(a= 1, b= 2, c= 1)))
    
  }
  
  
  lapply(dataS,
         FUN= fun,
         start= start,
         formula= formula)
  
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

getAllCoef <- function(object, groups) {
  
  getCoef <- function(obj) {
    
    r <- coef(obj)
    
    if (any(class(obj) == 'nlme')) {
      
      r2 <- obj$coefficients$fixed
      r <- rbind(r, r2)
      r <- as.data.frame(r)
      row.names(r)[nrow(r)] <- ""
      
    }
    
    return(r)
    
  }
  
  bindCoef <- function(mods) {
    
    out <- do.call(rbind, lapply(mods, getCoef))
    out <- as.data.frame(out)
    out$label <- row.names(out)
    out$label <- ifelse(nchar(out$label) < 4,
                        gsub("\\.", "", out$label),
                        gsub("[[:digit:]]\\.", "", out$label))
    
    return(out)
  }
  
  all_coef <- lapply(object, bindCoef)
  names(all_coef) <- groups
  
  dropDupes <- function(x, y) {
    
    x <- x[!(x$label %in% y$label),]
    
    rbind(y, x)
    
  }
  
  out <- Reduce(dropDupes, rev(all_coef))
  out[,groups[1]] <- ifelse(nchar(out$label) < 2,
                            NA,
                            out$label)
  out[,groups[2]] <- ifelse(nchar(out$label) < 2,
                            out$label,
                            NA)
  out$label <- NULL
  
  out <- out[order(as.numeric(out[,groups[1]])),]
  
  row.names(out) <- NULL
  
  return(out)
  
}

subsetCoefs <- function(data, coefs, N_SPCD) {
  
  spcd_list <- data[,.(N= .N), "SPCD"][N > 5 & N < N_SPCD][order(as.numeric(SPCD))]
  
  coefs[(coefs$SPCD %in% spcd_list$SPCD) |
          !is.na(coefs$JENKINS_SPGRPCD), ]
  
}

#
# total biomass----------------------------------------------------------------
cat("fitting total biomass models\n")

total <- tree[tree$DO_BH >= 1.0 & !is.na(tree$TT_WDBK_DW_ADJ),
              c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                "TT_WDBK_DW_ADJ")]
names(total) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                  "TT_WDBK_DW_ADJ")

total <- merge(x= total,
               y= ref,
               by= "SPCD")
total$weights <- 1 / total$DBH^2

# need data by spcd then jenkins group
total_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= total, N_MIN= 50)

# fit by each level
all_total_mods <- Map(fitByGroup,
                      data= total_by,
                      group= c("SPCD", "JENKINS_SPGRPCD"),
                      funName= c("fitNLS", "fitNLME"),
                      formula= replicate("TT_WDBK_DW_ADJ ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
total_biomass_coefs <- getAllCoef(object= all_total_mods,
                                  groups= c("SPCD", "JENKINS_SPGRPCD"))

# only want species-level effects if sample sie between 5 and 50 (80 for vol)
total_biomass_coefs <- subsetCoefs(total, total_biomass_coefs, 50)

#
# branch biomass---------------------------------------------------------------
cat("fitting branch biomass models\n")

branch <- tree[tree$DO_BH >= 1.0 & !is.na(tree$BRT_WDBK_DW_TOT),
               c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                 "BRT_WDBK_DW_TOT")]

names(branch) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                   "BRT_WDBK_DW_TOT")

branch <- merge(x= branch,
               y= ref,
               by= "SPCD")
branch$weights <- 1 / branch$DBH^2

# need data by spcd then jenkins group
branch_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= branch, N_MIN= 50)

# fit by each level
all_branch_mods <- Map(fitByGroup,
                      data= branch_by,
                      group= c("SPCD", "JENKINS_SPGRPCD"),
                      funName= c("fitNLS", "fitNLME"),
                      formula= replicate("BRT_WDBK_DW_TOT ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
branch_biomass_coefs <- getAllCoef(object= all_branch_mods,
                                  groups= c("SPCD", "JENKINS_SPGRPCD"))

# only want species-level effects if sample sie between 5 and 50 (80 for vol)
branch_biomass_coefs <- subsetCoefs(branch, branch_biomass_coefs, 50)

#
# total stem bark weight-------------------------------------------------------
cat("fitting total stem bark weight models\n")

bkwt <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_BK_DW_TOT),
             c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
               "ST_BK_DW_TOT")]
names(bkwt) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                 "ST_BK_DW_TOT")

bkwt <- merge(x= bkwt,
              y= ref,
              by= "SPCD")
bkwt$weights <- 1 / bkwt$DBH^2

# need data by spcd then jenkins group
bkwt_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                    sumBy,
                    data= bkwt, N_MIN= 50)

# fit by each level
all_bkwt_mods <- Map(fitByGroup,
                       data= bkwt_by,
                       group= c("SPCD", "JENKINS_SPGRPCD"),
                       funName= c("fitNLS", "fitNLME"),
                       formula= replicate("ST_BK_DW_TOT ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
bark_biomass_coefs <- getAllCoef(object= all_bkwt_mods,
                                   groups= c("SPCD", "JENKINS_SPGRPCD"))

bark_biomass_coefs <- subsetCoefs(bkwt, bark_biomass_coefs, 50)

#
# total wood volume------------------------------------------------------------
cat("fitting total wood volume models\n")

volib <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_WD_CV_TOT),
              c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                "ST_WD_CV_TOT")]

names(volib) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                  "ST_WD_CV_TOT")

volib <- merge(x= volib,
               y= ref,
               by= "SPCD")
volib$weights <- 1 / volib$DBH^2

# need data by spcd then jenkins group
volib_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                  sumBy,
                  data= volib, N_MIN= 80)

# fit by each level
all_volib_mods <- Map(fitByGroup,
                     data= volib_by,
                     group= c("SPCD", "JENKINS_SPGRPCD"),
                     funName= c("fitNLS", "fitNLME"),
                     formula= replicate("ST_WD_CV_TOT ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
volib_coefs <- getAllCoef(object= all_volib_mods,
                                 groups= c("SPCD", "JENKINS_SPGRPCD"))

volib_coefs <- subsetCoefs(volib, volib_coefs, 80)

#
# total bark volume------------------------------------------------------------
cat("fitting total bark volume models\n")

volbk <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_BK_CV_TOT),
              c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                "ST_BK_CV_TOT")]

names(volbk) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                  "ST_BK_CV_TOT")

volbk <- merge(x= volbk,
               y= ref,
               by= "SPCD")
volbk$weights <- 1 / volbk$DBH^2

# need data by spcd then jenkins group
volbk_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= volbk, N_MIN= 80)

# fit by each level
all_volbk_mods <- Map(fitByGroup,
                      data= volbk_by,
                      group= c("SPCD", "JENKINS_SPGRPCD"),
                      funName= c("fitNLS", "fitNLME"),
                      formula= replicate("ST_BK_CV_TOT ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
volbk_coefs <- getAllCoef(object= all_volbk_mods,
                          groups= c("SPCD", "JENKINS_SPGRPCD"))

volbk_coefs <- subsetCoefs(volbk, volbk_coefs, 80)

#
#
# total stem volume------------------------------------------------------------
cat("fitting total stem volume models\n")

volob <- tree[tree$DO_BH >= 1.0 & !is.na(tree$ST_WDBK_CV_TOT),
              c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
                "ST_WDBK_CV_TOT")]

names(volob) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                  "ST_WDBK_CV_TOT")

volob <- merge(x= volob,
               y= ref,
               by= "SPCD")
volob$weights <- 1 / volob$DBH^2

# need data by spcd then jenkins group
volob_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                   sumBy,
                   data= volob, N_MIN= 80)

# fit by each level
all_volob_mods <- Map(fitByGroup,
                      data= volob_by,
                      group= c("SPCD", "JENKINS_SPGRPCD"),
                      funName= c("fitNLS", "fitNLME"),
                      formula= replicate("ST_WDBK_CV_TOT ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
volob_coefs <- getAllCoef(object= all_volob_mods,
                          groups= c("SPCD", "JENKINS_SPGRPCD"))

volob_coefs <- subsetCoefs(volob, volob_coefs, 80)

#

# volume ratio inside bark-----------------------------------------------------
cat("fitting volume ratio inside bark models\n")

rcumib <- merge(x= stem[,c("AUTHOR", "LOC", "SPCD", "TREENO", "ST_HT", "ST_WD_CV_CUM")],
                y= volib,
                by= c("AUTHOR", "LOC", "SPCD", "TREENO"))
rcumib$weights <- NULL

names(rcumib) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "HTAG", "VCUMIB",
                   "DBH", "THT", "VOLIB", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

rcumib$VCUMIB <- round(rcumib$VCUMIB, 3)
rcumib$VOLIB <- round(rcumib$VOLIB, 3)
rcumib$X <- rcumib$HTAG / rcumib$THT
rcumib$RCUMIB <- rcumib$VCUMIB / rcumib$VOLIB

# can't have 1 ratios
rcumib <- rcumib[rcumib$X < 1, ]
rcumib <- rcumib[rcumib$X > 0, ]
rcumib$weights <- 1 / (rcumib$X * (1 - rcumib$X))
rcumib[is.infinite(rcumib$weights), "weights"] <- 75

# have to subset by tree, not profile
rcumib_tree <- unique(rcumib[,c("AUTHOR", "LOC", "SPCD", "TREENO", "GENUS", "JENKINS_SPGRPCD")])

rcumib_tree_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                         sumBy,
                         data= rcumib_tree, N_MIN= 80)

rcumib_by <- lapply(rcumib_tree_by, myMerge, x= rcumib)

all_rcumib_mods <- Map(fitByGroup,
                       data= rcumib_by,
                       group= c("SPCD", "JENKINS_SPGRPCD"),
                       funName= c("fitNLS", "fitNLME2"),
                       formula= replicate("RCUMIB ~ (1 - (1 - X)^alpha)^beta", 2))

# group assigned as a row name, make variable
rcumib_coefs <- getAllCoef(object= all_rcumib_mods,
                           groups= c("SPCD", "JENKINS_SPGRPCD"))

rcumib_coefs <- subsetCoefs(rcumib_tree, rcumib_coefs, 80)

#
# volume ratio outside bark----------------------------------------------------
cat("fitting volume ratio outside bark models\n")

rcumob <- merge(x= stem[,c("AUTHOR", "LOC", "SPCD", "TREENO", "ST_HT", "ST_WDBK_CV_CUM")],
                y= volob,
                by= c("AUTHOR", "LOC", "SPCD", "TREENO"))
rcumob$weights <- NULL

names(rcumob) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "HTAG", "VCUMOB",
                   "DBH", "THT", "VOLOB", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

rcumob$VCUMOB <- round(rcumob$VCUMOB, 3)
rcumob$VOLIB <- round(rcumob$VOLOB, 3)
rcumob$X <- rcumob$HTAG / rcumob$THT
rcumob$RCUMOB <- rcumob$VCUMOB / rcumob$VOLOB

# can't have 1 ratios
rcumob <- rcumob[rcumob$X < 1, ]
rcumob <- rcumob[rcumob$X > 0, ]
rcumob$weights <- 1 / (rcumob$X * (1 - rcumob$X))
rcumob[is.infinite(rcumob$weights), "weights"] <- 75

# have to subset by tree, not profile
rcumob_tree <- unique(rcumob[,c("AUTHOR", "LOC", "SPCD", "TREENO", "GENUS", "JENKINS_SPGRPCD")])

rcumob_tree_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                         sumBy,
                         data= rcumob_tree, N_MIN= 80)

rcumob_by <- lapply(rcumob_tree_by, myMerge, x= rcumob)

all_rcumob_mods <- Map(fitByGroup,
                       data= rcumob_by,
                       group= c("SPCD", "JENKINS_SPGRPCD"),
                       funName= c("fitNLS", "fitNLME2"),
                       formula= replicate("RCUMOB ~ (1 - (1 - X)^alpha)^beta", 2))

# group assigned as a row name, make variable
rcumob_coefs <- getAllCoef(object= all_rcumob_mods,
                           groups= c("SPCD", "JENKINS_SPGRPCD"))

rcumob_coefs <- subsetCoefs(rcumob_tree, rcumob_coefs, 80)

#
# foliage biomass--------------------------------------------------------------
cat("fitting foliage biomass models\n")

fol <- tree[tree$DO_BH >= 1.0 & !is.na(tree$FOL_DW),
            c("AUTHOR", "LOC", "SPCD", "TREENO", "DO_BH", "HT_TOT",
              "FOL_DW")]
names(fol) <- c("AUTHOR", "LOC", "SPCD", "TREENO", "DBH", "THT",
                "FOL_DW")

fol <- merge(x= fol,
             y= ref,
             by= "SPCD")
fol$weights <- 1 / fol$DBH^2

# need data by spcd, then genus, then jenkins group
fol_by <- lapply(c("SPCD", "JENKINS_SPGRPCD"),
                 sumBy,
                 data= fol, N_MIN= 50)

# fit by each level
all_fol_mods <- Map(fitByGroup,
                    data= fol_by,
                    group= c("SPCD", "JENKINS_SPGRPCD"),
                    funName= c("fitNLS", "fitNLME"),
                    formula= replicate("FOL_DW ~ a * DBH^b * THT^c", 2))

# group assigned as a row name, make variable
foliage_coefs <- getAllCoef(object= all_fol_mods,
                            groups= c("SPCD", "JENKINS_SPGRPCD"))

foliage_coefs <- subsetCoefs(fol, foliage_coefs, 50)

#
#
# end work---------------------------------------------------------------------
coef_list <- ls(pattern= "_coefs")

writeIt <- function(name) {
  
  obj <- get(name)
  
  fn <- paste0("Coefs/nlme/", name, ".csv")
  
  cat("saving", paste0(name, ".csv"), "\n")
  
  write.csv(obj, fn, row.names= FALSE)
  
  
}

s <- lapply(coef_list, writeIt)
