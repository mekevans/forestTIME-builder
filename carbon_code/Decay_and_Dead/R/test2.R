library(data.table)
library(RPostgreSQL)

# drv <- DBI::dbDriver("PostgreSQL")
# 
# if (!exists("con")) con <- dbConnect(drv, dbname= "fiadb")
# 
# query <- "
# SELECT
# tree.cn as tre_cn,
# cond.cond_status_cd
# FROM
# fs_fiadb.tree
# JOIN fs_fiadb.cond USING (plt_cn, condid)
# WHERE
# cond.cond_status_cd = 1
# "
# 
# cond_status <- dbGetQuery(con, query)
# 
# write.csv(cond_status, "trees_on_forested_conditions.csv", row.names= FALSE)

setwd("~/Documents/")

cond_status <- read.csv("trees_on_forested_conditions.csv")

tech_transfer_path <- "~/Documents/nsbe_tech_transfer/"

name_trans <- read.csv("tech_transfer/name_translate.csv")

file_list <- list.files("tech_transfer/From_FIA/2022_03_30/", pattern= "_03_30.csv",
                        full.names = TRUE)

# file_list <- file_list[1]

fia <- lapply(file_list, fread, data.table= FALSE)
fia <- do.call(rbind, fia)

# fia <- fread("tech_transfer/AR_2022_03_25.csv",
#              data.table= FALSE)
fia$TRE_CN <- as.character(fia$TRE_CN)
fia$PLT_CN <- as.character(fia$PLT_CN)

# restrict to trees on forested conditions only
fia <- merge(x= fia,
             y= cond_status,
             by.x= "TRE_CN",
             by.y= "tre_cn")

# these are actually mean
med_cr_prop <- read.csv("tech_transfer/median_crprop.csv")
med_cr_prop$SFTWD_HRDWD <- ifelse(med_cr_prop$hwd_yn == 'N', 'S', 'H')

# total stem comps
fia$DRYBIO_STEM_TOP <- (fia$DRYBIO_TOP - fia$DRYBIO_TOP_BARK - fia$DRYBIO_BRANCH) +
                        fia$DRYBIO_TOP_BARK
fia$DRYBIO_STEM_TOP_BARK <- fia$DRYBIO_TOP_BARK

fia$DRYBIO_STEM_BARK <- fia$DRYBIO_STUMP_BARK + fia$DRYBIO_BOLE_BARK + fia$DRYBIO_STEM_TOP_BARK
fia$DRYBIO_STEM <- (fia$DRYBIO_STUMP) + (fia$DRYBIO_BOLE) + (fia$DRYBIO_STEM_TOP)


# look at arkansas first
# fia <- fia[fia$STATECD == 5,]
# fia <- fia[!(fia$ECODIV %in% c("", " 20")), ]

# plot(fia$DRYBIO_STEM + fia$DRYBIO_BRANCH + fia$DRYBIO_STEM_BARK,
#      fia$DRYBIO_AG); abline(0,1)

# get rid of any dead trees that are not standing dead
fia <- fia[fia$STATUSCD %in% 1 | fia$STANDING_DEAD_CD %in% 1, ]

# must have dbh and tht
fia <- fia[!is.na(fia$DIA) & !is.na(fia$HT), ]

# must have a spcd
# fia[is.na(fia$SPN),]

decay_codes <- read.csv(file.path("tech_transfer/decay_codes.csv"),
                        as.is= TRUE)

fia[fia$STATUSCD == 1,]$DECAYCD <- 0
fia[fia$STATUSCD == 1,]$STANDING_DEAD_CD <- 0
fia[is.na(fia$ACTUALHT), 'ACTUALHT'] <- fia[is.na(fia$ACTUALHT), 'HT']

fia <- merge(x= fia,
             y= decay_codes,
             by.x= c("SFTWD_HRDWD", "DECAYCD"),
             by.y= c("spgrp", "decaycd"))

# flist <- list.files("~/Box/External FIA CRM 2.0/FS_FIA_FICS/PNWRS",
#                     full.names = TRUE)
#
# fia <- rbindlist(lapply(flist, fread))

# unique(fia$ECODIV)

# fia[fia$ECODIV == 'Wa0',]$ECOSUBCD

# add cull density reduction factors
decay_ratio_hw <- decay_codes[decay_codes$spgrp == 'H' &
                                decay_codes$decaycd == 3,]$wood_decay
decay_ratio_sw <- decay_codes[decay_codes$spgrp == 'S' &
                                decay_codes$decaycd == 3,]$wood_decay

fia$CULL_DECAY_RATIO <- 1
fia[fia$STATUSCD == 1 & fia$SFTWD_HRDWD == 'H', "CULL_DECAY_RATIO"] <- decay_ratio_hw
fia[fia$STATUSCD == 1 & fia$SFTWD_HRDWD == 'S', "CULL_DECAY_RATIO"] <- decay_ratio_sw

# just do arkansas right now
# fia <- fia[fia$STATECD == 5,]

# sound no broken tops
# fia <- fia[fia$CULL_FLD == 0 & !is.na(fia$CULL_FLD),]
# fia <- fia[fia$HT == fia$ACTUALHT & !is.na(fia$HT) & !is.na(fia$ACTUALHT),]
# fia <- fia[fia$STATUSCD == 1,]

source("nsbe_tech_transfer/VTECO/R/VT_CRM2_ApplyFuns.R")

# live trees with no broken tops for now
# fia <- fia[fia$STATUSCD == 1,]
# fia <- fia[fia$HT == fia$ACTUALHT, ]
# lets do all live and standing dead

fia <- merge(x= fia,
             y= med_cr_prop[,c("Province", "SFTWD_HRDWD", "CRmn")],
             by.x= c("ECOPROV", "SFTWD_HRDWD"),
             by.y= c("Province", "SFTWD_HRDWD"),
             all.x= TRUE)

miss_sft <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED',]$CRmn[1]
miss_hwd <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED',]$CRmn[2]

fia[is.na(fia$CRmn) & fia$SFTWD_HRDWD == 'S','CRmn'] <- miss_sft
fia[is.na(fia$CRmn) & fia$SFTWD_HRDWD == 'H','CRmn'] <- miss_hwd

fia$BROKEN_TOP <- !(fia$HT == fia$ACTUALHT)

fia1 <- fia[,c('TRE_CN', 'DIA', 'HT', 'SPN', 'STDORGCD', 'STANDING_DEAD_CD',
               'ACTUALHT', 'BROKEN_TOP',
               'ECODIV', 'JENKINS_SPGRPCD', 'WOOD_SPGR_GREENVOL_DRYWT',
               'CULL_FLD',
               'CR', 'DECAYCD', 'CRmn',
               'wood_decay', 'bark_decay', 'branch_decay',
               'CULL_DECAY_RATIO')]
names(fia1) <- c("TRE_CN", "DBH", "THT", "SPCD", 'STDORGCD', 'STANDING_DEAD_CD',
                 'ACTUALHT', 'BROKEN_TOP',
                 "DIVISION", "JENKINS_SPGRPCD", "WDSG",
                 'CULL',
                 'CR', 'DECAYCD', 'CRmn',
                 'DECAY_WD', 'DECAY_BK', 'DECAY_BR',
                 'CULL_DECAY_RATIO')

# fia1$DIVISION <-
fia1[is.na(fia1$STDORGCD), "STDORGCD"] <- 0
fia1$SPCD <- ifelse(fia1$SPCD %in% c(111, 131) & fia1$STDORGCD == 1,
                    paste0("1_", fia1$SPCD),
                    fia1$SPCD)

fia1[is.na(fia1$CULL),'CULL'] <- 0

unique(fia1$DIVISION)

# fia1$CULL <- ifelse(fia1$CULL < 98, fia1$CULL, 100)

fia1$SPCD_DIVISION <- paste(fia1$SPCD, fia1$DIVISION)

# equation numbers and forms
forms <- read.csv("nsbe_tech_transfer/VTECO/Files/equation_forms_and_calls.csv", as.is= TRUE)
add_me <- data.frame(equation= c(3.1, 6.1),
                     rhs= c("<- a * DBH^b * THT^c * WDSG",
                            "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta"))

forms <- rbind(forms, add_me)

forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)

if (TRUE) {

# edit the function to match fia changes
applyAllLevels <- function(data, level, coefs, lhs) {

  data <- as.data.frame(data)

  # subset to level
  getLevel <- function(x) {

    ccs <-     c("a", "a0", "a1",
                 "alpha", "beta",
                 "b", "b0", "b1",
                 "c", "c1",
                 "b2",
                 "equation")

    ccs <- ccs[ccs %in% names(x)]

    x <- x[!is.na(x[,level]), c(level, ccs)]

    return(x)

  }

  if (class(coefs) == 'list') {

    the_coefs <- lapply(coefs, getLevel)

    the_coefs <- Reduce(myMerge, the_coefs)

  } else {

    the_coefs <- getLevel(coefs)

  }

  data <- merge(x= data,
                y= the_coefs,
                by= intersect(names(data), names(the_coefs)))

  if (nrow(data) == 0) return(NULL)

  if (lhs %in% c("Total", "Branch") &
      any(names(the_coefs) == 'JENKINS_SPGRPCD')) data$equation <- 3.1

  if (lhs == 'HT4') data$equation <- 7
  if (lhs == "VMERIB_GROSS") data$equation <- 8
  if (lhs == "VMEROB_GROSS") data$equation <- 9
  if (lhs == "VSTPIB_GROSS") data$equation <- 10
  if (lhs == "VSTPOB_GROSS") data$equation <- 11
  if (lhs == "HTSAW") data$equation <- 12
  if (lhs == "VSAWIB_GROSS") data$equation <- 13
  if (lhs == "VSAWOB_GROSS") data$equation <- 14
  if (lhs == "HT_RAT_ADJ") data$equation <- 6.1

  # split by equation form, construct formula, and apply
  data_split <- split(data, data$equation)

  # dat= data_split[[1]]
  applyIt <- function(dat) {

    rhs <- forms[forms$equation == unique(dat$equation), "rhs"]

    formula <- paste(lhs, rhs)

    dat2 <- within(dat, eval(parse(text= formula)))

    return(dat2)

  }

  data2 <- do.call(rbind, lapply(data_split, applyIt))

  out_names <- c("ID", lhs)

  data2 <- data2[,out_names]

  #names(data2)[2] <- paste(names(data2)[2], level, sep= "_")

  data2 <- data2[order(data2$ID), ]

  return(data2)

}

fia_check <- fia#[fia$STANDING_DEAD_CD == 1 & fia$BROKEN_TOP,]

data= fia1
coef_dir= "nsbe_tech_transfer/VTECO/Coefs/combined/"
var_names= c(DBH= "DBH", THT= "THT", CULL= "CULL")
all.vars= TRUE
predictCRM2 <- function(data,
                        coef_dir,
                        var_names= c(DBH= "DBH", THT= "THT", CULL= "CULL"),
                        gross.volume= FALSE,
                        all.vars= TRUE) {

  data <- as.data.frame(data)

  keepN <- names(data)

  # add a unique id
  data$ID <- 1:nrow(data)

  data$SPCD_NUMERIC <- as.numeric(gsub("1_", "", data$SPCD))

  # split point
  data$k <- ifelse(data$SPCD_NUMERIC < 300, 9, 11)
  data$saw <- ifelse(data$SPCD_NUMERIC < 300, 7, 9)

  # adjust var names
  data[,names(var_names)] <- data[,var_names]

  # all needed coefs
  coef_files <- list.files(coef_dir, pattern= "_coefs.csv")
  all_coefs <- lapply(coef_files, function(x) read.csv(file.path(coef_dir, x), as.is= TRUE))
  names(all_coefs) <- gsub("_coefs.csv", "", coef_files)

  levels <- c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD")

  # predict all volumes first
  # total wood volume
  cat("predicting total stem wood volume\n")
  all_volib <- lapply(levels, applyAllLevels,
                      data= data,
                      coefs= all_coefs[["volib"]],
                      lhs= 'VTOTIB_GROSS')

  all_volib <- Reduce(combineLevels2, all_volib)
  
  data <- merge(x= data,
                y= all_volib,
                by= "ID")

  # total bark volume
  cat("predicting total stem wood and bark volume\n")
  all_volob <- lapply(levels, applyAllLevels,
                      data= data,
                      coefs= all_coefs[["volbk"]],
                      lhs= 'VTOTBK_GROSS')

  all_volob <- Reduce(combineLevels2, all_volob)

  data <- merge(x= data,
                y= all_volob,
                by= "ID")

  # outside bark volume
  data$VTOTOB_GROSS <- data$VTOTIB_GROSS + data$VTOTBK_GROSS

  # merch height
  cat("finding merchantable height\n")
  ht4 <- lapply(levels, applyAllLevels,
                data= data,
                coefs= list(all_coefs[["rcumob"]], all_coefs[["volob"]]),
                lhs= 'HT4')

  ht4 <- Reduce(combineLevels2, ht4)

  ht4$HT4 <- pmax(ht4$HT4, 5)

  data <- merge(x= data,
                y= ht4,
                by= "ID")

  # merch vol ib
  cat("predicting merchantable stem wood volume\n")
  vmerib <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VMERIB_GROSS')

  vmerib <- Reduce(combineLevels2, vmerib)

  data <- merge(x= data,
                y= vmerib,
                by= "ID")

  # merch vol ob
  # use rcumib coefs to ensure positive bark vol
  cat("predicting merchantable stem wood and bark volume\n")
  vmerob <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VMEROB_GROSS')

  vmerob <- Reduce(combineLevels2, vmerob)

  data <- merge(x= data,
                y= vmerob,
                by= "ID")

  data$VMERBK_GROSS <- data$VMEROB_GROSS - data$VMERIB_GROSS

  # stump vol ib
  cat("predicting stump wood volume\n")
  vstpib <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSTPIB_GROSS')

  vstpib <- Reduce(combineLevels2, vstpib)

  data <- merge(x= data,
                y= vstpib,
                by= "ID")

  # stump vol ob
  cat("predicting stump wood and bark volume\n")
  vstpob <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSTPOB_GROSS')

  vstpob <- Reduce(combineLevels2, vstpob)

  data <- merge(x= data,
                y= vstpob,
                by= "ID")

  # no stump volumes for saplings
  data[data$DIA < 5.0, c("VSTPOB_GROSS", "VSTPIB_GROSS")] <- NA

  data$VSTPBK_GROSS <- data$VSTPOB_GROSS - data$VSTPIB_GROSS

  # tip vol ib
  data$VTOPIB_GROSS <- data$VTOTIB_GROSS - data$VMERIB_GROSS - data$VSTPIB_GROSS

  # tip vol ob
  data$VTOPOB_GROSS <- data$VTOTOB_GROSS - data$VMEROB_GROSS - data$VSTPOB_GROSS
  data$VTOPBK_GROSS <- data$VTOPOB_GROSS - data$VTOPIB_GROSS

  # sawlog vols
  cat("finding sawlog height\n")
  htsaw <- lapply(levels, applyAllLevels,
                  data= data,
                  coefs= list(all_coefs[["rcumob"]], all_coefs[["volob"]]),
                  lhs= 'HTSAW')

  htsaw <- Reduce(combineLevels2, htsaw)

  htsaw$HTSAW <- pmax(htsaw$HTSAW, 5)

  data <- merge(x= data,
                y= htsaw,
                by= "ID")

  # sawtimber vol ib
  cat("predicting sawlog stem wood volume\n")
  vsawib <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSAWIB_GROSS')

  vsawib <- Reduce(combineLevels2, vsawib)

  data <- merge(x= data,
                y= vsawib,
                by= "ID")

  # sawtimber vol ob
  # use rcumib coefs to ensure positive bark vol
  cat("predicting sawlog stem wood and bark volume\n")
  vsawob <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSAWOB_GROSS')

  vsawob <- Reduce(combineLevels2, vsawob)

  data <- merge(x= data,
                y= vsawob,
                by= "ID")

  data$VSAWBK_GROSS <- data$VSAWOB_GROSS - data$VSAWIB_GROSS

  # sawtimber rules: 9" for softwoods, 11" for hardwoods
  # otherwise missing
  data[data$SPCD_NUMERIC < 300 & data$DBH <  9.0 & !is.na(data$DBH),
       c("VSAWIB_GROSS", "VSAWOB_GROSS", "VSAWBK_GROSS")] <- NA
  data[data$SPCD_NUMERIC > 300 & data$DBH < 11.0 & !is.na(data$DBH),
       c("VSAWIB_GROSS", "VSAWOB_GROSS", "VSAWBK_GROSS")] <- NA

  # get height reduction for broken tops
  htadj <- lapply(levels, applyAllLevels,
                  data= data,
                  coefs= all_coefs[["rcumib"]],
                  lhs= 'HT_RAT_ADJ')
  
  htadj <- Reduce(combineLevels2, htadj)
  
  data <- merge(x= data,
                y= htadj,
                by= "ID")
  
  data$HT_RAT_ADJ <- ifelse(data$ACTUALHT < data$THT,
                            data$HT_RAT_ADJ,
                            1)
  
  # apply broken top adjustment to volumes
  # this will be the volume used to determine wood specific gravity
  data$VTOTIB_PRESENT <- data$VTOTIB_GROSS * data$HT_RAT_ADJ
  data$VTOTBK_PRESENT <- data$VTOTBK_GROSS * data$HT_RAT_ADJ
  data$VTOTOB_PRESENT <- data$VTOTIB_PRESENT
  
  data$VMERIB_PRESENT <- ifelse(data$ACTUALHT < data$HT4,
                                (data$VTOTIB_GROSS * data$HT_RAT_ADJ) - data$VSTPIB_GROSS,
                                data$VMERIB_GROSS)
  data$VMERBK_PRESENT <- ifelse(data$ACTUALHT < data$HT4,
                                (data$VTOTBK_GROSS * data$HT_RAT_ADJ) - data$VSTPBK_GROSS,
                                data$VMERBK_GROSS)
  data$VMEROB_PRESENT <- data$VMERIB_PRESENT + data$VMERBK_PRESENT
  
  data$VSAWIB_PRESENT <- ifelse(data$ACTUALHT < data$HTSAW,
                                data$VSAWIB_GROSS * data$HT_RAT_ADJ,
                                data$VSAWIB_GROSS)
  data$VSAWBK_PRESENT <- ifelse(data$ACTUALHT < data$HTSAW,
                                data$VSAWBK_GROSS * data$HT_RAT_ADJ,
                                data$VSAWBK_GROSS)
  data$VSAWOB_PRESENT <- data$VSAWIB_PRESENT + data$VSAWBK_PRESENT
  
  data$VSTPIB_PRESENT <- data$VSTPIB_GROSS
  data$VSTPBK_PRESENT <- data$VSTPBK_GROSS
  data$VSTPOB_PRESENT <- data$VSTPOB_GROSS
  
  data$VTOPIB_PRESENT <- data$VTOTIB_PRESENT - data$VMERIB_PRESENT - data$VSTPIB_PRESENT
  data$VTOPBK_PRESENT <- data$VTOTBK_PRESENT - data$VMERBK_PRESENT - data$VSTPBK_PRESENT
  data$VTOPOB_PRESENT <- data$VTOPIB_PRESENT + data$VTOPBK_PRESENT
  
  # if broken top is below bole height top vols are zero
  data[data$ACTUALHT < data$HT4 & !is.na(data$HT4),
       c("VTOPIB_PRESENT", "VTOPBK_PRESENT", "VTOPOB_PRESENT")] <- 0
  
  # this is the reduction factor to apply to all present volumes
  data[data$CULL >= 98, "CULL"] <- 100
  data$SND_WOOD_DECAY <- ifelse(data$CULL < 98,
                                (1 - data$CULL / 100),
                                0)
  
  pres_vol_list <- c("VTOTIB_PRESENT", "VMERIB_PRESENT",
                      "VSTPIB_PRESENT", "VTOPIB_PRESENT",
                      "VSAWIB_PRESENT")
  
  for (i in pres_vol_list) {
    
    i2 <- gsub("_PRESENT", "_SOUND", i)
    
    data[,i2] <- data[,i] * data$SND_WOOD_DECAY
    
  }
  
  # cull not applied to bark
  data$VTOTBK_SOUND <- data$VTOTBK_PRESENT
  data$VSTPBK_SOUND <- data$VSTPBK_PRESENT
  data$VMERBK_SOUND <- data$VMERBK_PRESENT
  data$VTOPBK_SOUND <- data$VTOPBK_PRESENT
  data$VSAWBK_SOUND <- data$VSAWBK_PRESENT
  
  # get sound ob vols
  data$VTOTOB_SOUND <- data$VTOTIB_SOUND + data$VTOTBK_SOUND
  data$VMEROB_SOUND <- data$VMERIB_SOUND + data$VMERBK_SOUND
  data$VSTPOB_SOUND <- data$VSTPIB_SOUND + data$VSTPBK_SOUND
  data$VTOPOB_SOUND <- data$VTOPIB_SOUND + data$VTOPBK_SOUND
  data$VSAWOB_SOUND <- data$VSAWIB_SOUND + data$VSAWBK_SOUND
  
  # total biomass
  cat("predicting total biomass\n")
  totbio <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["total_biomass"]],
                   lhs= 'Total')

  totbio <- Reduce(combineLevels2, totbio)

  data <- merge(x= data,
                y= totbio,
                by= "ID")
  
  # stem bark weight
  cat("predicting total stem bark weight\n")
  bark_weight <- lapply(levels, applyAllLevels,
                        data= data,
                        coefs= all_coefs[["bark_biomass"]],
                        lhs= 'Bark')
  
  bark_weight <- Reduce(combineLevels2, bark_weight)
  
  data <- merge(x= data,
                y= bark_weight,
                by= "ID")
  
  # branch weight
  cat("predicting total branch weight\n")
  branch_weight <- lapply(levels, applyAllLevels,
                          data= data,
                          coefs= all_coefs[["branch_biomass"]],
                          lhs= 'Branch')
  
  branch_weight <- Reduce(combineLevels2, branch_weight)
  
  data <- merge(x= data,
                y= branch_weight,
                by= "ID")
  
  # foliage weight
  cat("predicting foliage weight\n")
  foliage_weight <- lapply(levels, applyAllLevels,
                           data= data,
                           coefs= all_coefs[["foliage"]],
                           lhs= 'FOLIAGE')
  
  foliage_weight <- Reduce(combineLevels2, foliage_weight)
  
  data <- merge(x= data,
                y= foliage_weight,
                by= "ID")
  
  data$Wood <- data$VTOTIB_GROSS * data$WDSG * 62.4
  
  # need wood weight, bark weight, branch weight w/ and w/o deductions
  # wood deductions: broken top ratio adjust plus cull decay reduction plus decay
  # bark deductions: broken top ratio adjust plus decay
  # branch deductions: branch loss plus decay
  data$CULL_REDUCTION <- ifelse(data$STANDING_DEAD_CD < 1,
                                (1 - (1 - data$CULL_DECAY_RATIO) * (data$CULL)/100),
                                1)
  data$WOOD_REDUCTION <- data$HT_RAT_ADJ * data$CULL_REDUCTION * data$DECAY_WD
  data$BARK_REDUCTION <- data$HT_RAT_ADJ * data$DECAY_BK
  
  # get crown proportion adjustments
  data$CRprop_HT <- (1 - data$ACTUALHT/data$THT * (1 - (data$CR)/100))
  
  # dead trees use average
  data$CRprop_HT <- ifelse(data$STANDING_DEAD_CD == 1,
                           data$CRmn / 100,
                           data$CRprop_HT)
  
  data$broken_crn_prop <- pmax((data$ACTUALHT - (1 - data$CRprop_HT) * data$THT)/(data$CRprop_HT * data$THT), 0)
  
  data[!data$BROKEN_TOP, 'broken_crn_prop'] <- 1
  
  data$BRANCH_REDUCTION <- data$broken_crn_prop * data$DECAY_BR
  
  data$FOLIAGE_REDUCTION <- ifelse(data$STANDING_DEAD_CD == 1,
                                   0,
                                   data$broken_crn_prop)
  
  # get reduced components
  data$Wood_Reduced <- data$Wood * data$WOOD_REDUCTION
  data$Bark_Reduced <- data$Bark * data$BARK_REDUCTION
  data$Branch_Reduced <- data$Branch * data$BRANCH_REDUCTION
  data$Foliage_Reduced <- data$FOLIAGE * data$FOLIAGE_REDUCTION
  
  data$AGB_Reduction_Factor <- (data$Wood_Reduced + data$Bark_Reduced + data$Branch_Reduced) / 
    (data$Wood + data$Bark + data$Branch)

  data$Total_Reduced <- data$Total * data$AGB_Reduction_Factor
  
  cat("harmonizing components\n")

  # total from comps
  data$TotalC <- data$Wood_Reduced + data$Bark_Reduced + data$Branch_Reduced

  # difference between the two estimates
  data$Diff <- data$Total_Reduced - data$TotalC

  # distribute differences across the three comps
  data$WoodR <- data$Wood_Reduced / data$TotalC
  data$BarkR <- data$Bark_Reduced / data$TotalC
  data$BranchR <- data$Branch_Reduced / data$TotalC

  data$WoodAdd <- data$Diff * data$WoodR
  data$BarkAdd <- data$Diff * data$BarkR
  data$BranchAdd <- data$Diff * data$BranchR

  data$WoodF <- data$WoodAdd + data$Wood_Reduced
  data$BarkF <- data$BarkAdd + data$Bark_Reduced
  data$BranchF <- data$BranchAdd + data$Branch_Reduced

  # recalculate SG
  data$WDSGAdj <- data$WoodF / data$VTOTIB_PRESENT / 62.4
  data$BKSGAdj <- data$BarkF / data$VTOTBK_PRESENT / 62.4

  # merch stem weights
  data$WMERIB <- data$VMERIB_PRESENT * data$WDSGAdj * 62.4
  data$WMERBK <- data$VMERBK_PRESENT * data$BKSGAdj * 62.4
  data$WMEROB <- data$WMERIB + data$WMERBK

  # stump weights
  data$WSTPIB <- data$VSTPIB_PRESENT * data$WDSGAdj * 62.4
  data$WSTPBK <- data$VSTPBK_PRESENT * data$BKSGAdj * 62.4
  data$WSTPOB <- data$WSTPIB + data$WSTPBK

  # top weights
  data$WTOPIB <- data$VTOPIB_PRESENT * data$WDSGAdj * 62.4
  data$WTOPBK <- data$VTOPBK_PRESENT * data$BKSGAdj * 62.4
  data$WTOPOB <- data$WTOPIB + data$WTOPBK

  # total stem weight
  data$WTOTIB <- data$WoodF
  data$WTOTBK <- data$BarkF
  data$WTOTOB <- data$WoodF + data$BarkF

  # branch
  data$WTOTBCH <- data$BranchF
  data$WMERBCH <- data$WTOTBCH + data$WTOPOB

  # foliage
  data$FOLIAGE <- data$Foliage_Reduced
  
  # total biomass
  data$BIOMASS <- data$Total_Reduced

  # add foliage
  data$AGB <- data$BIOMASS + data$FOLIAGE

  out_vars <- c(keepN,
                "VTOTIB_SOUND", "VTOTOB_SOUND", "VTOTBK_SOUND",
                "VMERIB_SOUND", "VMEROB_SOUND", "VMERBK_SOUND",
                "VSTPIB_SOUND", "VSTPOB_SOUND", "VSTPBK_SOUND",
                "VSAWIB_SOUND", "VSAWOB_SOUND", "VSAWBK_SOUND",
                "VTOPIB_SOUND", "VTOPOB_SOUND", "VTOPBK_SOUND",
                "WTOTIB", "WTOTOB", "WTOTBK",
                "WMERIB", "WMEROB", "WMERBK",
                "WTOTBCH", "WMERBCH", "WSTPOB",
                "FOLIAGE", "AGB",
                "BIOMASS")

  if (all.vars) {

    return(data)

  }

  out <- data[,out_vars]

  if (gross.volume) {
  
    gvols <- data[,grepl("_GROSS", names(data))]
    
    out <- cbind(out, gvols)

  }

  return(out)

}

}

fia2 <- predictCRM2(data= fia1,
                    coef_dir= "nsbe_tech_transfer/VTECO/Coefs/combined/")

fia2[fia2$STANDING_DEAD_CD == 1, "class"] <- "standing_dead"
fia2[fia2$STANDING_DEAD_CD == 0 &
       fia2$CULL > 0 &
       !fia2$BROKEN_TOP, "class"] <- "cull"
fia2[fia2$STANDING_DEAD_CD == 0 &
       fia2$CULL < 1 &
       fia2$BROKEN_TOP, "class"] <- "broken_top"
fia2[fia2$STANDING_DEAD_CD == 0 &
       fia2$CULL > 0 &
       fia2$BROKEN_TOP, "class"] <- "cull_and_broken_top"
fia2[fia2$STANDING_DEAD_CD == 0 &
       fia2$CULL < 1 &
       !fia2$BROKEN_TOP, "class"] <- "alive"

fia2$DRYBIO_STEM_WOOD <- fia2$WTOTIB
fia2$DRYBIO_STEM_BARK <- fia2$WTOTBK
fia2$DRYBIO_BRANCHES <- fia2$WTOTBCH
fia2$DRYBIO_AG <- fia2$BIOMASS
fia2$DRYBIO_FOLIAGE <- fia2$FOLIAGE

by_sum <- by(fia2,
             fia2$class,
             function(x) summary(x[,c(grepl("DRYBIO_", names(x)))]))

library(dplyr)
by_sum <- fia2 %>%
  melt(measure.vars= c("DRYBIO_STEM_WOOD",
                       "DRYBIO_STEM_BARK",
                       "DRYBIO_BRANCHES",
                       "DRYBIO_AG",
                       "DRYBIO_FOLIAGE"),
       na.rm= TRUE,
       variable.name= "component") %>%
  group_by(class, component) %>%
  summarise(Min= min(value),
            Mean = mean(value),
            Max= max(value)) %>%
  melt(measure.vars= c("Min", "Mean", "Max"), variable.name= 'variable') %>%
  dcast(class+variable~component)

by_sum[by_sum$variable == 'Min',]

fia2_check <- fia2[fia2$STANDING_DEAD_CD < 1 &
                     !fia2$BROKEN_TOP &
                     fia2$CULL < 1,]

fia1_check <- fia[fia$STANDING_DEAD_CD < 1 &
                     !fia$BROKEN_TOP &
                     fia$CULL_FLD < 1,]

plot(fia1_check$DRYBIO_AG, fia2_check$BIOMASS)

# names(fia2)[grepl("^V", names(fia2))][1:15] <- 
#   paste0(names(fia2)[grepl("^V", names(fia2))][1:15], "_SOUND")

library(reshape2)
fiaL <- melt(fia_check[,c("TRE_CN", "STATUSCD", "SFTWD_HRDWD", "ECOPROV",
                    "STATECD",
                    name_trans$fia)],
             measure.vars= name_trans$fia)
fiaL <- merge(x= fiaL,
              y= name_trans,
              by.x= "variable",
              by.y= "fia")

fia2L <- melt(fia2_check,
              measure.vars= name_trans$dmw)

dd <- merge(x= fiaL,
            y= fia2L,
            by.x= c("TRE_CN", "dmw"),
            by.y= c("TRE_CN", "variable"))

library(ggplot2)
ggplot(dd, aes(x= value.x, y= value.y)) +
 geom_point() +
 facet_wrap(~variable, scales= "free")

options(scipen= 10)
checkComp <- function(var) {
  
  tmp <- dd[dd$variable == var,]
  tmp <- tmp[!is.na(tmp$diff), ]
  cat(as.character(var), "\n")
  summary(tmp$diff)
  
}

# vlist <- unique(dd$variable)
# 
# i=0
# i=i+1
# checkComp(vlist[i])

dd[dd$variable == 'DRYBIO_TOP' & !is.na(dd$diff) & dd$diff > 15,]

ts <- dd[dd$variable == 'DRYBIO_STEM',]

summary(ts[!is.na(ts$diff), ]$diff)

ts[ts$variable == 'DRYBIO_STEM' & ts$diff > 38 & !is.na(ts$diff), ]

printTree <- function(cn) {

  dd[dd$TRE_CN == cn,
     c('TRE_CN', 'DBH', 'THT', 'SPCD', 'DIVISION', 'ACTUALHT', 'CR', 'CULL',
       'variable', 'dmw', 'value.x', 'value.y')]
  
}

(printTree('534130561126144'))

(printTree('235025570020004'))

dd[dd$value.x < 0.001 & !is.na(dd$value.x), 'value.x'] <- 0
dd[dd$value.y < 0.001 & !is.na(dd$value.y), 'value.y'] <- 0

dd$diff <- abs((dd$value.x - dd$value.y) / dd$value.x * 100)
dd[is.infinite(dd$diff), 'diff'] <- 0

dd <- dd[dd$ACTUALHT <= dd$THT, ]

check <- dd[!is.na(dd$diff),]

checkDT <- as.data.table(check)

summ <- checkDT[,.(max_pct_diff= max(abs(diff))), "variable"]

library(ggplot2)
ggplot(summ, aes(x= variable, y= max_pct_diff)) +
  geom_bar(stat= 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept= 5, linetype= 5, colour= 'red')

check[which.max(check$diff),]$diff

table(fia2[fia2$ACTUALHT > fia2$THT,]$STANDING_DEAD_CD)

fia[fia$ACTUALHT > fia$HT & fia$STATUSCD ==1 & fia$STATECD %in% c(6, 41),]

summary(check$diff)

plot(check$diff)

plot(check[check$diff < 60,]$diff)

sap <- dd[dd$DBH < 5,]
mer <- dd[dd$DBH >= 5,]

sap_no_match <- sap[is.na(sap$value.x) & !is.na(sap$value.y),]
table(sap_no_match$variable)

mer_no_match <- mer[is.na(mer$value.x) & !is.na(mer$value.y),]
table(mer_no_match$variable)

fia2[fia2$DBH < 5 & is.na(fia2$BIOMASS),]

sap[sap$variable == 'DRYBIO_AG' & !is.na(sap$value.x),]

sap[sap$variable == 'DRYBIO_BRANCH' & is.na(sap$diff),]

table(sap[is.na(sap$diff), "variable"])

merch <- dd[dd$DBH >= 5,]

table(merch[is.na(merch$diff), "variable"])

merch[merch$variable == 'DRYBIO_STEM_TOP' & is.na(merch$diff),][1,]

printTree('243401467489998')

library(ggplot2)

ggplot(dd, aes(x= value.x, y= value.y, colour= BROKEN_TOP)) +
  geom_point() +
  facet_wrap(~variable, scales= "free") +
  geom_abline(size= 1, colour= 'black', linetype= 5)

dd[dd$CR < 1,]

dd <- dd[dd$CR > 0,]

dd$diff <- abs((dd$value.x - dd$value.y) / dd$value.x * 100)
dd[is.infinite(dd$diff), 'diff'] <- 0

max(dd$diff, na.rm= TRUE)

dd[which(dd$diff == max(dd$diff, na.rm= TRUE)),]

dd[dd$diff %in% 100,]

# dd2 <- dd[!is.na(dd$)]

dd[is.na(dd$value.x) & !is.na(dd$value.y),]

merch <- dd[dd$DBH >= 5.0,]

merch <- merch[merch$CULL < 98, ]

merch$diff <- abs((merch$value.x - merch$value.y) / merch$value.x * 100)

merch[which(merch$diff == max(merch$diff)),]

merch[!is.na(merch$value.x) & is.na(merch$value.y),]

dd[dd$TRE_CN == '534148610126144',
   c('TRE_CN', 'DBH', 'THT', 'SPCD', 'DIVISION', 'ACTUALHT', 'CULL',
     'variable', 'value.x', 'value.y')]

dd[is.na(dd$value.x) & !is.na(dd$value.y),]

makePlot <- function(var) {
  
  ggplot(dd[dd$variable == var,], aes(x= value.x, y= value.y)) +
    geom_point() +
    facet_wrap(~variable, scales= "free") +
    geom_abline(size= 1, colour= 'red', linetype= 5)
  
}

makePlot("VOLCFSND_BARK")

dd <- cbind(fia2[fia2$SPCD == 531,
                 c("TRE_CN", "SPCD", "DIVISION", "DBH", "THT", "ACTUALHT", "CULL", "BIOMASS", "VTOTIB")], fia[fia$SPN == 531,c("DRYBIO_AG", "VOLTSSND"),drop= FALSE])

head(dd)

# total stem vols gross
plot(fia2$VTOTIB_GROSS, fia$VOLTSGRS); abline(0,1,col='red')
plot(fia2$VTOTBK, fia$VOLTSGRS_BARK); abline(0,1,col='red')

# merch stem vols gross
plot(fia2$VMERIB_GROSS, fia$VOLCFGRS); abline(0,1,col='red')
plot(fia2$VMERBK, fia$VOLCFGRS_BARK); abline(0,1,col='red')

# stump vols gross
plot(fia2$VSTPIB_GROSS, fia$VOLCFGRS_STUMP); abline(0,1,col='red')
plot(fia2$VSTPBK, fia$VOLCFGRS_STUMP_BARK); abline(0,1,col='red')

# top vols gross
plot(fia2$VTOPIB_GROSS, fia$VOLCFGRS_TOP); abline(0,1,col='red')
plot(fia2$VTOPBK, fia$VOLCFGRS_TOP_BARK); abline(0,1,col='red')


# total stem vols snd
plot(fia2$VTOTIB, fia$VOLTSSND); abline(0,1,col='red')

# merch stem vols snd
plot(fia2$VMERIB, fia$VOLCFSND); abline(0,1,col='red')

# stump vols snd
plot(fia2$VSTPIB, fia$VOLCFSND_STUMP); abline(0,1,col='red')

# top vols gross
plot(fia2$VTOPIB, fia$VOLCFSND_TOP); abline(0,1,col='red')

v1= 'DRYBIO_STUMP'
v2= 'WSTPIB'
checkDiff <- function(v1, v2) {

  diff <- abs(fia[,v1] - fia2[,v2])
  d1 <- fia[order(diff, decreasing = TRUE),
            c('TRE_CN', 'SPN', 'ECOSUBCD', 'ECODIV',
              'CULL_FLD',
              'DIA', "HT", 'ACTUALHT', v1)]
  d2 <- fia2[order(diff, decreasing = TRUE),
            c(v2),drop=FALSE]

  head(cbind(d1, d2))


}

# weights
# stump wood weight
all.equal(fia2$WSTPIB, fia$DRYBIO_STUMP)
plot(fia2$WSTPIB, fia$DRYBIO_STUMP); abline(0,1,col='red')
plot(fia2$WSTPBK, fia$DRYBIO_STUMP_BARK); abline(0,1,col='red')

# merch stem
plot(fia2$WMERIB, fia$DRYBIO_BOLE); abline(0,1,col='red')
plot(fia2$WMERBK, fia$DRYBIO_BOLE_BARK); abline(0,1,col='red')

plot(fia2$BIOMASS, fia$DRYBIO_AG); abline(0,1,col='red')

# branches
plot(fia2$WTOTBCH, fia$DRYBIO_BRANCH); abline(0,1,col='red')
plot(fia2$WMERBCH, fia$DRYBIO_TOP); abline(0,1,col='red')

# stem top wood
plot(fia2$WTOPIB, fia$DRYBIO_TOP - fia$DRYBIO_BRANCH - fia$DRYBIO_TOP_BARK); abline(0,1,col='red')
abline(0,1)

fia[fia$TRE_CN == 310582466489998,c('ECOSUBCD', 'ECODIV')]

fia2[fia2$TRE_CN == 310582466489998, c('ECOSUBCD', 'ECODIV')]

nrow(fia2[fia2$SPCD == 827 & fia2$DIVISION == " 20",])

plot(fia2$WMERBK, fia$DRYBIO_BOLE_BARK); abline(0,1,col='red')

diff <- abs(fia2$WSTPIB - fia$DRYBIO_STUMP)
which(diff == max(diff))

fia2[2721,]
fia[2721,]

fiadb <- fread("~/Documents/Work/R_Data/fiadb_estimates/fiadb_vteco.csv")

fiadb[fiadb$ECOSUBCD == '234Ec',][1,]
