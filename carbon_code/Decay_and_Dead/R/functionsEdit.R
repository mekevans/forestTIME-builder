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
  if (lhs == "R4") data$equation <- 100.1
  if (lhs == "R1") data$equation <- 100.2
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

  # ratio4
  cat("get volume ratio to the 4-inch top\n")
  r4 <- lapply(levels, applyAllLevels,
               data= data,
               coefs= all_coefs[["rcumib"]],
               lhs= 'R4')

  r4 <- Reduce(combineLevels2, r4)

  data <- merge(x= data,
                y= r4,
                by= "ID")

  data$HT1 <- 1

  # ratio1
  cat("get volume ratio to 1-foot stump\n")
  r1 <- lapply(levels, applyAllLevels,
               data= data,
               coefs= all_coefs[["rcumib"]],
               lhs= 'R1')

  r1 <- Reduce(combineLevels2, r1)

  data <- merge(x= data,
                y= r1,
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

getRatio <- function(data, h, dbh, tht) {

  if (is.numeric(h)) data$h <- h else data$h <- data[,h]

  (1 - (1 - (data[,h] / data[,tht]))^data$alpha)^data$beta

}
