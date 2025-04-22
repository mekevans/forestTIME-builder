#originally from carbon_code/Decay_and_Dead/R/VT_CRM2_ApplyFuns.R
#overwrites the predictCRM2() function in carbon_code/R/VT_CRM2_ApplyFuns.R.
#I do not know why.  This seems to be the only version that is actually used in Renata's code
predictCRM2 <- function(
  data,
  # coef_dir,
  forms,
  var_names = c(DBH = "DBH", THT = "THT", CULL = "CULL"),
  gross.volume = FALSE,
  all.vars = TRUE
) {
  msg <- "prepping data"
  cli::cli_progress_step("Estimating carbon: {msg}", spinner = TRUE)
  data <- as.data.frame(data)

  keepN <- names(data)

  # add a unique id
  data$ID <- 1:nrow(data)

  data$SPCD_NUMERIC <- as.numeric(gsub("1_", "", data$SPCD))

  # split point
  data$k <- ifelse(data$SPCD_NUMERIC < 300, 9, 11)
  data$saw <- ifelse(data$SPCD_NUMERIC < 300, 7, 9)

  # adjust var names
  data[, names(var_names)] <- data[, var_names]

  # all needed coefs are stored as internal package data.  See prep_internal_data.R
  # coef_files <- list.files(coef_dir, pattern = "_coefs.csv")
  # all_coefs <- lapply(
  #   coef_files,
  #   function(x) read.csv(file.path(coef_dir, x), as.is = TRUE)
  # )
  # names(all_coefs) <- gsub("_coefs.csv", "", coef_files)

  levels <- c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD")

  # predict all volumes first
  # total wood volume
  msg <- "predicting total stem wood volume"
  cli::cli_progress_update()
  all_volib <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["volib"]],
    lhs = 'VTOTIB_GROSS',
    forms = forms
  )

  all_volib <- Reduce(combineLevels2, all_volib)

  data <- merge(x = data, y = all_volib, by = "ID")

  # total bark volume
  msg <- "predicting total stem wood and bark volume"
  cli::cli_progress_update()
  all_volob <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["volbk"]],
    lhs = 'VTOTBK_GROSS',
    forms = forms
  )

  all_volob <- Reduce(combineLevels2, all_volob)

  data <- merge(x = data, y = all_volob, by = "ID")

  # outside bark volume
  data$VTOTOB_GROSS <- data$VTOTIB_GROSS + data$VTOTBK_GROSS

  # merch height
  msg <- "finding merchantable height"
  cli::cli_progress_update()
  ht4 <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = list(all_coefs[["rcumob"]], all_coefs[["volob"]]),
    lhs = 'HT4',
    forms = forms
  )

  ht4 <- Reduce(combineLevels2, ht4)

  ht4$HT4 <- pmax(ht4$HT4, 5)

  data <- merge(x = data, y = ht4, by = "ID")

  # merch vol ib
  msg <- "predicting merchantable stem wood volume"
  cli::cli_progress_update()
  vmerib <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VMERIB_GROSS',
    forms = forms
  )

  vmerib <- Reduce(combineLevels2, vmerib)

  data <- merge(x = data, y = vmerib, by = "ID")

  # merch vol ob
  # use rcumib coefs to ensure positive bark vol
  msg <- "predicting merchantable stem wood and bark volume"
  cli::cli_progress_update()
  vmerob <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VMEROB_GROSS',
    forms = forms
  )

  vmerob <- Reduce(combineLevels2, vmerob)

  data <- merge(x = data, y = vmerob, by = "ID")

  data$VMERBK_GROSS <- data$VMEROB_GROSS - data$VMERIB_GROSS

  # stump vol ib
  msg <- "predicting stump wood volume"
  cli::cli_progress_update()
  vstpib <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VSTPIB_GROSS',
    forms = forms
  )

  vstpib <- Reduce(combineLevels2, vstpib)

  data <- merge(x = data, y = vstpib, by = "ID")

  # stump vol ob
  msg <- "predicting stump wood and bark volume"
  cli::cli_progress_update()
  vstpob <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VSTPOB_GROSS',
    forms = forms
  )

  vstpob <- Reduce(combineLevels2, vstpob)

  data <- merge(x = data, y = vstpob, by = "ID")

  # no stump volumes for saplings
  data[data$DIA < 5.0, c("VSTPOB_GROSS", "VSTPIB_GROSS")] <- NA

  data$VSTPBK_GROSS <- data$VSTPOB_GROSS - data$VSTPIB_GROSS

  # tip vol ib
  data$VTOPIB_GROSS <- data$VTOTIB_GROSS - data$VMERIB_GROSS - data$VSTPIB_GROSS

  # tip vol ob
  data$VTOPOB_GROSS <- data$VTOTOB_GROSS - data$VMEROB_GROSS - data$VSTPOB_GROSS
  data$VTOPBK_GROSS <- data$VTOPOB_GROSS - data$VTOPIB_GROSS

  # sawlog vols
  msg <- "finding sawlog height"
  cli::cli_progress_update()
  htsaw <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = list(all_coefs[["rcumob"]], all_coefs[["volob"]]),
    lhs = 'HTSAW',
    forms = forms
  )

  htsaw <- Reduce(combineLevels2, htsaw)

  htsaw$HTSAW <- pmax(htsaw$HTSAW, 5)

  data <- merge(x = data, y = htsaw, by = "ID")

  # sawtimber vol ib
  msg <- "predicting sawlog stem wood volume"
  cli::cli_progress_update()
  vsawib <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VSAWIB_GROSS',
    forms = forms
  )

  vsawib <- Reduce(combineLevels2, vsawib)

  data <- merge(x = data, y = vsawib, by = "ID")

  # sawtimber vol ob
  # use rcumib coefs to ensure positive bark vol
  msg <- "predicting sawlog stem wood and bark volume"
  cli::cli_progress_update()
  vsawob <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'VSAWOB_GROSS',
    forms = forms
  )

  vsawob <- Reduce(combineLevels2, vsawob)

  data <- merge(x = data, y = vsawob, by = "ID")

  data$VSAWBK_GROSS <- data$VSAWOB_GROSS - data$VSAWIB_GROSS

  # get height reduction for broken tops
  htadj <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["rcumib"]],
    lhs = 'HT_RAT_ADJ',
    forms = forms
  )

  htadj <- Reduce(combineLevels2, htadj)

  data <- merge(x = data, y = htadj, by = "ID")

  data$HT_RAT_ADJ <- ifelse(data$ACTUALHT < data$THT, data$HT_RAT_ADJ, 1)

  # apply broken top adjustment to volumes
  # this will be the volume used to determine wood specific gravity
  data$VTOTIB_PRESENT <- data$VTOTIB_GROSS * data$HT_RAT_ADJ
  data$VTOTBK_PRESENT <- data$VTOTBK_GROSS * data$HT_RAT_ADJ
  data$VTOTOB_PRESENT <- data$VTOTIB_PRESENT + data$VTOTBK_PRESENT

  data$VMERIB_PRESENT <- ifelse(
    data$ACTUALHT < data$HT4,
    (data$VTOTIB_GROSS * data$HT_RAT_ADJ) - data$VSTPIB_GROSS,
    data$VMERIB_GROSS
  )
  data$VMERBK_PRESENT <- ifelse(
    data$ACTUALHT < data$HT4,
    (data$VTOTBK_GROSS * data$HT_RAT_ADJ) - data$VSTPBK_GROSS,
    data$VMERBK_GROSS
  )
  data$VMEROB_PRESENT <- data$VMERIB_PRESENT + data$VMERBK_PRESENT

  data$VSAWIB_PRESENT <- ifelse(
    data$ACTUALHT < data$HTSAW,
    (data$VTOTIB_GROSS * data$HT_RAT_ADJ) - data$VSTPIB_GROSS,
    data$VSAWIB_GROSS
  )
  data$VSAWBK_PRESENT <- ifelse(
    data$ACTUALHT < data$HTSAW,
    (data$VTOTBK_GROSS * data$HT_RAT_ADJ) - data$VSTPBK_GROSS,
    data$VSAWBK_GROSS
  )
  data$VSAWOB_PRESENT <- data$VSAWIB_PRESENT + data$VSAWBK_PRESENT

  data$VSTPIB_PRESENT <- data$VSTPIB_GROSS
  data$VSTPBK_PRESENT <- data$VSTPBK_GROSS
  data$VSTPOB_PRESENT <- data$VSTPOB_GROSS

  data$VTOPIB_PRESENT <- data$VTOTIB_PRESENT -
    data$VMERIB_PRESENT -
    data$VSTPIB_PRESENT
  data$VTOPBK_PRESENT <- data$VTOTBK_PRESENT -
    data$VMERBK_PRESENT -
    data$VSTPBK_PRESENT
  data$VTOPOB_PRESENT <- data$VTOPIB_PRESENT + data$VTOPBK_PRESENT

  # if broken top is below bole height top vols are zero
  data[
    data$ACTUALHT < data$HT4 & !is.na(data$HT4),
    c("VTOPIB_PRESENT", "VTOPBK_PRESENT", "VTOPOB_PRESENT")
  ] <- 0

  # this is the reduction factor to apply to all present volumes
  # data[data$CULL >= 99, "CULL"] <- 100
  data$SND_WOOD_DECAY <- ifelse(data$CULL < 99, (1 - data$CULL / 100), 0)

  pres_vol_list <- c(
    "VTOTIB_PRESENT",
    "VMERIB_PRESENT",
    "VSTPIB_PRESENT",
    "VTOPIB_PRESENT",
    "VSAWIB_PRESENT"
  )

  for (i in pres_vol_list) {
    i2 <- gsub("_PRESENT", "_SOUND", i)

    data[, i2] <- data[, i] * data$SND_WOOD_DECAY
  }

  # cull not applied to bark
  # but bark decay rates applied to bark vols so densities make sense
  data$VTOTBK_SOUND <- data$VTOTBK_PRESENT * data$DECAY_BK
  data$VSTPBK_SOUND <- data$VSTPBK_PRESENT * data$DECAY_BK
  data$VMERBK_SOUND <- data$VMERBK_PRESENT * data$DECAY_BK
  data$VTOPBK_SOUND <- data$VTOPBK_PRESENT * data$DECAY_BK
  data$VSAWBK_SOUND <- data$VSAWBK_PRESENT * data$DECAY_BK

  # get sound ob vols
  data$VTOTOB_SOUND <- data$VTOTIB_SOUND + data$VTOTBK_SOUND
  data$VMEROB_SOUND <- data$VMERIB_SOUND + data$VMERBK_SOUND
  data$VSTPOB_SOUND <- data$VSTPIB_SOUND + data$VSTPBK_SOUND
  data$VTOPOB_SOUND <- data$VTOPIB_SOUND + data$VTOPBK_SOUND
  data$VSAWOB_SOUND <- data$VSAWIB_SOUND + data$VSAWBK_SOUND

  # sawlog volume rules:
  # sawtimber rules: 9" for softwoods, 11" for hardwoods
  # otherwise missing
  data[
    data$SFTWD_HRDWD == 'S' & data$DBH < 9.0 & !is.na(data$DBH),
    c(
      "VSAWIB_GROSS",
      "VSAWOB_GROSS",
      "VSAWBK_GROSS",
      "VSAWIB_PRESENT",
      "VSAWOB_PRESENT",
      "VSAWBK_PRESENT",
      "VSAWIB_SOUND",
      "VSAWOB_SOUND",
      "VSAWBK_SOUND"
    )
  ] <- NA
  data[
    data$SFTWD_HRDWD == 'H' & data$DBH < 11.0 & !is.na(data$DBH),
    c(
      "VSAWIB_GROSS",
      "VSAWOB_GROSS",
      "VSAWBK_GROSS",
      "VSAWIB_PRESENT",
      "VSAWOB_PRESENT",
      "VSAWBK_PRESENT",
      "VSAWIB_SOUND",
      "VSAWOB_SOUND",
      "VSAWBK_SOUND"
    )
  ] <- NA

  # total biomass
  msg <- "predicting total biomass"
  cli::cli_progress_update()
  totbio <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["total_biomass"]],
    lhs = 'Total',
    forms = forms
  )

  totbio <- Reduce(combineLevels2, totbio)

  data <- merge(x = data, y = totbio, by = "ID")

  # stem bark weight
  msg <- "predicting total stem bark weight"
  cli::cli_progress_update()
  bark_weight <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["bark_biomass"]],
    lhs = 'Bark',
    forms = forms
  )

  bark_weight <- Reduce(combineLevels2, bark_weight)

  data <- merge(x = data, y = bark_weight, by = "ID")

  # branch weight
  msg <- "predicting total branch weight"
  cli::cli_progress_update()
  branch_weight <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["branch_biomass"]],
    lhs = 'Branch',
    forms = forms
  )

  branch_weight <- Reduce(combineLevels2, branch_weight)

  data <- merge(x = data, y = branch_weight, by = "ID")

  # foliage weight
  msg <- "predicting foliage weight"
  cli::cli_progress_update()
  foliage_weight <- lapply(
    levels,
    applyAllLevels,
    data = data,
    coefs = all_coefs[["foliage"]],
    lhs = 'FOLIAGE',
    forms = forms
  )

  foliage_weight <- Reduce(combineLevels2, foliage_weight)

  data <- merge(x = data, y = foliage_weight, by = "ID")

  data$Wood <- data$VTOTIB_GROSS * data$WDSG * 62.4

  # need wood weight, bark weight, branch weight w/ and w/o deductions
  # wood deductions: broken top ratio adjust plus cull decay reduction plus decay
  # bark deductions: broken top ratio adjust plus decay
  # branch deductions: branch loss plus decay
  data$CULL_REDUCTION <- ifelse(
    data$STANDING_DEAD_CD < 1,
    (1 - (1 - data$CULL_DECAY_RATIO) * (data$CULL) / 100),
    1
  )
  data$WOOD_REDUCTION <- data$HT_RAT_ADJ * data$CULL_REDUCTION * data$DECAY_WD
  data$BARK_REDUCTION <- data$HT_RAT_ADJ * data$DECAY_BK * data$DECAY_WD

  # get crown proportion adjustments
  # minimum CR of 1 to prevent 0 branch biomass
  data$CR <- pmax(data$CR, 1)
  # data[fia_check$CR_PROP < 0.1,]$CR <- 0
  data$CRprop_HT <- (1 - data$ACTUALHT / data$THT * (1 - (data$CR) / 100))

  # dead trees use average
  data$CRprop_HT <- ifelse(
    data$STANDING_DEAD_CD == 1,
    data$CRmn / 100,
    data$CRprop_HT
  )

  data$broken_crn_prop <- pmax(
    (data$ACTUALHT - (1 - data$CRprop_HT) * data$THT) /
      (data$CRprop_HT * data$THT),
    0
  )

  data$broken_crn_prop <- ifelse(
    data$broken_crn_prop < 0.01,
    0,
    data$broken_crn_prop
  )

  data[!data$BROKEN_TOP, 'broken_crn_prop'] <- 1

  data$BRANCH_REDUCTION <- data$broken_crn_prop * data$DECAY_BR * data$DECAY_WD

  data$FOLIAGE_REDUCTION <- ifelse(
    data$STANDING_DEAD_CD == 1,
    0,
    data$broken_crn_prop
  )

  # get reduced components
  data$Wood_Reduced <- data$Wood * data$WOOD_REDUCTION
  data$Bark_Reduced <- data$Bark * data$BARK_REDUCTION
  data$Branch_Reduced <- data$Branch * data$BRANCH_REDUCTION
  data$Foliage_Reduced <- data$FOLIAGE * data$FOLIAGE_REDUCTION

  data$AGB_Reduction_Factor <- (data$Wood_Reduced +
    data$Bark_Reduced +
    data$Branch_Reduced) /
    (data$Wood + data$Bark + data$Branch)

  data$Total_Reduced <- data$Total * data$AGB_Reduction_Factor

  msg <- "harmonizing components"
  cli::cli_progress_update()

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

  # sawlog weights
  data$WSAWIB <- data$VSAWIB_PRESENT * data$WDSGAdj * 62.4
  data$WSAWBK <- data$VSAWBK_PRESENT * data$BKSGAdj * 62.4
  data$WSAWOB <- data$WSAWIB + data$WSAWBK

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

  # carbon
  data$CARBON <- data$BIOMASS * (data$C_FRAC / 100)

  # add foliage
  data$AGB <- data$BIOMASS + data$FOLIAGE

  # for saplings, set everything but biomass and foliage to NA
  # also keep total stem and branches
  data[
    data$DBH < 5,
    c(
      "VMERBK_GROSS",
      "VMERBK_SOUND",
      "VMERIB_GROSS",
      "VMERIB_SOUND",
      "VSAWBK_GROSS",
      "VSAWBK_SOUND",
      "VSAWIB_GROSS",
      "VSAWIB_SOUND",
      "VSTPBK_GROSS",
      "VSTPBK_SOUND",
      "VSTPIB_GROSS",
      "VSTPIB_SOUND",
      "VTOPBK_GROSS",
      "VTOPBK_SOUND",
      "VTOPIB_GROSS",
      "VTOPIB_SOUND",
      # "VTOTBK_GROSS", "VTOTBK_SOUND",
      # "VTOTIB_GROSS", "VTOTIB_SOUND",
      "WMERBK",
      "WMERIB",
      "WSTPBK",
      "WSTPIB",
      "WTOPBK",
      "WTOPIB"
      # "WTOTBCH", "WTOTBK", "WTOTIB"
    )
  ] <- NA

  out_vars <- c(
    keepN,
    "VTOTIB_SOUND",
    "VTOTOB_SOUND",
    "VTOTBK_SOUND",
    "VMERIB_SOUND",
    "VMEROB_SOUND",
    "VMERBK_SOUND",
    "VSTPIB_SOUND",
    "VSTPOB_SOUND",
    "VSTPBK_SOUND",
    "VSAWIB_SOUND",
    "VSAWOB_SOUND",
    "VSAWBK_SOUND",
    "VTOPIB_SOUND",
    "VTOPOB_SOUND",
    "VTOPBK_SOUND",
    "WTOTIB",
    "WTOTOB",
    "WTOTBK",
    "WMERIB",
    "WMEROB",
    "WMERBK",
    "WTOTBCH",
    "WMERBCH",
    "WSTPOB",
    "FOLIAGE",
    "AGB",
    "BIOMASS",
    "CARBON"
  )

  if (all.vars) {
    return(data)
  }

  out <- data[, out_vars]

  if (gross.volume) {
    gvols <- data[, grepl("_GROSS", names(data))]

    out <- cbind(out, gvols)
  }

  return(out)
}

# also from the Decay_and_Dead folder
applyAllLevels <- function(data, level, coefs, lhs, forms) {
  data <- as.data.frame(data)

  # subset to level
  getLevel <- function(x) {
    ccs <- c(
      "a",
      "a0",
      "a1",
      "alpha",
      "beta",
      "b",
      "b0",
      "b1",
      "c",
      "c1",
      "b2",
      "equation"
    )

    ccs <- ccs[ccs %in% names(x)]

    x <- x[!is.na(x[, level]), c(level, ccs)]

    return(x)
  }

  if (class(coefs) == 'list') {
    the_coefs <- lapply(coefs, getLevel)

    the_coefs <- Reduce(myMerge, the_coefs)
  } else {
    the_coefs <- getLevel(coefs)
  }

  data <- merge(
    x = data,
    y = the_coefs,
    by = intersect(names(data), names(the_coefs))
  )

  if (nrow(data) == 0) return(NULL)

  if (
    lhs %in% c("Total", "Branch") & any(names(the_coefs) == 'JENKINS_SPGRPCD')
  )
    data$equation <- 3.1

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

    dat2 <- within(dat, eval(parse(text = formula)))

    return(dat2)
  }

  data2 <- do.call(rbind, lapply(data_split, applyIt))

  out_names <- c("ID", lhs)

  data2 <- data2[, out_names]

  #names(data2)[2] <- paste(names(data2)[2], level, sep= "_")

  data2 <- data2[order(data2$ID), ]

  return(data2)
}

combineLevels2 <- function(x, y) {
  level1 <- x
  level2 <- y[!(y$ID %in% x$ID), ]

  out <- rbind(level1, level2)

  if (!is.null(out)) return(out[order(out$ID), ])
}

myMerge <- function(x, y) {
  # cat("merging on", paste(intersect(names(x), names(y)), collapse= ", "), "\n")

  merge(x = x, y = y, by = intersect(names(x), names(y)))
}

findHT <- function(data, dbh = "DBH", tht = "THT", dlim = 4) {
  data$dlim <- ifelse(is.character(dlim), data[, dlim], dlim)

  hmod <- function(D, H, d, alpha, beta, a, b, c) {
    # don't need merch info for saplings
    if (D < 5.0 | is.na(D)) return(NA)

    # create function to optimize
    kz <- function(h) {
      pd = sqrt(
        a *
          D^b *
          H^c /
          .005454154 /
          H *
          alpha *
          beta *
          (1 - h / H)^(alpha - 1) *
          (1 - (1 - h / H)^alpha)^(beta - 1)
      )
      abs(pd - d)
    }
    stats::optimise(kz, lower = 0, upper = H)$minimum
  }

  mapply(
    hmod,
    D = data[, dbh],
    H = data[, tht],
    d = dlim,
    alpha = data$alpha,
    beta = data$beta,
    a = data$a,
    b = data$b,
    c = data$c
  )
}

findVol <- function(
  data,
  hl = 1,
  hu = "HT4",
  dbh = "DBH",
  tht = "THT",
  vol = "VTOTIB"
) {
  if (is.numeric(hl)) data$hl <- hl else data$hl <- data[, hl]
  if (is.numeric(hu)) data$hu <- hu else data$hu <- data[, hu]

  # volume to bottom and top
  data$VL <- (1 - (1 - (data$hl / data[, tht]))^data$alpha)^data$beta *
    data[, vol]
  data$VU <- (1 - (1 - (data$hu / data[, tht]))^data$alpha)^data$beta *
    data[, vol]

  data$VU - data$VL
}
