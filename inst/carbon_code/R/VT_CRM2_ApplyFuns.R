# Virginia Tech CRM2.0 application funs

getDivision <- function(x, prov= FALSE) {
  
  x <- trimws(x)
  
  # first pass
  y <- ifelse(nchar(x) == 0, "",
              ifelse(nchar(x) == 4, substr(x, 1, 3),
                     ifelse(nchar(x) == 7, substr(x, 1, nchar(x) - 3),
                            substr(x, 1, nchar(x) - 2))))
  
  if (prov) return(y)
  
  # finish trimming
  z <- ifelse(nchar(y) != 0,
              paste0(substr(y, 1, nchar(y) - 1),
                     "0"),
              "")
  
  return(z)
  
}

myMerge <- function(x, y) {
  
  # cat("merging on", paste(intersect(names(x), names(y)), collapse= ", "), "\n")
  
  merge(x= x, y= y, by= intersect(names(x), names(y)))
  
}

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
  if (lhs == "VMERIB") data$equation <- 8
  if (lhs == "VMEROB") data$equation <- 9
  if (lhs == "VSTPIB") data$equation <- 10
  if (lhs == "VSTPOB") data$equation <- 11
  if (lhs == "HTSAW") data$equation <- 12
  if (lhs == "VSAWIB") data$equation <- 13
  if (lhs == "VSAWOB") data$equation <- 14
  
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

combineLevels <- function(x, y) {
  
  y <- y[!y$ID %in% x$ID,]
  
  x[!x$ID %in% y$ID,]
  
}

combineLevels2 <- function(x, y) {
  
  level1 <- x
  level2 <- y[!(y$ID %in% x$ID),]
  
  out <- rbind(level1, level2)
  
  if (!is.null(out))  return(out[order(out$ID), ])
  
}

findHT <- function(data, dbh= "DBH", tht= "THT", dlim= 4) {
  
  data$dlim <- ifelse(is.character(dlim),
                      data[,dlim],
                      dlim)
  
  hmod <- function(D,H,d,alpha,beta,a,b,c) {
    
    # don't need merch info for saplings
    if (D < 5.0 | is.na(D)) return(NA)
    
    # create function to optimize
    kz <- function(h) {
      pd= sqrt(a*D^b*H^c/.005454154/H * alpha*beta*(1-h/H)^(alpha-1)*(1-(1-h/H)^alpha)^(beta-1))
      abs(pd - d)
    }
    
    optimise(kz, lower= 0, upper= H)$minimum
    
  }
  
  mapply(hmod,
         D= data[,dbh],
         H= data[,tht],
         d= dlim,
         alpha= data$alpha,
         beta= data$beta,
         a= data$a,
         b= data$b,
         c= data$c)
  
}

findVol <- function(data, hl= 1, hu= "HT4", dbh= "DBH", tht= "THT", vol= "VTOTIB") {
  
  if (is.numeric(hl)) data$hl <- hl else data$hl <- data[,hl]
  if (is.numeric(hu)) data$hu <- hu else data$hu <- data[,hu]
  
  # volume to bottom and top
  data$VL <- (1 - (1 - (data$hl / data[,tht]))^data$alpha)^data$beta * data[,vol]
  data$VU <- (1 - (1 - (data$hu / data[,tht]))^data$alpha)^data$beta * data[,vol]
  
  data$VU - data$VL
  
}

makeVars <- function(data, levels){
  
  for (i in levels) {
    
    call <- paste0(i, "_DIVISION <- paste(", i, ", DIVISION)")
    
    data <- within(data, eval(parse(text= call)))
    
  }
  
  return(data)
  
}

levels <- c("SPCD_DIVISION", "SPCD", "JENKINS_SPGRPCD")

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
                      lhs= 'VTOTIB')
  
  all_volib <- Reduce(combineLevels2, all_volib)
  
  data <- merge(x= data,
                y= all_volib,
                by= "ID")
  
  # total bark volume
  cat("predicting total stem wood and bark volume\n")
  all_volob <- lapply(levels, applyAllLevels,
                      data= data,
                      coefs= all_coefs[["volbk"]],
                      lhs= 'VTOTBK')
  
  all_volob <- Reduce(combineLevels2, all_volob)
  
  data <- merge(x= data,
                y= all_volob,
                by= "ID")
  
  # outside bark volume
  data$VTOTOB <- data$VTOTIB + data$VTOTBK
  
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
                   lhs= 'VMERIB')
  
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
                   lhs= 'VMEROB')
  
  vmerob <- Reduce(combineLevels2, vmerob)
  
  data <- merge(x= data,
                y= vmerob,
                by= "ID")
  
  data$VMERBK <- data$VMEROB - data$VMERIB
  
  # stump vol ib
  cat("predicting stump wood volume\n")
  vstpib <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSTPIB')
  
  vstpib <- Reduce(combineLevels2, vstpib)
  
  data <- merge(x= data,
                y= vstpib,
                by= "ID")
  
  # stump vol ob
  cat("predicting stump wood and bark volume\n")
  vstpob <- lapply(levels, applyAllLevels,
                   data= data,
                   coefs= all_coefs[["rcumib"]],
                   lhs= 'VSTPOB')
  
  vstpob <- Reduce(combineLevels2, vstpob)
  
  data <- merge(x= data,
                y= vstpob,
                by= "ID")
  
  # no stump volumes for saplings
  data[data$DIA < 5.0, c("VSTPOB", "VSTPIB")] <- NA
  
  data$VSTPBK <- data$VSTPOB - data$VSTPIB
  
  # tip vol ib
  data$VTOPIB <- data$VTOTIB - data$VMERIB - data$VSTPIB
  
  # tip vol ob
  data$VTOPOB <- data$VTOTOB - data$VMEROB - data$VSTPOB
  data$VTOPBK <- data$VTOPOB - data$VTOPIB
  
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
                   lhs= 'VSAWIB')
  
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
                   lhs= 'VSAWOB')
  
  vsawob <- Reduce(combineLevels2, vsawob)
  
  data <- merge(x= data,
                y= vsawob,
                by= "ID")
  
  data$VSAWBK <- data$VSAWOB - data$VSAWIB
  
  # sawtimber rules: 9" for softwoods, 11" for hardwoods
  # otherwise missing
  data[data$SPCD_NUMERIC < 300 & data$DBH <  9.0 & !is.na(data$DBH),
       c("VSAWIB", "VSAWOB", "VSAWBK")] <- NA
  data[data$SPCD_NUMERIC > 300 & data$DBH < 11.0 & !is.na(data$DBH),
       c("VSAWIB", "VSAWOB", "VSAWBK")] <- NA
  
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
  
  # bcef
  data$BCEF <- data$Total / data$VTOTIB
  
  # apply cull
  # hardcode these to be safe
  vol_vars_ib <- c("VTOTIB",
                   "VMERIB",
                   "VSTPIB",
                   "VTOPIB",
                   "VSAWIB")
  
  vol_vars_ob <- c("VTOTOB",
                   "VMEROB",
                   "VSTPOB",
                   "VTOPOB",
                   "VSAWOB")
  
  # keep gross vars as well
  gvols <- data[,c("ID", c(vol_vars_ib, vol_vars_ob))]
  
  for (i in vol_vars_ib) {
    
    data[,i] <- data[,i] * (1 - data$CULL / 100)
    
  }
  
  data$VTOTOB <- data$VTOTIB + data$VTOTBK
  data$VMEROB <- data$VMERIB + data$VMERBK
  data$VSTPOB <- data$VSTPIB + data$VSTPBK
  data$VTOPOB <- data$VTOPIB + data$VTOPBK
  data$VSAWOB <- data$VSAWIB + data$VSAWBK
  
  # convert total stem wood vol to weight
  data$Wood  <- data$VTOTIB * data$WDSG * 62.4
  # data$Bark0 <- data$VTOTBK * data$BKSG * 62.4
  
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
  
  cat("harmonizing components and total\n")
  
  # use volume * miles and smith
  # data$Bark <- data$Bark0
  
  # get adjusted total from bcef
  data$Total <- data$VTOTIB * data$BCEF
  
  # total from comps
  data$TotalC <- data$Wood + data$Bark + data$Branch
  
  # difference between the two estimates
  data$Diff <- data$Total - data$TotalC
  
  # distribute differences across the three comps
  data$WoodR <- data$Wood / data$TotalC
  data$BarkR <- data$Bark / data$TotalC
  data$BranchR <- data$Branch / data$TotalC
  
  data$WoodAdd <- data$Diff * data$WoodR
  data$BarkAdd <- data$Diff * data$BarkR
  data$BranchAdd <- data$Diff * data$BranchR
  
  data$WoodF <- data$WoodAdd + data$Wood
  data$BarkF <- data$BarkAdd + data$Bark
  data$BranchF <- data$BranchAdd + data$Branch
  
  # recalculate SG
  data$WDSGAdj <- data$WoodF / data$VTOTIB / 62.4
  data$BKSGAdj <- data$BarkF / data$VTOTBK / 62.4
  
  # merch stem weights
  data$WMERIB <- data$VMERIB * data$WDSGAdj * 62.4
  data$WMERBK <- data$VMERBK * data$BKSGAdj * 62.4
  data$WMEROB <- data$WMERIB + data$WMERBK
  
  # stump weights
  data$WSTPIB <- data$VSTPIB * data$WDSGAdj * 62.4
  data$WSTPBK <- data$VSTPBK * data$BKSGAdj * 62.4
  data$WSTPOB <- data$WSTPIB + data$WSTPBK
  
  # top weights
  data$WTOPIB <- data$VTOPIB * data$WDSGAdj * 62.4
  data$WTOPBK <- data$VTOPBK * data$BKSGAdj * 62.4
  data$WTOPOB <- data$WTOPIB + data$WTOPBK
  
  # total stem weight
  data$WTOTIB <- data$WoodF
  data$WTOTBK <- data$BarkF
  data$WTOTOB <- data$WoodF + data$BarkF
  
  # branch
  data$WTOTBCH <- data$BranchF
  data$WMERBCH <- data$WTOTBCH + data$WTOPOB
  
  # total biomass
  data$BIOMASS <- data$Total
  
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
  
  # add foliage
  data$AGB <- data$BIOMASS + data$FOLIAGE
  
  out_vars <- c(keepN,
                "VTOTIB", "VTOTOB", "VTOTBK",
                "VMERIB", "VMEROB", "VMERBK",
                "VSTPIB", "VSTPOB", "VSTPBK", 
                "WTOTIB", "WTOTOB", "WTOTBK",
                "WMERIB", "WMEROB", "WMERBK",
                "WTOTBCH", "WMERBCH", "WSTPOB",
                "VSAWIB", "VSAWOB", "VSAWBK",
                "FOLIAGE", "AGB",
                "BIOMASS")
  
  if (all.vars) {
    
    grossv_names <- names(gvols)[2:ncol(gvols)]
    grossv_names <- paste0(grossv_names, "_GROSS")
    
    names(gvols)[2:ncol(gvols)] <- grossv_names
    
    gvols$ID <- NULL
    
    out <- cbind(data, gvols)
    
    return(out)
    
  }
  
  out <- data[,out_vars]
  
  if (gross.volume) {
    
    grossv_names <- names(gvols)[2:ncol(gvols)]
    grossv_names <- paste0(grossv_names, "_GROSS")
    
    names(gvols)[2:ncol(gvols)] <- grossv_names
    
    gvols$ID <- NULL
    
    out <- cbind(out, gvols)
    
  }
  
  return(out)
  
}
