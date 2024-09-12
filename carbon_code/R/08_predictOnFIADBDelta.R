# apply CRM2 to inventory delta dataset

library(data.table)

nsvb_path <- '/mnt/Main/nsbe_tech_transfer/VTECO/'
setwd(nsvb_path)

fiadb_path <- '/mnt/Main/fiadb_nsvb_delta/'

# prep work
fiadb <- fread(file.path(fiadb_path, "data/03_fiadb_delta_biomass.csv"))
names(fiadb) <- toupper(names(fiadb))

# finish editing ecodivision
fiadb$DIVISION <- ifelse(nchar(fiadb$ECODIVISION) != 0,
                         paste0(substr(fiadb$ECODIVISION,
                                       1,
                                       nchar(fiadb$ECODIVISION) - 1),
                                "0"),
                         "")

cfrac <- read.csv("Decay_and_Dead/nsvb/carbon_ratio.csv")
cfrac$C_FRAC <- cfrac$CARBON_RATIO * 100

fiadb <- merge(x= fiadb,
               y= cfrac,
               by.x= 'SPCD',
               by.y= 'spcd')

# make planted pine - use time2 status
fiadb[is.na(fiadb$STDORGCD_END), "STDORGCD_END"] <- 0
fiadb$SPCD <- ifelse(fiadb$SPCD %in% c(111, 131) & fiadb$STDORGCD_END == 1,
                     paste0("1_", fiadb$SPCD),
                     fiadb$SPCD)

# equation numbers and forms
forms <- read.csv(file.path("Files/equation_forms_and_calls.csv"))
add_me <- data.frame(equation= c(3.1, 6.1),
                     rhs= c("<- a * DBH^b * THT^c * WDSG",
                            "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta"))

forms <- rbind(forms, add_me)

forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)

# load application funs
source("R/VT_CRM2_ApplyFuns.R")

# function updates
source("Decay_and_Dead/R/nsvb_update_funcs.R")

fiadb <- makeVars(fiadb, "SPCD")

# no dead trees in GRM
fiadb$STANDING_DEAD_CD <- 0
fiadb$CULL_DECAY_RATIO <- 1

# set missing diameters to 0
fiadb[is.na(fiadb$DIA_BEGIN), 'DIA_BEGIN'] <- 0
fiadb[is.na(fiadb$DIA_END), 'DIA_END'] <- 0

# apply over fiadb
# need predictions for t1 and t2
fiadb1 <- fiadb
fiadb1$ACTUALHT <- fiadb1$ACTUALHT_BEGIN
fiadb1$CR <- fiadb1$CR_BEGIN
fiadb1$BROKEN_TOP <- !(fiadb1$HT_BEGIN == fiadb1$ACTUALHT_BEGIN)

fiadb2 <- fiadb
fiadb2$ACTUALHT <- fiadb2$ACTUALHT_END
fiadb2$CR <- fiadb2$ACTUALHT_END
fiadb2$BROKEN_TOP <- !(fiadb2$HT_END == fiadb2$ACTUALHT_END)

fiadb1 <- predictCRM2(data= fiadb1,
                      coef_dir= "Coefs/combined/",
                      var_names= c(DBH= "DIA_BEGIN", THT= "HT_BEGIN", CULL= "CULLFLD_BEGIN"))

fiadb2 <- predictCRM2(fiadb2,
                      coef_dir= "Coefs/combined/",
                      var_names= c(DBH= "DIA_END", THT= "HT_END", CULL= "CULLFLD_END"))

# all possible pred vars
# need to match up with what the system actually predicts
pred_vars <- c("VTOTIB", "VTOTOB", "VTOTBK", "VMERIB",
               "VMEROB", "VMERBK", "VSTPIB", "VSTPOB", "VSTPBK",
               "WTOTIB", "WTOTOB", "WTOTBK", "WMERIB", "WMEROB",
               "WMERBK", "WTOTBCH", "WMERBCH", "WSTPOB", "VSAWIB",
               "VSAWOB", "VSAWBK", "FOLIAGE", "AGB", "BIOMASS")

pred_vars <- pred_vars[pred_vars %in% names(fiadb1)]

# merge all
fiadb1 <- fiadb1[,c("TREE_CN", pred_vars)]
names(fiadb1)[2:ncol(fiadb1)] <- paste0(names(fiadb1)[2:ncol(fiadb1)], "_BEGIN")
for (i in 2:ncol(fiadb1)) fiadb1[,i] <- fiadb1[,i] * -1

fiadb2 <- fiadb2[,c("TREE_CN", pred_vars)]
names(fiadb2)[2:ncol(fiadb2)] <- paste0(names(fiadb2)[2:ncol(fiadb2)], "_END")

fiadb_out <- Reduce(myMerge, list(fiadb, fiadb1, fiadb2))

# temp file
data.table::fwrite(fiadb_out, file.path(fiadb_path, "data/fiadb_delta_biomass_w_prediction.csv"))
file.copy('Output/fiadb_delta_biomass_w_prediction.csv',
          '../../fiadb_estimates/fiadb_delta_vteco.csv')
