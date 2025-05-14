# apply CRM2 to standing inventory dataset

library(data.table)

fiadb_path <- "/mnt/Main/"

setwd("/mnt/Main/nsbe_tech_transfer/VTECO/")

# prep work
fiadb <- fread(file.path(fiadb_path, "fia_tree_agb_2017_live_dead.csv"))
names(fiadb) <- toupper(names(fiadb))

# no hawaii
fiadb <- fiadb[fiadb$STATECD != 15, ]

# adjust PR/VI a litle
fiadb[fiadb$STATECD == 72, ]$NF_REGION <- NA
fiadb[fiadb$STATECD == 72, ]$RSCD <- 99
fiadb[fiadb$STATECD == 78, ]$NF_REGION <- NA
fiadb[fiadb$STATECD == 78, ]$RSCD <- 99

ref <- fread("../Shared/Files/REF_SPECIES.csv",
             select= c("SPCD", "GENUS", "JENKINS_SPGRPCD",
                       'SFTWD_HRDWD',
                       "WOOD_SPGR_GREENVOL_DRYWT",
                       "BARK_SPGR_GREENVOL_DRYWT"))
names(ref) <- c("SPCD", "GENUS", "JENKINS_SPGRPCD",
                "SFTWD_HRDWD",
                "WDSG", "BKSG")

fiadb <- merge(x= fiadb,
               y= ref,
               by= c("SPCD", "GENUS"))

# no trees with missing heights and no woodland species
fiadb <- fiadb[fiadb$JENKINS_SPGRPCD < 10 & !is.na(fiadb$HT), ]

# use field cull for cull
fiadb$CULL <- fiadb$CULL_FLD
fiadb[is.na(fiadb$CULL), "CULL"] <- 0

# need several updated coefficient/parameter/ratio files

# these are actually mean
med_cr_prop <- read.csv("Decay_and_Dead/nsvb/median_crprop.csv")
med_cr_prop$SFTWD_HRDWD <- ifelse(med_cr_prop$hwd_yn == 'N', 'S', 'H')

decay_codes <- read.csv("Decay_and_Dead/nsvb/decay_codes.csv")
names(decay_codes) <- c("spgrp", "decaycd",
                        "DECAY_WD", "DECAY_BK", "DECAY_BR")

# only using live trees for this
fiadb[fiadb$STATUSCD == 1, 'DECAYCD'] <- 0
fiadb[fiadb$STATUSCD == 1, 'STANDING_DEAD_CD'] <- 0
fiadb[is.na(fiadb$ACTUALHT), 'ACTUALHT'] <- fiadb[is.na(fiadb$ACTUALHT), 'HT']

fiadb <- merge(x= fiadb,
              y= decay_codes,
              by.x= c("SFTWD_HRDWD", "DECAYCD"),
              by.y= c("spgrp", "decaycd"))

# add cull density reduction factors
decay_ratio_hw <- decay_codes[decay_codes$spgrp == 'H' &
                                decay_codes$decaycd == 3,]$DECAY_WD
decay_ratio_sw <- decay_codes[decay_codes$spgrp == 'S' &
                                decay_codes$decaycd == 3,]$DECAY_WD

fiadb$CULL_DECAY_RATIO <- 1
fiadb[fiadb$STATUSCD == 1 & fiadb$SFTWD_HRDWD == 'H', "CULL_DECAY_RATIO"] <- decay_ratio_hw
fiadb[fiadb$STATUSCD == 1 & fiadb$SFTWD_HRDWD == 'S', "CULL_DECAY_RATIO"] <- decay_ratio_sw

source("R/VT_CRM2_ApplyFuns.R")

fiadb <- merge(x= fiadb,
              y= med_cr_prop[,c("Province", "SFTWD_HRDWD", "CRmn")],
              by.x= c("ECODIVISION", "SFTWD_HRDWD"),
              by.y= c("Province", "SFTWD_HRDWD"),
              all.x= TRUE)

miss_sft <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED',]$CRmn[1]
miss_hwd <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED',]$CRmn[2]

fiadb[is.na(fiadb$CRmn) & fiadb$SFTWD_HRDWD == 'S','CRmn'] <- miss_sft
fiadb[is.na(fiadb$CRmn) & fiadb$SFTWD_HRDWD == 'H','CRmn'] <- miss_hwd

fiadb$BROKEN_TOP <- !(fiadb$HT == fiadb$ACTUALHT)

# carbon stuff
cfrac <- read.csv("Decay_and_Dead/nsvb/fia_wood_c_frac.csv", as.is= TRUE)
cfrac <- cfrac[,c("SPCD", "fia.wood.c")]
names(cfrac)[2] <- "C_FRAC"

cfrac_fia <- read.csv("Decay_and_Dead/nsvb/carbon_ratio.csv")

cfrac_fia <- cfrac_fia[cfrac_fia$spcd %in% cfrac$SPCD,]

cfrac <- merge(x= cfrac,
               y= cfrac_fia,
               by.x= c("SPCD"),
               by.y= c("spcd"))

cfrac$fia.wood.c <- cfrac$CARBON_RATIO * 100

cfrac_dead <- read.csv("Decay_and_Dead/nsvb/dead_carbon.csv", as.is= TRUE)

fiadb <- merge(x= fiadb,
              y= cfrac,
              by.x= "SPCD",
              by.y= "SPCD",
              all.x= TRUE)

fiadb <- merge(x= fiadb,
              y= cfrac_dead,
              by.x= c("SFTWD_HRDWD", "DECAYCD"),
              by.y= c("spgrp", "decaycd"),
              all.x= TRUE)

fiadb$fia.wood.c <- ifelse(fiadb$STANDING_DEAD_CD == 1,
                          fiadb$cfrac,
                          fiadb$fia.wood.c)

# fia <- fia[fia$STATECD == 48, ]

fiadb[is.na(fiadb$CR) & fiadb$STATUSCD == 1,]$CR <- 0

fiadb[is.na(fiadb$STDORGCD), "STDORGCD"] <- 0
fiadb$SPCD <- ifelse(fiadb$SPCD %in% c(111, 131) & fiadb$STDORGCD == 1,
                    paste0("1_", fiadb$SPCD),
                    fiadb$SPCD)

fiadb[is.na(fiadb$CULL),'CULL'] <- 0

fiadb$SPCD_DIVISION <- paste(fiadb$SPCD, fiadb$DIVISION)

# equation numbers and forms
forms <- read.csv(file.path("Files/equation_forms_and_calls.csv"))
add_me <- data.frame(equation= c(3.1, 6.1),
                     rhs= c("<- a * DBH^b * THT^c * WDSG",
                            "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta"))

forms <- rbind(forms, add_me)

forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)

source("Decay_and_Dead/R/nsvb_update_funcs.R")

# apply over fiadb
# data= fiadb[fiadb$STATE_NAME == 'Arkansas',]
# coef_dir= "Coefs/combined/"
# var_names= c(DBH= "DIA", THT= "HT", CULL= "CULL")
# gross.volume= FALSE
# all.vars= TRUE
fiadb2 <- predictCRM2(fiadb,
                      coef_dir= "Coefs/combined/",
                      var_names= c(DBH= "DIA", THT= "HT", CULL= "CULL"),
                      gross.volume= FALSE,
                      all.vars= TRUE)

fiadb2[is.na(fiadb2$VOLCSSND), c("VSAWIB", "VSAWOB", "VSAWBK")] <- NA

# temp file
data.table::fwrite(fiadb2,
                   file.path(fiadb_path, "fiadb_vteco_w_dead_and_loss_deductions.csv"))

# if(save_predictions) {
#
#   file.copy("Output/fiadb_inventory_w_pred.csv",
#             file.path(fiadb_out_path, "fiadb_vteco.csv"),
#             #"~/Documents/Work/R_Data/fiadb_umaine.csv",
#             overwrite= TRUE)
#
# }
