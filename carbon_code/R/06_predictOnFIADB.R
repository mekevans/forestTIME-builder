# apply CRM2 to standing inventory dataset

# library(data.table)

# setwd("~/Documents/Work/R_Data/CRM2/")

# prep work
fiadb <- fread(file.path(fiadb_path, "fia_tree_agb_2017.csv"))
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
                       "WOOD_SPGR_GREENVOL_DRYWT",
                       "BARK_SPGR_GREENVOL_DRYWT"))
names(ref) <- c("SPCD", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

fiadb <- merge(x= fiadb,
               y= ref,
               by= c("SPCD", "GENUS"))

# make planted pine
fiadb$SPCD <- ifelse(fiadb$SPCD %in% c(111, 131) & fiadb$STDORGCD == 1,
                     paste0("1_", fiadb$SPCD),
                     fiadb$SPCD)

# no trees with missing heights and no woodland species
fiadb <- fiadb[fiadb$JENKINS_SPGRPCD < 10 & !is.na(fiadb$HT), ]

# get estimated cull
fiadb$CULL <- round(100 - (fiadb$VOLCFSND / fiadb$VOLCFGRS * 100), 0)
fiadb[is.na(fiadb$CULL), "CULL"] <- 0

# equation numbers and forms
forms <- read.csv("Files/equation_forms_and_calls.csv", as.is= TRUE)
add_me <- data.frame(equation= 3.1,
                     rhs= "<- a * DBH^b * THT^c * WDSG")

forms <- rbind(forms, add_me)

# load application funs
source("R/VT_CRM2_ApplyFuns.R")

fiadb <- makeVars(fiadb, "SPCD")

fiadb[fiadb$STATECD == 72, ]$JENKINS_SPGRPCD <- 11
fiadb[fiadb$STATECD == 78, ]$JENKINS_SPGRPCD <- 11

# apply over fiadb
fiadb2 <- predictCRM2(fiadb,
                      coef_dir= "Coefs/combined/",
                      var_names= c(DBH= "DIA", THT= "HT", CULL= "CULL"),
                      gross.volume= TRUE,
                      all.vars= FALSE)

fiadb2[is.na(fiadb2$VOLCSSND), c("VSAWIB", "VSAWOB", "VSAWBK")] <- NA

# temp file
data.table::fwrite(fiadb2, "Output/fiadb_inventory_w_pred.csv")

if(save_predictions) {

  file.copy("Output/fiadb_inventory_w_pred.csv",
            file.path(fiadb_out_path, "fiadb_vteco.csv"),
            #"~/Documents/Work/R_Data/fiadb_umaine.csv",
            overwrite= TRUE)

}
