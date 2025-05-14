# apply CRM2 to nsbe

# library(data.table)

# need two file paths - one for nsbe tech transfer repo
# and one for nsbe biomass/volume dataset repo

# setwd("~/Documents/Work/R_Data/nsbe_tech_transfer/VT/")

fp_nsbe_bio <- nsbe_repo_path

# prep work--------------------------------------------------------------------
nsbe <- fread(file.path(fp_nsbe_bio, "/Output/NSBE_FORMAT/NSBE_TREE.csv.gz"))

# need to distinguish planted from natural loblolly/slash
ref <- fread("../Shared/Files/REF_SPECIES.csv",
             select= c("SPCD", "GENUS", "JENKINS_SPGRPCD",
                       "WOOD_SPGR_GREENVOL_DRYWT",
                       "BARK_SPGR_GREENVOL_DRYWT"))
names(ref) <- c("SPCD", "GENUS", "JENKINS_SPGRPCD", "WDSG", "BKSG")

nsbe <- merge(x= nsbe,
              y= ref,
              by= c("SPCD"))

# ecodivisions
ecodiv <- read.csv(file.path(nsbe_repo_path,
                             "Work/Shared/CRM2/Coefs/NSBE_Ecodivisions.csv"),
                   as.is= TRUE)

nsbe <- merge(x= nsbe,
              y= ecodiv,
              by= c("AUTHOR", "LOC"))

# make planted loblolly, slash own group
# make same as FIADB: 0= natural, 1= planted
nsbe$STDORGCD <- ifelse(nsbe$TR_ORIGIN > 1, 1, 0)
# if missing make natural
nsbe[is.na(nsbe$STDORGCD), ]$STDORGCD <- 0

nsbe$SPCD2 <- ifelse(nsbe$SPCD %in% c(111, 131) & nsbe$STDORGCD == 1,
                     paste0("1_", nsbe$SPCD),
                     nsbe$SPCD)

nsbe$SPCD <- nsbe$SPCD2

# no trees with missing heights and no woodland species
nsbe <- nsbe[nsbe$JENKINS_SPGRPCD < 10 & !is.na(nsbe$HT_TOT), ]

nsbe <- nsbe[nsbe$DO_BH >= 1.0, ]

# no large redwoods
# No large redwoods
nsbe <- nsbe[!(nsbe$AUTHOR %in% c("Sillett2010", "Stillett_et_al")), ]

# no cull in nsbe
nsbe$CULL <- 0

# equation numbers and forms
forms <- read.csv("Files/equation_forms_and_calls.csv", as.is= TRUE)
add_me <- data.frame(equation= 3.1,
                     rhs= "<- a * DBH^b * THT^c * WDSG")

forms <- rbind(forms, add_me)

# load all helper funs
source("R/VT_CRM2_ApplyFuns.R")

# locations
locs <- fread(file.path(nsbe_repo_path, "/Output/NSBE_FORMAT/NSBE_LOCS.csv.gz"))
locs <- locs[,c("AUTHOR", "LOC", "STATE")]

nsbe <- makeVars(nsbe, "SPCD")

nsbe$JENKINS_SPGRPCD <- ifelse(grepl('PR_', nsbe$AUTHOR), 11, nsbe$JENKINS_SPGRPCD)

# apply to nsbe dataset--------------------------------------------------------
nsbe2 <- predictCRM2(nsbe,
                     coef_dir= "Coefs/combined/",
                     var_names= c(DBH= "DO_BH", THT= "HT_TOT", CULL= "CULL"))
#
# make paired for scorecard----------------------------------------------------
source("../Shared/R/sharedFuns.R")

test_wide <- makePaired(nsbe2)

test_wide <- merge(x= test_wide,
                   y= locs,
                   by= c("AUTHOR", "LOC"))

source("../Shared/R/makeScorecard.R")

# write.csv(test_wide, "Output/nsbe_tree_w_pred_long.csv", row.names= FALSE)

if (save_predictions) {
  
  fwrite(test_wide, file.path(pred_errors_path, "prediction_errors_VT_ECO.csv"))
  
}

ggplot(test_wide, aes(x= OBSR, y= PRED)) +
  geom_point() +
  geom_abline(size= 1, colour= "red", linetype= 5) +
  geom_smooth(method= lm, se= FALSE, colour= "blue", formula= y ~ x) +
  facet_wrap(~VAR3, scales= "free")
