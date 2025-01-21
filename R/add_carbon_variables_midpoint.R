# apply CRM2 to standing inventory dataset
# example script

# setwd("~/RData/nsvb_test/nsvb_test/")

# prep work--------------------------------------------------------------------
# test data built from the 'query.sql' script
# trees from one plot
fiadb <- tbl(con, "trees_annual_measures_midpoint_nsvb") |>
  filter(JENKINS_SPGRPCD < 10, !is.na(HT)) |>
  mutate(CR = as.numeric(CR),
         HT = as.numeric(HT),
         DIA = as.numeric(DIA),
         ACTUALHT = as.numeric(ACTUALHT)) |>
  collect()

# these are actually mean
# this table is in fiadb as fs_fia_reference.ref_tree_stnd_dead_cr_prop
# but you have to merge on a derived column 'ecoprov/province'
# i find it easier to do in R
med_cr_prop <- read.csv(here::here("carbon_code", "Decay_and_Dead", "nsvb", "median_crprop.csv"))

med_cr_prop$SFTWD_HRDWD <- ifelse(med_cr_prop$hwd_yn == 'N', 'S', 'H')

# various helper functions
# the main nsvb functions 'applyAllLevels' and 'predictCRM2'
# are overwritten when loading "Decay_and_Dead/R/nsvb_update_funcs.R"
source(here::here("carbon_code", "R", "VT_CRM2_ApplyFuns.R"))

fiadb$PROVINCE <- getDivision(fiadb$ECOSUBCD, TRUE)
fiadb$DIVISION <- getDivision(fiadb$ECOSUBCD)

fiadb <- merge(
  x = fiadb,
  y = med_cr_prop[, c("Province", "SFTWD_HRDWD", "CRmn")],
  by.x = c("PROVINCE", "SFTWD_HRDWD"),
  by.y = c("Province", "SFTWD_HRDWD"),
  all.x = TRUE
)

miss_sft <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED', ]$CRmn[1]
miss_hwd <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED', ]$CRmn[2]

fiadb[is.na(fiadb$CRmn) &
        fiadb$SFTWD_HRDWD == 'S', 'CRmn'] <- miss_sft
fiadb[is.na(fiadb$CRmn) &
        fiadb$SFTWD_HRDWD == 'H', 'CRmn'] <- miss_hwd

fiadb$BROKEN_TOP <- !(fiadb$HT == fiadb$ACTUALHT)

fiadb[is.na(fiadb$CR) & fiadb$STATUSCD == 1, 'CR'] <- 0

# planted loblolly/slash use separate equations
fiadb[is.na(fiadb$STDORGCD), "STDORGCD"] <- 0
fiadb$SPCD <- ifelse(fiadb$SPCD %in% c(111, 131) &
                       fiadb$STDORGCD == 1,
                     paste0("1_", fiadb$SPCD),
                     fiadb$SPCD)

fiadb[is.na(fiadb$CULL), 'CULL'] <- 0

# finest level of model application
fiadb$SPCD_DIVISION <- paste(fiadb$SPCD, fiadb$DIVISION)

# equation numbers and forms are stored in ref file
forms <- read.csv(here::here("carbon_code",
                             "Files", 
                             "equation_forms_and_calls.csv"))
add_me <- data.frame(
  equation = c(3.1, 6.1),
  rhs = c("<- a * DBH^b * THT^c * WDSG", "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta")
)

forms <- rbind(forms, add_me)

forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)

# contains updates for decay and dead trees
source(here::here("carbon_code", 
                  "Decay_and_Dead", 
                  "R",
                  "nsvb_update_funcs.R"))

# apply over fiadb
# predictCRM2 is located in 'Decay_and_Dead/R/nsvb_update_funcs.R'
fiadb2 <- predictCRM2(
  data = fiadb,
  # directory where the coefficient files are
  coef_dir = here::here("carbon_code", 
  "Coefs", 
  "combined"),
  # what are the variable names for dbh/total height/cull
  # should probably update this for c_frac, actual_ht, etc
  var_names = c(DBH = "DIA", THT = "HT", CULL = "CULL"),
  gross.volume = FALSE,
  all.vars = TRUE
)

copy_to(
  dest = con,
  df = fiadb2,
  name = "tree_carbon_annualized_midpoint",
  temporary = FALSE
)

