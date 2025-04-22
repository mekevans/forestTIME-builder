tpa_rules <- readr::read_csv(
  "data-raw/DESIGNCD_TPA.csv",
  show_col_types = FALSE
)

REF_SPECIES <- readr::read_csv(
  "data-raw/REF_SPECIES.csv",
  show_col_types = FALSE
)

REF_TREE_DECAY_PROP <- readr::read_csv(
  "data-raw/REF_TREE_DECAY_PROP.csv",
  show_col_types = FALSE
)

REF_TREE_CARBON_RATIO_DEAD <- readr::read_csv(
  "data-raw/REF_TREE_CARBON_RATIO_DEAD.csv",
  show_col_types = FALSE
)

#originally from carbon_code/Decay_and_Dead/nsvb/median_crprop.csv
median_crprop_csv <- readr::read_csv(
  "data-raw/median_crprop.csv",
  show_col_types = FALSE
)

equation_forms_and_calls_csv <- read.csv(
  "data-raw/equation_forms_and_calls.csv",
)

coef_files <- fs::dir_ls("data-raw/coef_files/combined", regexp = "_coefs.csv")

all_coefs <- lapply(
  coef_files,
  function(x) read.csv(x, as.is = TRUE)
)
names(all_coefs) <- gsub("_coefs.csv", "", fs::path_file(coef_files))


usethis::use_data(
  tpa_rules,
  REF_SPECIES,
  REF_TREE_DECAY_PROP,
  REF_TREE_CARBON_RATIO_DEAD,
  median_crprop_csv,
  equation_forms_and_calls_csv,
  all_coefs,
  internal = TRUE,
  overwrite = TRUE
)
