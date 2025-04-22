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

usethis::use_data(
  tpa_rules,
  REF_SPECIES,
  REF_TREE_DECAY_PROP,
  REF_TREE_CARBON_RATIO_DEAD,
  internal = TRUE,
  overwrite = TRUE
)
