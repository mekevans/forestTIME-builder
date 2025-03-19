source(here::here("R/getDivision.R"))
source(here::here("R/predictCRM2.R"))

#' Estimate carbon
#' 
#' Estimates carbon using code provided by David Walker
#' 
#' 
#' @param data prepped data produced by [prep_carbon()].
#' @param carbon_dir directory where some files are
#' 
#' @references TODO: add ref to the paper this code is from
estimate_carbon <- function(data, carbon_dir = "carbon_code") {
  
  med_cr_prop <- 
    readr::read_csv(here::here(carbon_dir, "Decay_and_Dead/nsvb/median_crprop.csv")) |> 
    mutate(SFTWD_HRDWD = if_else(hwd_yn == 'N', 'S', 'H'))
  
  #seems like should go in prep_carbon() maybe?
  data_midpt_prepped <- 
    data_midpt_prepped |>
    mutate(PROVINCE = getDivision(ECOSUBCD, TRUE),
           DIVISION = getDivision(ECOSUBCD)) |> 
    #this is only necessary because this code uses [] for indexing instead of `filter()`
    mutate(CULL = ifelse(is.na(CULL), 0, CULL))
  
  fiadb <-
    data_midpt_prepped |>
    left_join(
      med_cr_prop |> select(PROVINCE = Province, SFTWD_HRDWD, CRmn),
      by = join_by(SFTWD_HRDWD, PROVINCE)
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
  
  #The `forms` object is assumed to be in the parent environment by
  #applyAllLevels() I think
  
  # equation numbers and forms are stored in ref file
  forms <- read.csv(here::here(carbon_dir,
                               "Files", 
                               "equation_forms_and_calls.csv"))
  add_me <- data.frame(
    equation = c(3.1, 6.1),
    rhs = c("<- a * DBH^b * THT^c * WDSG", "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta")
  )
  
  forms <- rbind(forms, add_me)
  
  forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
  forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)
  
  # apply over fiadb
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
  
  #TODO select only columns needed!!
  #return
  fiadb2
  
}