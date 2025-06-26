#' Estimate carbon
#'
#' Estimates carbon using code provided by David Walker with slight
#' modifications
#'
#' @param data_prepped tibble produced by [prep_carbon()]. 
#' @author David Walker
#' @noRd
#' @returns a tibble
estimate_carbon <- function(data_prepped) {
  med_cr_prop <-
    median_crprop_csv |>
    dplyr::mutate(SFTWD_HRDWD = dplyr::if_else(hwd_yn == 'N', 'S', 'H'))

  fiadb <-
    data_prepped |>
    # Can't estimate carbon for trees with missing heights or woodland species woodland species
    dplyr::filter(JENKINS_SPGRPCD < 10, !is.na(HT)) |>
    dplyr::left_join(
      med_cr_prop |> dplyr::select(PROVINCE = Province, SFTWD_HRDWD, CRmn),
      by = dplyr::join_by(SFTWD_HRDWD, PROVINCE)
    )

  miss_sft <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED', ]$CRmn[1]
  miss_hwd <- med_cr_prop[med_cr_prop$Province == 'UNDEFINED', ]$CRmn[2]

  fiadb[
    is.na(fiadb$CRmn) &
      fiadb$SFTWD_HRDWD == 'S',
    'CRmn'
  ] <- miss_sft
  fiadb[
    is.na(fiadb$CRmn) &
      fiadb$SFTWD_HRDWD == 'H',
    'CRmn'
  ] <- miss_hwd

  fiadb$BROKEN_TOP <- !(fiadb$HT == fiadb$ACTUALHT)

  # Assumes un-recorded CR for alive trees is 0

  # original code doesn't work with NAs for STATUSCD as is the case with plots with no trees
  # fiadb[is.na(fiadb$CR) & fiadb$STATUSCD == 1, 'CR'] <- 0
  fiadb <- dplyr::mutate(fiadb, CR = dplyr::if_else(is.na(CR) & STATUSCD == 1, 0, CR))

  # planted loblolly/slash use separate equations
  fiadb[is.na(fiadb$STDORGCD), "STDORGCD"] <- 0
  fiadb$SPCD <- ifelse(
    fiadb$SPCD %in% c(111, 131) & fiadb$STDORGCD == 1,
    paste0("1_", fiadb$SPCD),
    fiadb$SPCD
  )

  fiadb[is.na(fiadb$CULL), 'CULL'] <- 0

  # finest level of model application
  fiadb$SPCD_DIVISION <- paste(fiadb$SPCD, fiadb$DIVISION)

  #The `forms` object is assumed to be in the parent environment by
  #applyAllLevels() I think

  # equation numbers and forms are stored in ref file
  forms <- equation_forms_and_calls_csv
  add_me <- data.frame(
    equation = c(3.1, 6.1),
    rhs = c(
      "<- a * DBH^b * THT^c * WDSG",
      "<- (1 - (1 - ACTUALHT / THT)^alpha)^beta"
    )
  )

  forms <- rbind(forms, add_me)

  forms$rhs <- gsub('VTOTIB', 'VTOTIB_GROSS', forms$rhs)
  forms$rhs <- gsub('VTOTOB', 'VTOTOB_GROSS', forms$rhs)

  # apply over fiadb
  fiadb2 <- predictCRM2(
    data = fiadb,
    # # directory where the coefficient files are
    forms = forms,
    # what are the variable names for dbh/total height/cull
    # should probably update this for c_frac, actual_ht, etc
    var_names = c(DBH = "DIA", THT = "HT", CULL = "CULL"),
    gross.volume = FALSE,
    all.vars = TRUE
  ) |>
    dplyr::as_tibble() |>
    # I don't trust predictCRM2() to have not modified columns in weird ways
    # just to satisfy prerequesites of calculations. Therefore, I'm only going
    # to keep certain columns from the output and join them into the input data.
    dplyr::select(
      any_of(c(
        "tree_ID",
        "plot_ID",
        "YEAR",
        # DRYBIO_AG = AGB, #Includes foliage, which is not part of DRYBIO_AG
        "DRYBIO_AG" = "BIOMASS", #Does not include foliage
        "CARBON_AG" = "CARBON"
      ))
    )

  #return
  left_join(data_prepped, fiadb2, by = join_by(plot_ID, tree_ID, YEAR))
}
