#' Estimate carbon
#'
#' Estimates carbon using code provided by David Walker
#'
#'
#' @param data prepped data produced by [prep_carbon()].
#'
#' @references TODO: add ref to the paper this code is from
#' @export
#' @returns a tibble
estimate_carbon <- function(data) {
  med_cr_prop <-
    median_crprop_csv |>
    dplyr::mutate(SFTWD_HRDWD = dplyr::if_else(hwd_yn == 'N', 'S', 'H'))

  #seems like should go in prep_carbon() maybe?
  data_prepped <-
    data |>
    dplyr::mutate(
      PROVINCE = getDivision(ECOSUBCD, TRUE),
      DIVISION = getDivision(ECOSUBCD)
    ) |>
    # no trees with missing heights and no woodland species
    dplyr::filter(JENKINS_SPGRPCD < 10, !is.na(HT)) |>
    #this is only necessary because this code uses [] for indexing instead of `filter()`
    dplyr::mutate(
      dplyr::across(
        c(DECAYCD, STANDING_DEAD_CD),
        \(x) dplyr::if_else(STATUSCD == 1, 0, x)
      ),
      CULL = ifelse(is.na(CULL), 0, CULL)
    )

  fiadb <-
    data_prepped |>
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

  fiadb[is.na(fiadb$CR) & fiadb$STATUSCD == 1, 'CR'] <- 0

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
    #select only columns needed
    dplyr::select(
      any_of(c(
        "tree_ID",
        "plot_ID",
        "YEAR",
        "interpolated",
        "DIA",
        "HT",
        "ACTUALHT",
        "CR",
        "CULL",
        "PLT_CN",
        "CONDID",
        "COND_STATUS_CD",
        "PROP_BASIS",
        "CONDPROP_UNADJ",
        "STATUSCD",
        "RECONCILECD", #might be done with this column?
        "DECAYCD",
        "STANDING_DEAD_CD",
        "SPCD",
        # DESIGNCD, #only needed this to get TPA_UNADJ
        "TPA_UNADJ",
        # DRYBIO_AG = AGB, #Includes foliage, which is not part of DRYBIO_AG
        "DRYBIO_AG" = "BIOMASS", #Does not include foliage
        "CARBON_AG" = "CARBON"
      ))
    )

  #TODO: undo changing of NAs to 0s

  #return
  fiadb2
}
