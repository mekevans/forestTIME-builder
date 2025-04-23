#' Read in and join all required tables
#'
#' Reads in all the tables needed for carbon estimation and population scaling
#' and joins them into a single table.  Also creates unique tree an plot
#' identifiers (`tree_ID` and `plot_ID`, respectively).  Removes "problem" trees
#' such as those with only records when they are dead and trees that switch
#' species (`SPCD`). Fills in missing values for `ACTUALHT` with values from
#' `HT` to prepare for interpolation.
#'
#' @param db a list of tables produced by [read_fia()]
#' @export
#' @returns a tibble
prep_data <- function(db) {
  # Select only the columns we need from each table, to keep things slim
  cli::cli_progress_step("Wrangling data")
  PLOTGEOM <-
    db$PLOTGEOM |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::select(CN, INVYR, ECOSUBCD)

  PLOT <-
    db$PLOT |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(
      plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      .before = 1
    ) |>
    dplyr::select(
      plot_ID,
      CN,
      INVYR,
      DESIGNCD, #for joining TPA_UNADJ based on rules later
      # MACRO_BREAKPOINT_DIA #unclear if this is really needed
    )

  COND <-
    db$COND |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(
      plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      .before = 1
    ) |>
    dplyr::select(
      plot_ID,
      PLT_CN,
      INVYR,
      CONDID,
      CONDPROP_UNADJ,
      PROP_BASIS,
      COND_STATUS_CD,
      STDORGCD
    )

  TREE <-
    db$TREE |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(
      plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
      tree_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, sep = "_"),
      .before = 1
    ) |>
    dplyr::select(
      plot_ID,
      tree_ID,
      INVYR,
      PLT_CN,
      CONDID,
      MORTYR,
      STATUSCD,
      RECONCILECD,
      DECAYCD,
      STANDING_DEAD_CD,
      DIA,
      CR,
      HT,
      ACTUALHT,
      CULL,
      SPCD
    )

  POP_ESTN_UNIT <-
    db$POP_ESTN_UNIT |>
    dplyr::select(CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)

  POP_EVAL <-
    db$POP_EVAL |>
    dplyr::select(
      EVALID,
      EVAL_GRP_CN,
      ESTN_METHOD,
      CN,
      END_INVYR,
      REPORT_YEAR_NM
    )

  POP_EVAL_TYP <-
    db$POP_EVAL_TYP |>
    dplyr::select(EVAL_TYP, EVAL_CN)

  POP_PLOT_STRATUM_ASSGN <-
    db$POP_PLOT_STRATUM_ASSGN |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::select(STRATUM_CN, PLT_CN, INVYR)

  POP_STRATUM <-
    db$POP_STRATUM |>
    dplyr::select(
      ESTN_UNIT_CN,
      EXPNS,
      P2POINTCNT,
      ADJ_FACTOR_MICR,
      ADJ_FACTOR_SUBP,
      ADJ_FACTOR_MACR,
      CN,
      P1POINTCNT
    )

  # Join the tables
  data <-
    TREE |> #13,963
    dplyr::as_tibble() |>
    dplyr::left_join(PLOT, by = dplyr::join_by(plot_ID, INVYR)) |>
    dplyr::left_join(PLOTGEOM, by = dplyr::join_by(INVYR, CN)) |>
    dplyr::left_join(COND, by = dplyr::join_by(plot_ID, INVYR, PLT_CN, CONDID))

  #TODO These population tables are needed for pop scaling, but the 'many-to-many' relationship messes up the interpolation.  I think this is because plots can belong to multiple strata? Probably can't use this with interpolated data anyways?

  # left_join(POP_PLOT_STRATUM_ASSGN, by = join_by(INVYR, PLT_CN), relationship = 'many-to-many') %>% #many-to-many relationship?
  # left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
  # left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
  # left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
  # left_join(POP_EVAL_TYP, by = 'EVAL_CN', relationship = 'many-to-many') |>

  #remove trees that have only 0 or 1 non-NA measurment (we can't interpolate these)
  data <- data |>
    dplyr::group_by(tree_ID) |>
    dplyr::filter(
      sum(!is.na(DIA)) > 1 & sum(!is.na(HT)) > 1
    ) |>
    #remove trees that change species
    dplyr::filter(length(unique(SPCD)) == 1) |>

    #remove trees that were measured in error (https://github.com/mekevans/forestTIME-builder/issues/59#issuecomment-2758575994)
    dplyr::filter(!any(RECONCILECD %in% c(7, 8))) |>
    dplyr::ungroup() |>
    #coalesce ACTUALHT so it can be interpolated
    dplyr::mutate(ACTUALHT = dplyr::coalesce(ACTUALHT, HT)) |>
    dplyr::select(ACTUALHT, HT, dplyr::everything())
  #return:
  data
}
