#' Read in and join all required tables
#'
#' Reads in all the tables needed for carbon estimation and population scaling
#' and joins them into a single table. Then, some additional data cleaning steps
#' are performed.
#' 1. Creates unique tree and plot identifiers (`tree_ID` and `plot_ID`,
#'   respectively).
#' 2. Fills in missing values for `ACTUALHT` with values from `HT` to prepare
#'   for interpolation.
#' 3. Overwrites `SPCD` with whatever the last value of `SPCD` is for each tree
#'   (to handle trees that change `SPCD`).
#' 4. Fills a tree's `MORTYR` column so every row contains the recorded
#'   mortality year.
#'
#' @param db a list of tables produced by [fia_load()]
#' @export
#' @seealso [fia_add_composite_ids()]
#' @returns a tibble
fia_tidy <- function(db) {
  # Select only the columns we need from each table, to keep things slim
  cli::cli_progress_step("Wrangling data")
  PLOTGEOM <-
    db$PLOTGEOM |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(CN = as.character(CN)) |> 
    dplyr::select(PLT_CN = CN, INVYR, ECOSUBCD)

  PLOT <-
    db$PLOT |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(CN = as.character(CN)) |> 
    fia_add_composite_ids() |>
    dplyr::select(
      plot_ID,
      PLT_CN = CN,
      INVYR,
      DESIGNCD, #for joining TPA_UNADJ based on rules later
      # MACRO_BREAKPOINT_DIA #unclear if this is really needed
      INTENSITY
    )

  COND <-
    db$COND |>
    dplyr::filter(INVYR >= 2000L) |>
    dplyr::mutate(PLT_CN = as.character(PLT_CN)) |> 
    fia_add_composite_ids() |>
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
    dplyr::mutate(PLT_CN = as.character(PLT_CN)) |> 
    fia_add_composite_ids() |>
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

  # Join the tables
  data <-
    PLOT |>
    dplyr::as_tibble() |>
    dplyr::left_join(TREE, by = dplyr::join_by(plot_ID, PLT_CN, INVYR)) |>
    dplyr::left_join(PLOTGEOM, by = dplyr::join_by(INVYR, PLT_CN)) |>
    dplyr::left_join(COND, by = dplyr::join_by(plot_ID, INVYR, PLT_CN, CONDID))


  # use only base intensity plots 
  # data <- data |>
  #   dplyr::filter(INTENSITY == 1)

  # fill MORTYR so it is a property of trees
  data <- data |>
    dplyr::group_by(tree_ID) |>
    tidyr::fill(MORTYR, .direction = c("updown")) |>
    # if trees have more than one SPCD, set all to be the most recent SPCD
    # (https://github.com/mekevans/forestTIME-builder/issues/53)
    dplyr::mutate(SPCD = dplyr::last(SPCD)) |>
    dplyr::ungroup()

  # at this point, get the list of plots and years as following steps may remove
  # "empty" plots
  all_plots <- data |>
    dplyr::select(plot_ID, INVYR) |>
    dplyr::distinct() |>
    dplyr::left_join(PLOT, by = dplyr::join_by(plot_ID, INVYR))

  # coalesce ACTUALHT so it can be interpolated
  data <- data |>
    dplyr::mutate(ACTUALHT = dplyr::coalesce(ACTUALHT, HT))

  #   # deal with "problem" trees
    # data <- data |>
    #   dplyr::group_by(tree_ID) |>
    #   dplyr::filter(
    #     sum(!is.na(DIA)) > 1 & sum(!is.na(HT)) > 1
    #   ) |>
    #   # remove trees that have always been fallen and have no measurements
    #   dplyr::filter(
    #     !(sum(is.finite(DIA) & is.finite(HT)) == 0 & all(STANDING_DEAD_CD == 0))
    #   ) |>
    #   # remove trees that were measured in error
    #   # (https://github.com/mekevans/forestTIME-builder/issues/59#issuecomment-2758575994)
    #   dplyr::filter(!any(RECONCILECD %in% c(7, 8))) |>
    #   dplyr::ungroup() |>

    # join the empty plots back in
    data <-
      dplyr::full_join(
        data,
        all_plots,
        by = dplyr::join_by(plot_ID, PLT_CN, INVYR, DESIGNCD, INTENSITY)
      ) |>
      dplyr::arrange(plot_ID, tree_ID, INVYR) |>
      dplyr::select(plot_ID, tree_ID, INVYR, everything())

  # return:
  data
}
