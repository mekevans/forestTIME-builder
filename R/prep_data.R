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
#' @seealso [add_composite_ids()]
#' @returns a tibble
prep_data <- function(db) {
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
    add_composite_ids() |>
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
    add_composite_ids() |>
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
    add_composite_ids() |>
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

#' Add composite ID columns to data
#'
#' Creates a `tree_ID` and/or a `plot_ID` column that contain unique tree and
#' plot identifiers, respectively.  These are created by pasting together the
#' values for `STATECD`, `UNITCD`, `COUNTYCD`, `PLOT` and in the case of trees
#' `SUBP` and `TREE`.
#'
#' @param data A tibble or data frame with at least the `STATECD`, `UNITCD`,
#'   `COUNTYCD` and `PLOT` columns
#'
#' @seealso See [split_composite_ids()] for "undoing" this.
#' @returns The input tibble with a `plot_ID` and possibly also a `tree_ID`
#'   column added
#' @export
add_composite_ids <- function(data) {
  cols <- colnames(data)
  if (
    all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE") %in% cols)
  ) {
    data <-
      data |>
      dplyr::mutate(
        plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
        tree_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, sep = "_"),
        .before = 1
      )
  } else if (all(c("STATECD", "UNITCD", "COUNTYCD", "PLOT") %in% cols)) {
    data <-
      data |>
      dplyr::mutate(
        plot_ID = paste(STATECD, UNITCD, COUNTYCD, PLOT, sep = "_"),
        .before = 1
      )
  } else {
    stop("Not all required columns are present")
  }
  data
}

#' Split composite ID columns
#'
#' Splits the composite ID columns `tree_ID` and/or `plot_ID` into their
#' original component columns
#'
#' @param data A tibble with the `tree_ID` and/or `plot_ID` columns
#' @returns The input tibble with additional columns `STATECD`, `UNITCD`,
#'   `COUNTYCD`, `PLOT` and possibly `SUBP` and `TREE`.
#' @seealso [add_composite_ids()]
#' @export
split_composite_ids <- function(data) {
  cols <- colnames(data)
  if (!any(c("plot_ID", "tree_ID") %in% cols)) {
    stop("No composite ID columns found")
  }

  # tree_ID contains all the information in plot_ID, so if tree_ID exists, it's
  # enough to just split that one
  if ("tree_ID" %in% cols) {
    data <- data |>
      tidyr::separate_wider_delim(
        tree_ID,
        delim = "_",
        names = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "SUBP", "TREE"),
        cols_remove = FALSE
      )
    return(data)
  }

  if ("plot_ID" %in% cols) {
    data <- data |>
      tidyr::separate_wider_delim(
        plot_ID,
        delim = "_",
        names = c("STATECD", "UNITCD", "COUNTYCD", "PLOT"),
        cols_remove = FALSE
      )
    return(data)
  }
}
