expand_data <- function(data) {
  #We do the expand() in chunks because it is computationally expensive otherwise
  plot_chunks <-
    data |>
    dplyr::ungroup() |>
    dplyr::select(plot_ID) |>
    dplyr::distinct() |>
    dplyr::mutate(plot_chunk = ntile(plot_ID, n = 10))

  all_yrs <-
    dplyr::left_join(data, plot_chunks, by = join_by(plot_ID)) |>
    dplyr::group_by(plot_chunk) |>
    dplyr::group_split() |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(tree_ID) |>
        tidyr::expand(YEAR = tidyr::full_seq(INVYR, 1))
    }) |>
    purrr::list_rbind()

  tree_annual <-
    dplyr::right_join(
      data,
      all_yrs,
      by = dplyr::join_by(tree_ID, INVYR == YEAR)
    ) |>
    dplyr::rename(YEAR = INVYR) |>
    dplyr::arrange(tree_ID, YEAR) |>
    #fill down any time-invariant columns
    dplyr::group_by(tree_ID) |>
    tidyr::fill(plot_ID, SPCD, ECOSUBCD, DESIGNCD, PROP_BASIS) |>
    dplyr::ungroup() |>
    #rearrange
    dplyr::select(
      tree_ID,
      plot_ID,
      YEAR,
      DIA,
      HT,
      ACTUALHT,
      CR,
      CULL,
      everything()
    )
  tree_annual
}

# tree_annual |>
#   group_by(tree_ID, plot_ID) |>
#   summarize(n = length(unique(OWNGRPCD))) |>
#   filter(n > 1)
