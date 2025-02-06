#' Add tree_annualized table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct  group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
add_annual_estimates_to_db <- function(con) {
  existing_tables <- dbListTables(con)
  
  if ("tree_annualized" %in% existing_tables) {
    message("tree_annualized table already present in database!")
    return()
  }
  
  if (!(all(c("tree", "tree_info_composite_id") %in% existing_tables))) {
    message(
      "At least one of tree, tree_info_composite_id tables not present in database; needed for tree_annualized!"
    )
    return()
  }
  
  if (!("all_invyrs" %in% existing_tables)) {
    all_invyrs <- data.frame(INVYR = c(2000:2024))
    copy_to(dest = con, df = all_invyrs, name = "all_invyrs", temporary = FALSE)
  }
  
  
  trees <- tbl(con, "tree")  |>
    #filter(PLOT_COMPOSITE_ID == "27_2_61_20675") |>
    mutate(
      ACTUALHT = as.numeric(ACTUALHT),
      HT = as.numeric(HT),
      DIA = as.numeric(DIA),
      MORTYR = as.numeric(MORTYR),
      INVYR = as.numeric(INVYR)
    ) |>
    #fill ACTUALHT with HT if NA.
    mutate(ACTUALHT = ifelse(is.na(ACTUALHT), HT, ACTUALHT)) |>
    group_by(TREE_COMPOSITE_ID) |> #for every tree...
    #TODO: this is where I suspect trees with NAs for some surveys get dropped
    filter(all(STATUSCD == 1) |
             ((any(STATUSCD == 1) && any(STATUSCD == 2))),
           all(!is.na(DIA)),
           all(!is.na(HT)),
           all(!is.na(ACTUALHT))) |>
    ungroup() |>
    select(
      STATECD,
      TREE_COMPOSITE_ID,
      INVYR,
      DIA,
      HT,
      ACTUALHT,
      TREE_CN,
      PLT_CN,
      CONDID,
      MORTYR,
      STATUSCD
    ) |> 
    group_by(TREE_COMPOSITE_ID) |>
    # TODO: to deal with NAs, lag and lead would have to be replaced by functions
    # that look for the prev/next *non-NA* value or something
    mutate(prev_invyr = lag(INVYR, default = NA, order_by = INVYR),
           next_invyr = lead(INVYR, default = NA, order_by = INVYR),
           next_height = lead(HT, default = NA, order_by = INVYR),
           next_dia = lead(DIA, default = NA, order_by = INVYR),
           next_aheight = lead(ACTUALHT, default = NA, order_by = INVYR),
           ever_dead = any(STATUSCD == 2),
           live_year = ifelse(STATUSCD == 1,
                              INVYR,
                              NA),
           dead_year = ifelse(STATUSCD == 2,
                              INVYR,
                              NA)) |>
    mutate(last_live_year = max(live_year, na.rm = TRUE),
           first_dead_year = min(dead_year, na.rm = TRUE),
           next_height = ifelse(is.na(next_height),
                                HT,
                                next_height),
           next_dia = ifelse(is.na(next_dia),
                             DIA,
                             next_dia),
           next_aheight = ifelse(is.na(next_aheight),
                                 ACTUALHT,
                                 next_aheight)) |>
    mutate(midpoint_dead_year = ifelse(ever_dead,
                                       ceiling((first_dead_year + last_live_year) / 2),
                                       NA),
           first_dead_height = ifelse(INVYR == first_dead_year,
                                      HT,
                                      NA),
           first_dead_dia = ifelse(INVYR == first_dead_year,
                                   DIA,
                                   NA),
           first_dead_aheight = ifelse(INVYR == first_dead_year,
                                       ACTUALHT,
                                       NA),
           first_dead_cn = ifelse(INVYR == first_dead_year,
                                  TREE_CN,
                                  NA)) |>
    mutate(first_dead_height = max(first_dead_height,
                                   na.rm = TRUE),
           first_dead_dia = max(first_dead_dia,
                                na.rm = TRUE),
           first_dead_aheight = max(first_dead_aheight,
                                    na.rm = TRUE),
           first_dead_cn = max(first_dead_cn, na.rm =TRUE),
           dead_mortyr = ifelse(ever_dead,
                                max(MORTYR, na.rm = TRUE),
                                NA)) |>
    ungroup() |>
    mutate(period_start = ifelse(STATUSCD == 2,
                                 ifelse(INVYR == first_dead_year,
                                        midpoint_dead_year,
                                        INVYR),
                                 INVYR),
           period_stop = ifelse(is.na(next_invyr),
                                INVYR,
                                ifelse(INVYR == last_live_year,
                                       midpoint_dead_year - 1,
                                       next_invyr - 1)),
           period_length = ifelse(
             period_stop == period_start,
             NA,
             period_stop - period_start + 1)) |>
    mutate(mortyr_dead_year = ifelse(is.na(dead_mortyr),
                                     midpoint_dead_year,
                                     dead_mortyr)) |>
    mutate(period_start_mortyr = ifelse(STATUSCD == 2,
                                        ifelse(INVYR == first_dead_year,
                                               mortyr_dead_year,
                                               INVYR),
                                        INVYR),
           period_stop_mortyr = ifelse(is.na(next_invyr),
                                       INVYR,
                                       ifelse(INVYR == last_live_year,
                                              mortyr_dead_year - 1,
                                              next_invyr - 1)),
           period_length_mortyr = ifelse(
             period_stop_mortyr == period_start_mortyr,
             NA,
             period_stop_mortyr - period_start_mortyr + 1)) |>
    # Linear interpolation
    mutate(ht_slope_mortyr = ifelse(period_stop_mortyr == period_start_mortyr,
                                    0,
                                    (next_height - HT) / period_length_mortyr),
           dia_slope_mortyr = ifelse(period_stop_mortyr == period_start_mortyr,
                                     0,
                                     (next_dia - DIA)/ period_length_mortyr),
           aheight_slope_mortyr = ifelse(period_stop_mortyr == period_start_mortyr,
                                         0,
                                         (next_aheight - ACTUALHT) / period_length_mortyr),
           ht_slope = ifelse(period_stop == period_start,
                             0,
                             (next_height - HT) / period_length),
           dia_slope = ifelse(period_stop == period_start,
                              0,
                              (next_dia - DIA)/ period_length),
           aheight_slope = ifelse(period_stop == period_start,
                                  0,
                                  (next_aheight - ACTUALHT) / period_length))
  
  all_years <- tbl(con, "tree") |>
    select(TREE_COMPOSITE_ID) |>
    distinct() |>
    cross_join(tbl(con, "all_invyrs")) |>
    rename(YEAR = INVYR)
  
  by <-
    join_by(TREE_COMPOSITE_ID,
            between(YEAR, period_start, period_stop, bounds = "[]"))
  
  trees_annual_measures <- all_years |>
    inner_join(trees, by) |>
    mutate(
      is_dead = ifelse(
        ever_dead,
        YEAR >= midpoint_dead_year,
        FALSE),
      time_run_midpoint = ifelse(
        is_dead | YEAR == INVYR,
        0,
        YEAR - INVYR),
      HT_est_midpoint = ifelse(
        is_dead,
        first_dead_height,
        HT + (time_run_midpoint * ht_slope)),
      DIA_est_midpoint = ifelse(
        is_dead,
        first_dead_dia,
        DIA + (time_run_midpoint * dia_slope)),
      AHEIGHT_est_midpoint = ifelse(
        is_dead,
        first_dead_aheight,
        ACTUALHT + (time_run_midpoint * aheight_slope)),
      TREE_CN_midpoint = ifelse(ever_dead && YEAR >= midpoint_dead_year,
                                first_dead_cn,
                                TREE_CN)) |>
    select(STATECD,
           TREE_COMPOSITE_ID,
           TREE_CN_midpoint,
           YEAR,
           midpoint_dead_year,
           HT_est_midpoint,
           DIA_est_midpoint,
           AHEIGHT_est_midpoint) 
  
  by_mortyr <-
    join_by(TREE_COMPOSITE_ID,
            between(YEAR, period_start_mortyr, period_stop_mortyr, bounds = "[]"))
  
  trees_annual_measures_mortyr <- all_years |>
    inner_join(trees, by_mortyr) |>
    mutate(
      is_dead_mortyr = ifelse(
        ever_dead,
        YEAR >= mortyr_dead_year,
        FALSE),
      time_run_mortyr = ifelse(
        is_dead_mortyr | YEAR == INVYR,
        0,
        YEAR - INVYR),
      HT_est_mortyr = ifelse(
        is_dead_mortyr,
        first_dead_height,
        HT + (time_run_mortyr * ht_slope_mortyr)),
      DIA_est_mortyr = ifelse(
        is_dead_mortyr,
        first_dead_dia,
        DIA + (time_run_mortyr * dia_slope_mortyr)),
      AHEIGHT_est_mortyr = ifelse(
        is_dead_mortyr,
        first_dead_aheight,
        ACTUALHT + (time_run_mortyr * aheight_slope_mortyr))) |>
    mutate(TREE_CN_mortyr = ifelse(ever_dead && YEAR >= mortyr_dead_year,
                                   first_dead_cn,
                                   TREE_CN)) |>
    select(STATECD, 
           TREE_COMPOSITE_ID,
           TREE_CN_mortyr,
           YEAR,
           mortyr_dead_year,
           HT_est_mortyr,
           DIA_est_mortyr,
           AHEIGHT_est_mortyr)
  
  trees_annual_measures_mortyr_nsvb <- trees_annual_measures_mortyr |>   
  #  filter(grepl("27_2_61_20675", TREE_COMPOSITE_ID)) |>
    rename(TRE_CN = TREE_CN_mortyr,
           HT = HT_est_mortyr,
           DIA = DIA_est_mortyr,
           ACTUALHT = AHEIGHT_est_mortyr) |>
    left_join(tbl(con, "nsvb_vars") |> select(-HT, -DIA, -ACTUALHT),
              by = join_by(TRE_CN))
  
  trees_annual_measures_midpoint_nsvb <- trees_annual_measures |>   
    #filter(grepl("27_2_61_20675", TREE_COMPOSITE_ID)) |>
    rename(TRE_CN = TREE_CN_midpoint,
           HT = HT_est_midpoint,
           DIA = DIA_est_midpoint,
           ACTUALHT = AHEIGHT_est_midpoint) |>
    left_join(tbl(con, "nsvb_vars") |> select(-HT, -DIA, -ACTUALHT),
              by = join_by(TRE_CN))
  
  all_annual_measures <- trees_annual_measures |>
    left_join(trees_annual_measures_mortyr, by = join_by(STATECD, TREE_COMPOSITE_ID, YEAR)) 
  
  copy_to(dest = con, df = all_annual_measures, name = "tree_annualized", temporary = FALSE)

  copy_to(
    dest = con,
    df = trees_annual_measures_midpoint_nsvb,
    name = "trees_annual_measures_midpoint_nsvb",
    temporary = FALSE
  )
  copy_to(
    dest = con,
    df = trees_annual_measures_mortyr_nsvb,
    name = "trees_annual_measures_mortyr_nsvb",
    temporary = FALSE
  )
  
  return()
}