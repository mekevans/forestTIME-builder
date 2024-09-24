#' Add tree_annualized table to database
#'
#' @param con database connection
#'
#' @return nothing
#' @export
#' @importFrom DBI dbListTables dbSendStatement
#' @importFrom dplyr collect select distinct  group_by mutate ungroup left_join summarize n filter cross_join join_by lead inner_join
#' @importFrom arrow to_duckdb
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
    
    arrow::to_duckdb(all_invyrs, con, "all_invyrs")
    dbSendStatement(con, "CREATE TABLE all_invyrs AS SELECT * FROM all_invyrs")
  }
  
  
  trees <- tbl(con, "tree")  |>
    #filter(PLOT_COMPOSITE_ID == "27_2_61_20675") |>
    # Import needed columns as numeric
    mutate(
      ACTUALHT = as.numeric(ACTUALHT),
      HT = as.numeric(HT),
      DIA = as.numeric(DIA),
      MORTYR = as.numeric(MORTYR),
      INVYR = as.numeric(INVYR)
    ) |>
    # Fill in ACTUALHT from HT (as I understand, AHT is only filled in when it
    # is a correction to HT)
    mutate(ACTUALHT = ifelse(is.na(ACTUALHT), HT, ACTUALHT)) |>
    # Filter to trees that are a) always alive or b) always alive or dead
    # This needs to be changed - probably - to include trees that get STATUSCDs of 0 or are
    # dead plus STANDING_DEAD of 1.
    group_by(TREE_COMPOSITE_ID) |>
    filter(all(STATUSCD == 1) |
             ((any(STATUSCD == 1) && any(STATUSCD == 2))),
           # And this filters out trees with missing DIA/HT/ACTUALHT measurements.
           # This will need to change to interpolate if a tree has a missed
           # measurement sandwiched between two actual measurements.
           # I guess if a tree has leading/lagging missing measurements,
           # you could "interpolate" by setting the measurement to the first
           # non-NA measurement?
           all(!is.na(DIA)),
           all(!is.na(HT)),
           all(!is.na(ACTUALHT))) |>
    ungroup() |>
    select(
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
    # Within each tree, establish the next invyr, next measurements,
    # if it is ever dead,
    # and whether in each year it is alive or dead
    group_by(TREE_COMPOSITE_ID) |>
    mutate(
      prev_invyr = lag(INVYR, default = NA, order_by = INVYR),
      next_invyr = lead(INVYR, default = NA, order_by = INVYR),
      next_height = lead(HT, default = NA, order_by = INVYR),
      next_dia = lead(DIA, default = NA, order_by = INVYR),
      next_aheight = lead(ACTUALHT, default = NA, order_by = INVYR),
      ever_dead = any(STATUSCD == 2),
      live_year = ifelse(STATUSCD == 1, INVYR, NA),
      dead_year = ifelse(STATUSCD == 2, INVYR, NA)
    ) |>
    # Now identify the last survey in which this tree was seen alive
    # and the first year in which it was seen dead
    # Also fill in the first/last survey years with next_HT = current HT, etc.
    mutate(
      last_live_year = max(live_year, na.rm = TRUE),
      first_dead_year = min(dead_year, na.rm = TRUE),
      next_height = ifelse(is.na(next_height), HT, next_height),
      next_dia = ifelse(is.na(next_dia), DIA, next_dia),
      next_aheight = ifelse(is.na(next_aheight), ACTUALHT, next_aheight)
    ) |>
    # Define the years and measurements at death
    # using the midpoint method
    mutate(
      # Midpoint method: If a tree is ever dead,
      # the year of death is the midpoint between the first year in which it is
      # observed dead and the last year in which it is observed alive
      midpoint_dead_year = ifelse(ever_dead, ceiling((
        first_dead_year + last_live_year
      ) / 2), NA),
      # Get the height, dia, aheight, and cn of the first dead year.
      # These values will be propagated forward to future dead years.
      first_dead_height = ifelse(INVYR == first_dead_year, HT, NA),
      first_dead_dia = ifelse(INVYR == first_dead_year, DIA, NA),
      first_dead_aheight = ifelse(INVYR == first_dead_year, ACTUALHT, NA),
      first_dead_cn = ifelse(INVYR == first_dead_year, TREE_CN, NA)
    ) |>
    
    # Fill in first_dead_height, etc, for all records of this tree
    mutate(
      first_dead_height = max(first_dead_height, na.rm = T),
      first_dead_dia = max(first_dead_dia, na.rm = T),
      first_dead_aheight = max(first_dead_aheight, na.rm = T),
      first_dead_cn = max(first_dead_cn, na.rm = T),
      dead_mortyr = ifelse(ever_dead, max(MORTYR, na.rm = T), NA)
    ) |>
    ungroup() |>
    # Define the start, stop, and length of each
    # inter-survey period, which is the period of time we will interpolate over.
    # This is the invyr-invyr interval unless the tree dies,
    # in which case it is invyr-midpoint.
    mutate(
      # If a tree is dead,
      # if this is the first year it's dead, the midpoint_dead_year (this will be *earlier* than INVYR)
      # Otherwise, just start with INVYR.
      period_start = ifelse(
        STATUSCD == 2,
        ifelse(INVYR == first_dead_year, midpoint_dead_year, INVYR),
        INVYR
      ),
      # If this is the last survey for this tree, stop is also INVYR
      period_stop = ifelse(
        is.na(next_invyr),
        INVYR,
        # Otherwise, if this is the last year the tree is alive,
        # stop this period at the midpoint dead year minus 1 (this will be LATER than INVYR but EARLIER than next_invyr)
        ifelse(INVYR == last_live_year, midpoint_dead_year - 1, # Otherwise, it's the next_invyr minus 1.
               next_invyr - 1)
      ),
      # Calculate the length of time covered by this period
      period_length = ifelse(period_stop == period_start, NA, period_stop - period_start + 1)
    ) |>
    # Do the same using the mortyr method
    # Fill in missing mortyrs with the midpoint dead year
    mutate(mortyr_dead_year = ifelse(is.na(dead_mortyr), midpoint_dead_year, dead_mortyr)) |>
    # Define the period start year.
    # If a tree is dead,
    # if this INVYR is the first year it's recorded as dead,
    # start this period with the mortyr_dead_year. This will be *before* INVYR.
    # Otherwise it's the same as INVYR
    mutate(
      period_start_mortyr = ifelse(
        STATUSCD == 2,
        ifelse(INVYR == first_dead_year, mortyr_dead_year, INVYR),
        INVYR
      ),
      # Define the end of this time period.
      # If this is the last survey for this tree, the stop year is the INVYR.
      period_stop_mortyr = ifelse(
        is.na(next_invyr),
        INVYR,
        # If this INVYR is the last year it's seen alive,
        # the stop is the year before its mortality year.
        # This will be after INVYR but before next_invyr.
        ifelse(INVYR == last_live_year, mortyr_dead_year - 1, # If this isn't the last invyr it's seen alive,
               # stop is the year before the next invyr.
               next_invyr - 1)
      ),
      # Calculate the length of time covered by this period
      period_length_mortyr = ifelse(
        period_stop_mortyr == period_start_mortyr,
        NA,
        period_stop_mortyr - period_start_mortyr + 1
      )
    ) |>
    # Calculate slopes
    # If this period begins and ends in the same year
    # (i.e. this is the only measurement of this tree, or the last measurement of this tree
    # the slope is 0;
    # otherwise it's rise over run.
    mutate(
      ht_slope_mortyr = ifelse(
        period_stop_mortyr == period_start_mortyr,
        0,
        (next_height - HT) / period_length_mortyr
      ),
      dia_slope_mortyr = ifelse(
        period_stop_mortyr == period_start_mortyr,
        0,
        (next_dia - DIA) / period_length_mortyr
      ),
      aheight_slope_mortyr = ifelse(
        period_stop_mortyr == period_start_mortyr,
        0,
        (next_aheight - ACTUALHT) / period_length_mortyr
      ),
      ht_slope = ifelse(
        period_stop == period_start,
        0,
        (next_height - HT) / period_length
      ),
      dia_slope = ifelse(period_stop == period_start, 0, (next_dia - DIA) / period_length),
      aheight_slope = ifelse(
        period_stop == period_start,
        0,
        (next_aheight - ACTUALHT) / period_length
      )
    )
  # Get a table of all possible invyrs crossed with all of the TREE_COMPOSITE_IDs here
  all_years <- tbl(con, "tree") |>
    select(TREE_COMPOSITE_ID) |>
    distinct() |>
    cross_join(tbl(con, "all_invyrs")) |>
    rename(YEAR = INVYR)
  
  by <-
    join_by(TREE_COMPOSITE_ID,
            between(YEAR, period_start, period_stop, bounds = "[]"))
  
  # Join trees_annual_measures to the invyrs table
  # use the by above to get only the invyrs between the period stop and start years
  trees_annual_measures <- all_years |>
    inner_join(trees, by) |>
    # For each year...
    mutate(
      # Note if the tree is dead in this year
      is_dead = ifelse(ever_dead, YEAR >= midpoint_dead_year, FALSE),
      # Calculate how many years have passed in this mortyr
      time_run_midpoint = ifelse(YEAR == period_start, 0, YEAR - period_start),
      # Calculate estimates
      HT_est_midpoint = HT + (time_run_midpoint * ht_slope),
      DIA_est_midpoint = DIA + (time_run_midpoint * dia_slope),
      AHEIGHT_est_midpoint = ACTUALHT + (time_run_midpoint * aheight_slope),
      TREE_CN_midpoint = TREE_CN) |>
    select(
      TREE_COMPOSITE_ID,
      TREE_CN_midpoint,
      YEAR,
      midpoint_dead_year,
      HT_est_midpoint,
      DIA_est_midpoint,
      AHEIGHT_est_midpoint
    )
  
  # Repeat the above using the mortyr instead of the midpoint method.
  by_mortyr <-
    join_by(
      TREE_COMPOSITE_ID,
      between(YEAR, period_start_mortyr, period_stop_mortyr, bounds = "[]")
    )
  
  trees_annual_measures_mortyr <- all_years |>
    inner_join(trees, by_mortyr) |>
    mutate(
      is_dead_mortyr = ifelse(ever_dead, YEAR >= mortyr_dead_year, FALSE),
      time_run_mortyr = ifelse(YEAR == period_start_mortyr, 0, YEAR - period_start_mortyr),
      HT_est_mortyr = HT + (time_run_mortyr * ht_slope_mortyr),
      DIA_est_mortyr =  DIA + (time_run_mortyr * dia_slope_mortyr),
      AHEIGHT_est_mortyr = ACTUALHT + (time_run_mortyr * aheight_slope_mortyr),
      TREE_CN_mortyr = TREE_CN) |>
    select(
      TREE_COMPOSITE_ID,
      TREE_CN_mortyr,
      YEAR,
      mortyr_dead_year,
      HT_est_mortyr,
      DIA_est_mortyr,
      AHEIGHT_est_mortyr
    )
  
  # Extract the NSVB variables for the mortyr data
  trees_annual_measures_mortyr_nsvb <- trees_annual_measures_mortyr |>
    rename(
      TRE_CN = TREE_CN_mortyr,
      HT = HT_est_mortyr,
      DIA = DIA_est_mortyr,
      ACTUALHT = AHEIGHT_est_mortyr
    ) |>
    left_join(tbl(con, "nsvb_vars") |>
                select(-HT, -DIA, -ACTUALHT)) |>
    collect()
  
  # Extract the NSVB variables for the midpoint data
  trees_annual_measures_midpoint_nsvb <- trees_annual_measures |>
    rename(
      TRE_CN = TREE_CN_midpoint,
      HT = HT_est_midpoint,
      DIA = DIA_est_midpoint,
      ACTUALHT = AHEIGHT_est_midpoint
    ) |>
    left_join(tbl(con, "nsvb_vars") |>
                select(-HT, -DIA, -ACTUALHT)) |>
    collect()
  
  # Compile all annual measurements
  all_annual_measures <- trees_annual_measures |>
    left_join(trees_annual_measures_mortyr) |>
    collect()
  
  # Add all of these tables to the (intermediary state-level) database.
  arrow::to_duckdb(all_annual_measures, table_name = "tree_annualized", con = con)
  
  arrow::to_duckdb(trees_annual_measures_midpoint_nsvb,
                   table_name = "trees_annual_measures_midpoint_nsvb",
                   con = con)
  
  arrow::to_duckdb(trees_annual_measures_mortyr_nsvb,
                   table_name = "trees_annual_measures_mortyr_nsvb",
                   con = con)
  
  dbExecute(con,
            "CREATE TABLE tree_annualized AS SELECT * FROM tree_annualized")
  dbExecute(
    con,
    "CREATE TABLE trees_annual_measures_midpoint_nsvb AS SELECT * FROM trees_annual_measures_midpoint_nsvb"
  )
  dbExecute(
    con,
    "CREATE TABLE trees_annual_measures_mortyr_nsvb AS SELECT * FROM trees_annual_measures_mortyr_nsvb"
  )
  
  return()
  
}