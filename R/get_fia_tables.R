#these are the tables we need
tables <- c(
  "PLOT",
  "COND",
  "TREE",
  "PLOTGEOM",
  "POP_ESTN_UNIT",
  "POP_EVAL",
  "POP_EVAL_TYP",
  "POP_PLOT_STRATUM_ASSGN",
  "POP_STRATUM"
)

#' Download zip files from FIA datamart
#'
#' The zip files are smaller than just the *_TREE.csv, so this just downloads
#' the whole zip and extracts the required CSV files (TREE, PLOT, and COND). Uses
#' `curl::multi_download()` which resumes and skips partial and incomplete
#' downloads, respectively, when run subsequent times.
#' @param states vector of state abbreviations; for all states use `state.abb`.
#' @param rawdat_dir where to save the zip files.
#' @param extract logical; extract the TREE and PLOT csv files?
#' @param keep_zip logical; keep the .zip file after CSVs are extracted?
get_fia_tables <- function(
  states,
  rawdat_dir = here::here("data/rawdat/state/"),
  extract = TRUE,
  keep_zip = FALSE
) {
  states <- match.arg(states, state.abb, several.ok = TRUE)
  cli::cli_progress_step("Downloading FIA data for {states}")

  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  files <- glue::glue("{states}_CSV.zip")
  out_paths <- fs::path(rawdat_dir, files)
  urls <- URLencode(paste0(base_url, files))

  #check if .zip is already downloaded
  zip_check <- fs::path(rawdat_dir, files) |> fs::file_exists()

  #check if .csvs are there from a previous run with keep_zip = FALSE
  csv_check <-
    purrr::map(states, \(state) {
      fs::path(rawdat_dir, glue::glue("{state}_{tables}.csv"))
    }) |>
    rlang::set_names(states) |>
    purrr::map_lgl(\(x) all(fs::file_exists(x)))

  #if zip is there, but not CSVs just unzip
  if (any(zip_check & !csv_check) & isTRUE(extract)) {
    unzip_csvs(
      names(zip_check)[zip_check & !csv_check],
      rawdat_dir = rawdat_dir,
      keep_zip = keep_zip
    )
    #update CSV check
    csv_check <-
      purrr::map(states, \(state) {
        fs::path(rawdat_dir, glue::glue("{state}_{tables}.csv"))
      }) |>
      purrr::map_lgl(\(x) all(fs::file_exists(x)))
  }
  #only download what is needed
  urls <- urls[!csv_check]
  out_paths <- out_paths[!csv_check]

  if (length(urls) == 0) {
    cli::cli_warn("All CSVs already downloaded!")
    return(invisible(NULL))
  }

  #download file(s)
  resp <- curl::multi_download(
    urls = urls,
    destfiles = out_paths,
    resume = TRUE,
    progress = TRUE,
    useragent = "forestTIME-builder (https://github.com/mekevans/forestTIME-builder)",
    ssl_verifypeer = 0L #not sure why, but was getting SSL verification errors from datamart starting 2025-03-17
  )

  #TODO: check response for issues, retries, whatever

  zips <- resp$destfile
  if (isFALSE(extract)) {
    return(zips)
  } else {
    unzip_csvs(zips, rawdat_dir = rawdat_dir, keep_zip = keep_zip)
    return(invisible(NULL))
  }
}

unzip_csvs <- function(zips, rawdat_dir, keep_zip) {
  cli::cli_alert_info("Extracting CSVs from .zip files")
  #pull out the CSVs of interest for each state
  lapply(zips, \(zip) {
    csvs <- stringr::str_replace(
      fs::path_file(zip),
      "CSV.zip",
      glue::glue("{tables}.csv")
    )
    unzip(zip, files = csvs, exdir = rawdat_dir)
    if (isFALSE(keep_zip)) {
      cli::cli_alert_info("Removing .zip file")
      fs::file_delete(zip)
    }
  })
  return(invisible(TRUE))
}


#' Read in needed tables
#'
#' Wrapper for [rFIA::readFIA] that reads in the necessary tables
#' @inheritParams rFIA::readFIA()
#'
read_fia <- function(states, dir = here::here("data/rawdat/state/")) {
  rFIA::readFIA(dir = dir, states = states, tables = tables) |>
    purrr::map(tibble::as_tibble)
}
