#' Download zip files from FIA datamart
#' 
#' The zip files are smaller than just the *_TREE.csv, so this just downloads the whole zip and extracts the required CSV files (TREE)
#' uses curl::multi_download which resumes and skips partial and incomplete downloads, respectively, when run subsequent times
#' @param states vector of state abbreviations
#' @param rawdat_dir where to save the zip files
#' @param extract logical; extract the TREE and PLOT csv files?
#' @param keep_zip logical; keep the .zip file after CSVs are extracted?
download_zip_from_datamart <- function(states,
                                       rawdat_dir = "data/rawdat/state/",
                                       extract = TRUE,
                                       keep_zip = FALSE) {
  states <- match.arg(states, state.abb, several.ok = TRUE)
  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  files <- paste0(states, "_CSV.zip")
  out_paths <- fs::path(rawdat_dir, files)
  urls <- URLencode(paste0(base_url, files))
  
  #download file(s)
  resp <- curl::multi_download(
    urls = urls,
    destfiles = out_paths,
    resume = TRUE,
    progress = TRUE,
    useragent = "forestTIME-builder (https://github.com/mekevans/forestTIME-builder)"
  )
  
  #TODO: check response for issues, retries, whatever
  
  zips <- resp$destfile
  if (isFALSE(extract)) {
    return(zips)
  } else {
    cli::cli_alert_info("Extracting CSVs from .zip files")
    #pull out the CSVs of interest for each state
    lapply(zips, \(zip) {
      csvs <- c(
        gsub("CSV.zip", "TREE.csv", fs::path_file(zip)),
        gsub("CSV.zip", "PLOT.csv", fs::path_file(zip))
      )
      unzip(zip, files = csvs, exdir = rawdat_dir)
      if (isFALSE(keep_zip)) {
        cli::cli_alert_info("Removing .zip file")
        fs::file_delete(zip)
      }
    })
  }
}
