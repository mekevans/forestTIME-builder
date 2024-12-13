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
  
  resp$destfile
  
  #TODO extract and remove zip if desired
  #might want to do this one state at a time??
  if (isTRUE(extract)) {
    cli::cli_inform("Extracting CSVs from .zip files")
    #pull out the CSVs of interest for each state
  }
  
  if (isTRUE(extract) & isFALSE(keep_zip)) {
    cil::cli_inform("Removing .zip files")
    fs::file_delete(resp$destfile)
  }
  
}