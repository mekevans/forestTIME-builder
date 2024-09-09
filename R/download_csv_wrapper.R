source(here::here("R", "csv_download_fxns.R"))

#' Download state .csv files from DataMart
#'
#' @param states vector of state abbreviations to download data from
#' @param rawdat_dir directory to store csvs in
#' @param overwrite TRUE or FALSE. If TRUE, will re-download data and overwrite existing files. If FALSE, will only download files not already present in rawdat_dir.
#'
#' @return nothing
#' @export
#'
#' @importFrom purrr map
download_csv_from_datamart <- function(states, rawdat_dir, overwrite = FALSE) {
  
  if(!overwrite) {
    
    extant_states <- purrr::map_lgl(states, check_downloaded, rawdat_dir = rawdat_dir)
    states <- states[ !extant_states]
      
  }
  
  purrr::map(states, download_state_data, rawdat_dir = rawdat_dir, max_time = 2000)
  
  if(overwrite | 
     !file.exists(here::here("data", "rawdat", "REF_SPECIES.csv"))) {
    
    url <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv")
    
    csv_file <- file.path("data/rawdat", basename(url))
    
    options(timeout = 200)
    
    system.time(downloaded <- try(utils::download.file(url, destfile = csv_file)))
    
    
    if ("try-error" %in% class(downloaded)) {
      stop("Download failed")
    }
    
    on.exit(options(timeout = 60))
  }
  
  if(overwrite | 
     !file.exists(here::here("data", "rawdat", "REF_TREE_DECAY_PROP.csv"))) {
    
    url <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/REF_TREE_DECAY_PROP.csv")
    
    csv_file <- file.path("data/rawdat", basename(url))
    
    options(timeout = 200)
    
    system.time(downloaded <- try(utils::download.file(url, destfile = csv_file)))
    
    
    if ("try-error" %in% class(downloaded)) {
      stop("Download failed")
    }
    
    on.exit(options(timeout = 60))
  }
}

#' Check if files are already downloaded
#'
#' @param state two letter state abbreviation
#' @param rawdat_dir data storage directory
#'
#' @return logical whether or not all three .csv files for that state are present in rawdat_dir
#' @export
#'
check_downloaded <- function(state, rawdat_dir) {
  
  all(file.exists(here::here(rawdat_dir, paste0(state, "_TREE.csv"))),
      file.exists(here::here(rawdat_dir, paste0(state, "_PLOT.csv"))),
      file.exists(here::here(rawdat_dir, paste0(state, "_COND.csv"))))
  
}

