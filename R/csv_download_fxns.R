#' Download state data
#'
#' @param state two letter state abbreviation code
#' @param rawdat_dir where to put the data
#' @param max_time seconds at which to time out
#'
#' @return nothing
#' @export
#' 
download_state_data <- function(state = "CT", rawdat_dir = "data/csv", max_time = 300) {
  
  if(!dir.exists(here::here(rawdat_dir))) {
    
    dir.create(here::here(rawdat_dir), recursive = T)
    
  }
  
  download_from_datamart(state, table = "TREE", file_dir = here::here(rawdat_dir), max_time = max_time) #this is very fast
  download_from_datamart(state, table = "PLOT", file_dir = here::here(rawdat_dir), max_time = max_time) #this is very fast
  download_from_datamart(state, table = "COND", file_dir = here::here(rawdat_dir), max_time = max_time) #this is very fast
  
}


#' Workhorse function to download tables from datamart
#'
#' @param state two letter state code
#' @param table TREE, PLOT, or COND
#' @param file_dir where to put the data
#' @param max_time seconds at which to time out download
#'
#' @return
download_from_datamart <- function(state, table = "TREE", file_dir, max_time = 300) {
  
  url <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", state, "_", table, ".csv")
  
  csv_file <- file.path(file_dir, basename(url))
  
  options(timeout = max_time)
  
  system.time(downloaded <- try(utils::download.file(url, destfile = csv_file)))

  
  if ("try-error" %in% class(downloaded)) {
    stop("Download failed")
  }

  on.exit(options(timeout = 60))
   
}