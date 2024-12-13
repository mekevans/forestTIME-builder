download_datamart_zip <- function(states, rawdat_dir = "data/rawdat/state/") {
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
  
  #return
  resp$destfile
}