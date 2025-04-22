# run all virginia tech scripts

# path to this repo
setwd("VTECO")

script_list <- list.files("R/", pattern= "^[[:digit:]]")
script_list <- script_list[!grepl("^000", script_list)]

decay_list <- list.files("../Decay_and_Dead/R/", "^[[:digit:]]")
decay_list <- decay_list[!grepl("^000", decay_list)]

if (!run_delta) script_list <- script_list[!grepl("Delta", script_list)]

# flag if we want to estimate coefficients
# the mixed modeling is slow
fit_models <- dev

object_list <- ls()
object_list <- c(object_list, "object_list")

if (!fit_models) script_list <- script_list[-1:-3]

for (file in script_list) {
  
  cat("running script", file, "\n")
  
  source(file.path("R", file))
  
  # delete everything not in original object list
  rm(list= ls()[!ls() %in% object_list]) 
  
}

for (file in decay_list) {
  
  cat("running script", file, "\n")
  
  source(file.path("R", file))
  
  # delete everything not in original object list
  rm(list= ls()[!ls() %in% object_list]) 
  
}

# set back to original path
setwd(tech_transfer_path)
