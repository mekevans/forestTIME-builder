# library(data.table)

# prep work
fiadb2 <- fread("Output/fiadb_inventory_w_pred.csv")

names(fiadb2)[which(names(fiadb2) == 'TRE_CN')] <- "CN"

statecds <- read.csv("../Shared/Files/state_and_region_codes.csv", as.is= TRUE)
names(statecds) <- toupper(names(statecds))

source("../Shared/R/sharedFuns.R")

# try splitting fiadb into subsets to process
fiadbS <- split(fiadb2, fiadb2$STATECD)
fiadb3w <- lapply(fiadbS, castWide, tree_vars= vars)
fiadb3w <- rbindlist(fiadb3w)

# make paired variable for plot generation
# fiadb3w <- castWide(fiadb2, vars)

# levels of aggregation
# 1. state
# 2. state by county
# 3. species
# 4. species by state
# 5. species by diameter class
# 6. ecodivision
# 7. spcd by ecodivision
# 8. division
# 9. spcd by division

aggs <- lapply(by_vars, makeSum, dat= fiadb3w)

# meaningful variable names
vardefs <- c("Total aboveground biomass",
             "Merchantable wood volume",
             "Top and limb weight",
             "Merch bole wood and bark weight",
             "Stump wood and bark weight",
             "Sawlog wood volume")

names(vardefs) <- c("BIOMASS", "VMERIB", "WMERBCH", "WMEROB", "WSTPOB", "VSAWIB")

addLabel <- function(dat) {
  
  dat$VAR2 <- vardefs[dat$variable]
  
  dat
  
}

aggs <- lapply(aggs, addLabel)

orderIt <- function(dat) {
  
  dat[order(dat[,1]), ]
  
}

aggs <- lapply(aggs, orderIt)

Map(writeFIADBSummary, aggs, table_names)

file.remove("Output/fiadb_inventory_w_pred.csv")
