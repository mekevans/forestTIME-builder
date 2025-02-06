library(data.table)

setwd("/mnt/Main/nsbe_tech_transfer/VTECO/")

# prep work
fiadb2 <- fread("../../fiadb_vteco_w_dead_and_loss_deductions.csv")
fiadb2[,c("DBH", "THT")] <- NULL
names(fiadb2)[which(names(fiadb2) == 'TRE_CN')] <- 'CN'

fiar <- data.frame(FIAR= c("Southern", "Pacific NW", "Rocky Mountain", "Northern", "Northern"),
                   RS= c("SRS", "PNWRS", "RMRS", "NERS", "NCRS"))

fiadb2 <- merge(x= fiadb2,
                y= fiar,
                by= "RS")

fiadb2[,c('VMERIB', 'VSAWIB')] <- fiadb2[,c('VMERIB_SOUND', 'VSAWIB_SOUND')]

statecds <- read.csv("../Shared/Files/state_and_region_codes.csv", as.is= TRUE)
names(statecds) <- toupper(names(statecds))

source("../Shared/R/sharedFuns.R")

# try splitting fiadb into subsets to process
fiadbS <- split(fiadb2, fiadb2$STATECD)
fiadb3w <- lapply(fiadbS, castWide, tree_vars= vars)
fiadb3w <- rbindlist(fiadb3w)

fiadb3w$DCLASS <- ifelse(fiadb3w$DCLASS >= 60, 60, fiadb3w$DCLASS)

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

by_vars <- lapply(by_vars, c, 'STATUSCD')

aggs <- lapply(by_vars, makeSum, dat= fiadb3w)

# meaningful variable names
vardefs <- c("Total aboveground biomass",
             "Merchantable wood volume",
             "Top and limb weight",
             "Merch bole wood and bark weight",
             "Stump wood and bark weight",
             "Sawlog wood volume",
             "Total aboveground carbon")

names(vardefs) <- c("BIOMASS", "VMERIB", "WMERBCH", "WMEROB", "WSTPOB", "VSAWIB",
                    "CARBON")

addLabel <- function(dat) {
  
  dat$VAR2 <- vardefs[dat$variable]
  
  dat
  
}

aggs <- lapply(aggs, addLabel)

orderIt <- function(dat) {
  
  dat[order(dat[,1]), ]
  
}

aggs <- lapply(aggs, orderIt)
setwd("Decay_and_Dead/")
Map(writeFIADBSummary, aggs, table_names)

# file.remove("Output/fiadb_inventory_w_pred.csv")
