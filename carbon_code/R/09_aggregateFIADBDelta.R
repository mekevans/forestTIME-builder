library(data.table)

source("../Shared/R/sharedFuns.R")

fiadb <- fread("Output/fiadb_delta_biomass_w_prediction.csv")

# component definitions differ between merch and agb definitions
fiadb[fiadb$COMPONENT == 'NOT USED',
      c("WMEROB_BEGIN", "WMERBCH_BEGIN", "WSTPOB_BEGIN", "VMERIB_BEGIN",
        "WMEROB_END", "WMERBCH_END", "WSTPOB_END", "VMERIB_END")] <- NA

fiadb[fiadb$COMPONENT_AGB == 'NOT USED',
      c("BIOMASS_BEGIN", "BIOMASS_END")] <- NA

# reduce the tree df in size
tree_vars <- fiadb[,c("TREE_CN", "ID", "PLT_CN", "CONDID", "PREV_PLT_CN",
                      "PREV_COND", "STATECD", "UNITCD", "COUNTYCD", "LAT",
                      "LON", "SPCD", "GENUS", "JENKINS_SPGRPCD", "WDSG", 
                      "BKSG", "PREV_TRE_CN",
                      "TPAGROW_UNADJ", "ADJ_FACTOR", "COMPONENT",
                      "ADJ_FACTOR_AGB", "TPAGROW_UNADJ_AGB", "COMPONENT_AGB",
                      "REMPER", "EXPNS", "DIA3", "HT3", "DIA1",
                      "DIA2", "HT2", "EVALID", "STDORGCD_BEGIN", "STDORGCD_END",
                      "CULL_END", "CULL_BEGIN", "DIA_END", "HT_END")]

tree_vars$TREE_CN <- as.character(tree_vars$TREE_CN)

# need to go long - tree x component x time x method (crm1/crm2)
# do by var to reduce size
fiadb_long <- lapply(c(crm_vars, pred_vars), meltIt, data= fiadb)
fiadb_long <- rbindlist(fiadb_long)

fiadb_long[is.na(fiadb_long$value),]$value <- 0

# now cast to a wider format
# translate the names
fiadb_long[,c("var", "period")] <- tstrsplit(fiadb_long$variable, "_")
fiadb_long$TREE_CN <- as.character(fiadb_long$TREE_CN)

fiadb_long$var2 <- trans[fiadb_long$var]
fiadb_long[,c("var3", "type")] <- tstrsplit(fiadb_long$var2, "_")

# this is a tree x time dataset
fiadb_wide <- dcast(fiadb_long, TREE_CN+period~var3+type, value.var= "value")

# final form - tree x component x time
fiadb_final <- reshapeAgain(fiadb_wide)

fiadb_final <- merge(x= fiadb_final,
                     y= tree_vars,
                     by= "TREE_CN")

rm(fiadb_wide, fiadb_long, fiadb, tree_vars)

# add 2" diameter class starting at 1"
fiadb_final$DCLASS <- floor((fiadb_final$DIA_END + 1) / 2) * 2 - 1

# different adjustment and tpa vars depending on component
tpa_adj_vars <- unique(fiadb_final[,c("TREE_CN", "var3", "ADJ_FACTOR_AGB", "ADJ_FACTOR",
                                      "TPAGROW_UNADJ_AGB", "TPAGROW_UNADJ",
                                      "COMPONENT", "COMPONENT_AGB")])

tpa_adj_vars$ADJ_FACTOR_COMB <- ifelse(tpa_adj_vars$var3 == 'BIO',
                                       tpa_adj_vars$ADJ_FACTOR_AGB,
                                       tpa_adj_vars$ADJ_FACTOR)

tpa_adj_vars$TPAGROW_UNADJ_COMB <- ifelse(tpa_adj_vars$var3 == 'BIO',
                                          tpa_adj_vars$TPAGROW_UNADJ_AGB,
                                          tpa_adj_vars$TPAGROW_UNADJ)

tpa_adj_vars$COMPONENT_COMB <- ifelse(tpa_adj_vars$var3 == 'BIO',
                                          tpa_adj_vars$COMPONENT_AGB,
                                          tpa_adj_vars$COMPONENT)

tpa_adj_vars$CONV <- ifelse(tpa_adj_vars$var3 == 'VOL',
                            1,
                            2000)

tpa_adj_vars$COMPONENT_COMB <- gsub("[[:digit:]]", "", tpa_adj_vars$COMPONENT_COMB)

tpa_adj_vars[,c("ADJ_FACTOR_AGB", "ADJ_FACTOR",
                "TPAGROW_UNADJ_AGB", "TPAGROW_UNADJ",
                "COMPONENT", "COMPONENT_AGB")] <- NULL

fiadb_final <- merge(x= fiadb_final,
                     y= tpa_adj_vars,
                     by= c("TREE_CN", "var3"))

# can go ahead and remove all the component == 'NOT USED'
fiadb_final <- fiadb_final[fiadb_final$COMPONENT_COMB != 'NOT USED', ]

fwrite(fiadb_final, "Output/fiadb_to_summarise.csv")

# aggregate levels
delta_by <- list(c("STATECD"),
                 c("SPCD"),
                 c("DCLASS"),
                 c("COMPONENT_COMB"),
                 c("STATECD", "SPCD"),
                 c("STATECD", "DCLASS"),
                 c("SPCD", "DCLASS"),
                 c("STATECD", "UNITCD"),
                 c("STATECD", "UNITCD", "SPCD"))

lapply(delta_by, makeAggregations, data= fiadb_final)
lapply(delta_by, aggregateT1T2, data= fiadb_final)

file.remove("Output/fiadb_delta_biomass_w_prediction.csv")
file.remove("Output/fiadb_to_summarise.csv")
