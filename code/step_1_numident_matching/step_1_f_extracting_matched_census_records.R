###############################################################################################################################
################### Step 1f: Creating Census Extracts for Potential Matches and Household Members  ############################
###############################################################################################################################
library(haven)

#1940
load("intermediate_outputs/histid_1940_matches.rda")
census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))
colnames(census_1940)=tolower(colnames(census_1940))
public_1940=census_1940[which((census_1940$histid %in% matches_1940)==TRUE),]
serial_1940=unique(census_1940$serial[which(census_1940$histid %in% matches_1940)])
households_1940_histid=census_1940[which(census_1940$serial %in% serial_1940), c("histid", "serial")]
save(households_1940_histid, file="intermediate_outputs/extract_census_samples/households_1940.rda")
public_1940=census_1940[which((census_1940$serial %in% serial_1940)==TRUE),]
rm(census_1940)
save(public_1940, file="intermediate_outputs/extract_census_samples/public/public_1940.rda")
census_1940 <- as.data.frame(read.csv("input_data/census_restricted/census_1940.csv"))
colnames(census_1940)=tolower(colnames(census_1940))
restricted_1940=census_1940[which((census_1940$histid %in% households_1940_histid$histid)==TRUE),]
rm(census_1940)
save(restricted_1940, file="intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
rm(restricted_1940)
rm(list=ls())
gc()

#1930
load("intermediate_outputs/histid_1930_matches.rda")
census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
colnames(census_1930)=tolower(colnames(census_1930))
public_1930=census_1930[which((census_1930$histid %in% matches_1930)==TRUE),]
serial_1930=unique(census_1930$serial[which(census_1930$histid %in% matches_1930)])
households_1930_histid=census_1930[which(census_1930$serial %in% serial_1930), c("histid", "serial")]
save(households_1930_histid, file="intermediate_outputs/extract_census_samples/households_1930.rda")
public_1930=census_1930[which((census_1930$serial %in% serial_1930)==TRUE),]
rm(census_1930)
save(public_1930, file="intermediate_outputs/extract_census_samples/public/public_1930.rda")
census_1930 <- as.data.frame(read.csv("input_data/census_restricted/census_1930.csv"))
colnames(census_1930)=tolower(colnames(census_1930))
restricted_1930=census_1930[which((census_1930$histid %in% households_1930_histid$histid)==TRUE),]
rm(census_1930)
save(restricted_1930, file="intermediate_outputs/extract_census_samples/restricted/restricted_1930.rda")
rm(restricted_1930)
rm(list=ls())
gc()

#1920
load("intermediate_outputs/histid_1920_matches.rda")
census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
colnames(census_1920)=tolower(colnames(census_1920))
public_1920=census_1920[which((census_1920$histid %in% matches_1920)==TRUE),]
serial_1920=unique(census_1920$serial[which(census_1920$histid %in% matches_1920)])
households_1920_histid=census_1920[which(census_1920$serial %in% serial_1920), c("histid", "serial")]
save(households_1920_histid, file="intermediate_outputs/extract_census_samples/households_1920.rda")
public_1920=census_1920[which((census_1920$serial %in% serial_1920)==TRUE),]
rm(census_1920)
save(public_1920, file="intermediate_outputs/extract_census_samples/public/public_1920.rda")
census_1920 <- as.data.frame(read.csv("input_data/census_restricted/census_1920.csv"))
colnames(census_1920)=tolower(colnames(census_1920))
restricted_1920=census_1920[which((census_1920$histid %in% households_1920_histid$histid)==TRUE),]
rm(census_1920)
save(restricted_1920, file="intermediate_outputs/extract_census_samples/restricted/restricted_1920.rda")
rm(restricted_1920)
rm(list=ls())
gc()

#1910
load("intermediate_outputs/histid_1910_matches.rda")
census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
colnames(census_1910)=tolower(colnames(census_1910))
public_1910=census_1910[which((census_1910$histid %in% matches_1910)==TRUE),]
serial_1910=unique(census_1910$serial[which(census_1910$histid %in% matches_1910)])
households_1910_histid=census_1910[which(census_1910$serial %in% serial_1910), c("histid", "serial")]
save(households_1910_histid, file="intermediate_outputs/extract_census_samples/households_1910.rda")
public_1910=census_1910[which((census_1910$serial %in% serial_1910)==TRUE),]
rm(census_1910)
save(public_1910, file="intermediate_outputs/extract_census_samples/public/public_1910.rda")
census_1910 <- as.data.frame(read.csv("input_data/census_restricted/census_1910.csv"))
colnames(census_1910)=tolower(colnames(census_1910))
restricted_1910=census_1910[which((census_1910$histid %in% households_1910_histid$histid)==TRUE),]
rm(census_1910)
save(restricted_1910, file="intermediate_outputs/extract_census_samples/restricted/restricted_1910.rda")
rm(restricted_1910)
rm(list=ls())
gc()

#1900
load("intermediate_outputs/histid_1900_matches.rda")
census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
census_1900=as.data.frame(census_1900)
colnames(census_1900)=tolower(colnames(census_1900))
public_1900=census_1900[which((census_1900$histid %in% matches_1900)==TRUE),]
serial_1900=unique(census_1900$serial[which(census_1900$histid %in% matches_1900)])
households_1900_histid=census_1900[which(census_1900$serial %in% serial_1900), c("histid", "serial")]
save(households_1900_histid, file="intermediate_outputs/extract_census_samples/households_1900.rda")
public_1900=census_1900[which((census_1900$serial %in% serial_1900)==TRUE),]
rm(census_1900)
save(public_1900, file="intermediate_outputs/extract_census_samples/public/public_1900.rda")
census_1900 <- as.data.frame(read.csv("input_data/census_restricted/census_1900.csv"))
colnames(census_1900)=tolower(colnames(census_1900))
restricted_1900=census_1900[which((census_1900$histid %in% households_1900_histid$histid)==TRUE),]
rm(census_1900)
save(restricted_1900, file="intermediate_outputs/extract_census_samples/restricted/restricted_1900.rda")
rm(restricted_1900)
rm(list=ls())
gc()