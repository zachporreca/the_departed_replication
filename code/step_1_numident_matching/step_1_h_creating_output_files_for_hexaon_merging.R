########################################################################################################
################### Step 1h: Creating output files for matching to hexagons ############################
########################################################################################################

library(haven)

load("intermediate_outputs/matched_records/cohort_1900.rda")
load("intermediate_outputs/matched_records/cohort_1910.rda")
load("intermediate_outputs/matched_records/cohort_1940.rda")
load("intermediate_outputs/matched_records/cohort_1930.rda")
load("intermediate_outputs/matched_records/cohort_1920.rda")

load("intermediate_outputs/sicilian_numident.rda")
(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940))/nrow(numident) #19.4% match rate
(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940)) #9827 mori matched


histid_1900=unique(cohort_1900$histid_ct_1900)
histid_1910=append(unique(cohort_1900$histid_ct_1910), unique(cohort_1910$histid_ct_1910))
histid_1920=append(append(unique(cohort_1900$histid_ct_1920), unique(cohort_1910$histid_ct_1920)), unique(cohort_1920$histid_ct_1920))
histid_1930=append(append(append(unique(cohort_1900$histid_ct_1930), unique(cohort_1910$histid_ct_1930)), unique(cohort_1920$histid_ct_1930)), unique(cohort_1930$histid_ct_1930))
histid_1940=append(append(append(append(unique(cohort_1900$histid_ct_1940), unique(cohort_1910$histid_ct_1940)), unique(cohort_1920$histid_ct_1940)), unique(cohort_1930$histid_ct_1940)), unique(cohort_1940$histid_ct_1940))

histid_1900=histid_1900[which(is.na(histid_1900)==FALSE)]
histid_1910=histid_1910[which(is.na(histid_1910)==FALSE)]
histid_1920=histid_1920[which(is.na(histid_1920)==FALSE)]
histid_1930=histid_1930[which(is.na(histid_1930)==FALSE)]
histid_1940=histid_1940[which(is.na(histid_1940)==FALSE)]

census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
colnames(census_1900)=tolower(colnames(census_1900))
matched_1900=census_1900[which((census_1900$histid %in% histid_1900)),]
matched_1900=census_1900[which((census_1900$histid %in% histid_1900)|(census_1900$serial %in% unique(matched_1900$serial))),]
unique(matched_1900$serial)
rm(census_1900)
gc()

census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
colnames(census_1910)=tolower(colnames(census_1910))
matched_1910=census_1910[which((census_1910$histid %in% histid_1910)),]
matched_1910=census_1910[which((census_1910$histid %in% histid_1910)|(census_1910$serial %in% unique(matched_1910$serial))),]
unique(matched_1910$serial)
rm(census_1910)
gc()

census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
colnames(census_1920)=tolower(colnames(census_1920))
matched_1920=census_1920[which((census_1920$histid %in% histid_1920)),]
matched_1920=census_1920[which((census_1920$histid %in% histid_1920)|(census_1920$serial %in% unique(matched_1920$serial))),]
unique(matched_1920$serial)
rm(census_1920)
gc()

census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
colnames(census_1930)=tolower(colnames(census_1930))
matched_1930=census_1930[which((census_1930$histid %in% histid_1930)),]
matched_1930=census_1930[which((census_1930$histid %in% histid_1930)|(census_1930$serial %in% unique(matched_1930$serial))),]
unique(matched_1930$serial)
rm(census_1930)
gc()

census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))
colnames(census_1940)=tolower(colnames(census_1940))
matched_1940=census_1940[which((census_1940$histid %in% histid_1940)),]
matched_1940=census_1940[which((census_1940$histid %in% histid_1940)|(census_1940$serial %in% unique(matched_1940$serial))),]
unique(matched_1940$serial)
rm(census_1940)
gc()

#adding mori vs sicilian info
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident_1900=numident[which(numident$birth_year<1900),]
numident_1900$cohort_member_id=seq(1:nrow(numident_1900))
cohort_1900$mori=numident_1900[match(cohort_1900$cohort_member_id, numident_1900$cohort_member_id), "mori"]
cohort_1900$cutrera_1900=numident_1900[match(cohort_1900$cohort_member_id, numident_1900$cohort_member_id), "cutrera_1900"]
cohort_1900$damiani_1885=numident_1900[match(cohort_1900$cohort_member_id, numident_1900$cohort_member_id), "damiani_1885"]
cohort_1900$cutrera_or_damiani_maps=numident_1900[match(cohort_1900$cohort_member_id, numident_1900$cohort_member_id), "cutrera_or_damiani_maps"]
numident_1910=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]
numident_1910$cohort_member_id=seq(1:nrow(numident_1910))
cohort_1910$mori=numident_1910[match(cohort_1910$cohort_member_id, numident_1910$cohort_member_id), "mori"]
cohort_1910$cutrera_1900=numident_1900[match(cohort_1910$cohort_member_id, numident_1910$cohort_member_id), "cutrera_1900"]
cohort_1910$damiani_1885=numident_1900[match(cohort_1910$cohort_member_id, numident_1910$cohort_member_id), "damiani_1885"]
cohort_1910$cutrera_or_damiani_maps=numident_1900[match(cohort_1910$cohort_member_id, numident_1910$cohort_member_id), "cutrera_or_damiani_maps"]
numident_1920=numident[which(numident$birth_year>=1910 & numident$birth_year<1920),]
numident_1920$cohort_member_id=seq(1:nrow(numident_1920))
cohort_1920$mori=numident_1920[match(cohort_1920$cohort_member_id, numident_1920$cohort_member_id), "mori"]
cohort_1920$cutrera_1900=numident_1900[match(cohort_1920$cohort_member_id, numident_1920$cohort_member_id), "cutrera_1900"]
cohort_1920$damiani_1885=numident_1900[match(cohort_1920$cohort_member_id, numident_1920$cohort_member_id), "damiani_1885"]
cohort_1920$cutrera_or_damiani_maps=numident_1900[match(cohort_1920$cohort_member_id, numident_1920$cohort_member_id), "cutrera_or_damiani_maps"]
numident_1930=numident[which(numident$birth_year>=1920 & numident$birth_year<1930),]
numident_1930$cohort_member_id=seq(1:nrow(numident_1930))
cohort_1930$mori=numident_1930[match(cohort_1930$cohort_member_id, numident_1930$cohort_member_id), "mori"]
cohort_1930$cutrera_1900=numident_1900[match(cohort_1930$cohort_member_id, numident_1930$cohort_member_id), "cutrera_1900"]
cohort_1930$damiani_1885=numident_1900[match(cohort_1930$cohort_member_id, numident_1930$cohort_member_id), "damiani_1885"]
cohort_1930$cutrera_or_damiani_maps=numident_1900[match(cohort_1930$cohort_member_id, numident_1930$cohort_member_id), "cutrera_or_damiani_maps"]
numident_1940=numident[which(numident$birth_year>=1930 & numident$birth_year<1940),]
numident_1940$cohort_member_id=seq(1:nrow(numident_1940))
cohort_1940$mori=numident_1940[match(cohort_1940$cohort_member_id, numident_1940$cohort_member_id), "mori"]
cohort_1940$cutrera_1900=numident_1900[match(cohort_1940$cohort_member_id, numident_1940$cohort_member_id), "cutrera_1900"]
cohort_1940$damiani_1885=numident_1900[match(cohort_1940$cohort_member_id, numident_1940$cohort_member_id), "damiani_1885"]
cohort_1940$cutrera_or_damiani_maps=numident_1900[match(cohort_1940$cohort_member_id, numident_1940$cohort_member_id), "cutrera_or_damiani_maps"]
(sum(cohort_1900$mori)+sum(cohort_1910$mori)+sum(cohort_1920$mori)+sum(cohort_1930$mori)+sum(cohort_1940$mori))/(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940))
#23.3% of matches are mori, 21.8% of numident are mori. consistant match rate between all sicilians and mori sicilians
(sum(cohort_1900$cutrera_1900)+sum(cohort_1910$cutrera_1900)+sum(cohort_1920$cutrera_1900)+sum(cohort_1930$cutrera_1900)+sum(cohort_1940$cutrera_1900))/(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940))
#8.8% of matches are cutrera_1900, 9.1% of numident are cutrera_1900. consistant match rate between all sicilians and cutrera_1900 sicilians
(sum(cohort_1900$damiani_1885)+sum(cohort_1910$damiani_1885)+sum(cohort_1920$damiani_1885)+sum(cohort_1930$damiani_1885)+sum(cohort_1940$damiani_1885))/(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940))
#8.8% of matches are damiani_1885, 9.1% of numident are damiani_1885. consistant match rate between all sicilians and damiani_1885 sicilians
(sum(cohort_1900$cutrera_or_damiani_maps)+sum(cohort_1910$cutrera_or_damiani_maps)+sum(cohort_1920$cutrera_or_damiani_maps)+sum(cohort_1930$cutrera_or_damiani_maps)+sum(cohort_1940$cutrera_or_damiani_maps))/(nrow(cohort_1900)+nrow(cohort_1910)+nrow(cohort_1920)+nrow(cohort_1930)+nrow(cohort_1940))
#8.8% of matches are cutrera_or_damiani_maps, 9.1% of numident are cutrera_or_damiani_maps. consistant match rate between all sicilians and cutrera_or_damiani_maps sicilians

#replicating data files from previous hex panel build and adding sicilian/mori info
sicilians_matched_1900=matched_1900[which(matched_1900$histid %in% histid_1900),] #mori and non
sicilians_matched_1910=matched_1910[which(matched_1910$histid %in% histid_1910),] #mori and non
sicilians_matched_1920=matched_1920[which(matched_1920$histid %in% histid_1920),] #mori and non
sicilians_matched_1930=matched_1930[which(matched_1930$histid %in% histid_1930),] #mori and non
sicilians_matched_1940=matched_1940[which(matched_1940$histid %in% histid_1940),] #mori and non


mori_histid_1900=unique(cohort_1900[which(cohort_1900$mori==1),]$histid_ct_1900)
mori_histid_1910=append(unique(cohort_1900[which(cohort_1900$mori==1),]$histid_ct_1910), unique(cohort_1910[which(cohort_1910$mori==1),]$histid_ct_1910))
mori_histid_1920=append(append(unique(cohort_1900[which(cohort_1900$mori==1),]$histid_ct_1920), unique(cohort_1910[which(cohort_1910$mori==1),]$histid_ct_1920)), unique(cohort_1920[which(cohort_1920$mori==1),]$histid_ct_1920))
mori_histid_1930=append(append(append(unique(cohort_1900[which(cohort_1900$mori==1),]$histid_ct_1930), unique(cohort_1910[which(cohort_1910$mori==1),]$histid_ct_1930)), unique(cohort_1920[which(cohort_1920$mori==1),]$histid_ct_1930)), unique(cohort_1930[which(cohort_1930$mori==1),]$histid_ct_1930))
mori_histid_1940=append(append(append(append(unique(cohort_1900[which(cohort_1900$mori==1),]$histid_ct_1940), unique(cohort_1910[which(cohort_1910$mori==1),]$histid_ct_1940)), unique(cohort_1920[which(cohort_1920$mori==1),]$histid_ct_1940)), unique(cohort_1930[which(cohort_1930$mori==1),]$histid_ct_1940)), unique(cohort_1940[which(cohort_1940$mori==1),]$histid_ct_1940))

mori_matched_1900=matched_1900[which(matched_1900$histid %in% mori_histid_1900),] #mori
mori_matched_1910=matched_1910[which(matched_1910$histid %in% mori_histid_1910),] #mori
mori_matched_1920=matched_1920[which(matched_1920$histid %in% mori_histid_1920),] #mori
mori_matched_1930=matched_1930[which(matched_1930$histid %in% mori_histid_1930),] #mori
mori_matched_1940=matched_1940[which(matched_1940$histid %in% mori_histid_1940),] #mori


cutrera_1900_histid_1900=unique(cohort_1900[which(cohort_1900$cutrera_1900==1),]$histid_ct_1900)
cutrera_1900_histid_1910=append(unique(cohort_1900[which(cohort_1900$cutrera_1900==1),]$histid_ct_1910), unique(cohort_1910[which(cohort_1910$cutrera_1900==1),]$histid_ct_1910))
cutrera_1900_histid_1920=append(append(unique(cohort_1900[which(cohort_1900$cutrera_1900==1),]$histid_ct_1920), unique(cohort_1910[which(cohort_1910$cutrera_1900==1),]$histid_ct_1920)), unique(cohort_1920[which(cohort_1920$cutrera_1900==1),]$histid_ct_1920))
cutrera_1900_histid_1930=append(append(append(unique(cohort_1900[which(cohort_1900$cutrera_1900==1),]$histid_ct_1930), unique(cohort_1910[which(cohort_1910$cutrera_1900==1),]$histid_ct_1930)), unique(cohort_1920[which(cohort_1920$cutrera_1900==1),]$histid_ct_1930)), unique(cohort_1930[which(cohort_1930$cutrera_1900==1),]$histid_ct_1930))
cutrera_1900_histid_1940=append(append(append(append(unique(cohort_1900[which(cohort_1900$cutrera_1900==1),]$histid_ct_1940), unique(cohort_1910[which(cohort_1910$cutrera_1900==1),]$histid_ct_1940)), unique(cohort_1920[which(cohort_1920$cutrera_1900==1),]$histid_ct_1940)), unique(cohort_1930[which(cohort_1930$cutrera_1900==1),]$histid_ct_1940)), unique(cohort_1940[which(cohort_1940$cutrera_1900==1),]$histid_ct_1940))

cutrera_1900_matched_1900=matched_1900[which(matched_1900$histid %in% cutrera_1900_histid_1900),] #cutrera_1900
cutrera_1900_matched_1910=matched_1910[which(matched_1910$histid %in% cutrera_1900_histid_1910),] #cutrera_1900
cutrera_1900_matched_1920=matched_1920[which(matched_1920$histid %in% cutrera_1900_histid_1920),] #cutrera_1900
cutrera_1900_matched_1930=matched_1930[which(matched_1930$histid %in% cutrera_1900_histid_1930),] #cutrera_1900
cutrera_1900_matched_1940=matched_1940[which(matched_1940$histid %in% cutrera_1900_histid_1940),] #cutrera_1900

damiani_1885_histid_1900=unique(cohort_1900[which(cohort_1900$damiani_1885==1),]$histid_ct_1900)
damiani_1885_histid_1910=append(unique(cohort_1900[which(cohort_1900$damiani_1885==1),]$histid_ct_1910), unique(cohort_1910[which(cohort_1910$damiani_1885==1),]$histid_ct_1910))
damiani_1885_histid_1920=append(append(unique(cohort_1900[which(cohort_1900$damiani_1885==1),]$histid_ct_1920), unique(cohort_1910[which(cohort_1910$damiani_1885==1),]$histid_ct_1920)), unique(cohort_1920[which(cohort_1920$damiani_1885==1),]$histid_ct_1920))
damiani_1885_histid_1930=append(append(append(unique(cohort_1900[which(cohort_1900$damiani_1885==1),]$histid_ct_1930), unique(cohort_1910[which(cohort_1910$damiani_1885==1),]$histid_ct_1930)), unique(cohort_1920[which(cohort_1920$damiani_1885==1),]$histid_ct_1930)), unique(cohort_1930[which(cohort_1930$damiani_1885==1),]$histid_ct_1930))
damiani_1885_histid_1940=append(append(append(append(unique(cohort_1900[which(cohort_1900$damiani_1885==1),]$histid_ct_1940), unique(cohort_1910[which(cohort_1910$damiani_1885==1),]$histid_ct_1940)), unique(cohort_1920[which(cohort_1920$damiani_1885==1),]$histid_ct_1940)), unique(cohort_1930[which(cohort_1930$damiani_1885==1),]$histid_ct_1940)), unique(cohort_1940[which(cohort_1940$damiani_1885==1),]$histid_ct_1940))

damiani_1885_matched_1900=matched_1900[which(matched_1900$histid %in% damiani_1885_histid_1900),] #damiani_1885
damiani_1885_matched_1910=matched_1910[which(matched_1910$histid %in% damiani_1885_histid_1910),] #damiani_1885
damiani_1885_matched_1920=matched_1920[which(matched_1920$histid %in% damiani_1885_histid_1920),] #damiani_1885
damiani_1885_matched_1930=matched_1930[which(matched_1930$histid %in% damiani_1885_histid_1930),] #damiani_1885
damiani_1885_matched_1940=matched_1940[which(matched_1940$histid %in% damiani_1885_histid_1940),] #damiani_1885

cutrera_or_damiani_maps_histid_1900=unique(cohort_1900[which(cohort_1900$cutrera_or_damiani_maps==1),]$histid_ct_1900)
cutrera_or_damiani_maps_histid_1910=append(unique(cohort_1900[which(cohort_1900$cutrera_or_damiani_maps==1),]$histid_ct_1910), unique(cohort_1910[which(cohort_1910$cutrera_or_damiani_maps==1),]$histid_ct_1910))
cutrera_or_damiani_maps_histid_1920=append(append(unique(cohort_1900[which(cohort_1900$cutrera_or_damiani_maps==1),]$histid_ct_1920), unique(cohort_1910[which(cohort_1910$cutrera_or_damiani_maps==1),]$histid_ct_1920)), unique(cohort_1920[which(cohort_1920$cutrera_or_damiani_maps==1),]$histid_ct_1920))
cutrera_or_damiani_maps_histid_1930=append(append(append(unique(cohort_1900[which(cohort_1900$cutrera_or_damiani_maps==1),]$histid_ct_1930), unique(cohort_1910[which(cohort_1910$cutrera_or_damiani_maps==1),]$histid_ct_1930)), unique(cohort_1920[which(cohort_1920$cutrera_or_damiani_maps==1),]$histid_ct_1930)), unique(cohort_1930[which(cohort_1930$cutrera_or_damiani_maps==1),]$histid_ct_1930))
cutrera_or_damiani_maps_histid_1940=append(append(append(append(unique(cohort_1900[which(cohort_1900$cutrera_or_damiani_maps==1),]$histid_ct_1940), unique(cohort_1910[which(cohort_1910$cutrera_or_damiani_maps==1),]$histid_ct_1940)), unique(cohort_1920[which(cohort_1920$cutrera_or_damiani_maps==1),]$histid_ct_1940)), unique(cohort_1930[which(cohort_1930$cutrera_or_damiani_maps==1),]$histid_ct_1940)), unique(cohort_1940[which(cohort_1940$cutrera_or_damiani_maps==1),]$histid_ct_1940))

cutrera_or_damiani_maps_matched_1900=matched_1900[which(matched_1900$histid %in% cutrera_or_damiani_maps_histid_1900),] #cutrera_or_damiani_maps
cutrera_or_damiani_maps_matched_1910=matched_1910[which(matched_1910$histid %in% cutrera_or_damiani_maps_histid_1910),] #cutrera_or_damiani_maps
cutrera_or_damiani_maps_matched_1920=matched_1920[which(matched_1920$histid %in% cutrera_or_damiani_maps_histid_1920),] #cutrera_or_damiani_maps
cutrera_or_damiani_maps_matched_1930=matched_1930[which(matched_1930$histid %in% cutrera_or_damiani_maps_histid_1930),] #cutrera_or_damiani_maps
cutrera_or_damiani_maps_matched_1940=matched_1940[which(matched_1940$histid %in% cutrera_or_damiani_maps_histid_1940),] #cutrera_or_damiani_maps

non_mori_histid_1900=unique(cohort_1900[which(cohort_1900$mori==0),]$histid_ct_1900)
non_mori_histid_1910=append(unique(cohort_1900[which(cohort_1900$mori==0),]$histid_ct_1910), unique(cohort_1910[which(cohort_1910$mori==0),]$histid_ct_1910))
non_mori_histid_1920=append(append(unique(cohort_1900[which(cohort_1900$mori==0),]$histid_ct_1920), unique(cohort_1910[which(cohort_1910$mori==0),]$histid_ct_1920)), unique(cohort_1920[which(cohort_1920$mori==0),]$histid_ct_1920))
non_mori_histid_1930=append(append(append(unique(cohort_1900[which(cohort_1900$mori==0),]$histid_ct_1930), unique(cohort_1910[which(cohort_1910$mori==0),]$histid_ct_1930)), unique(cohort_1920[which(cohort_1920$mori==0),]$histid_ct_1930)), unique(cohort_1930[which(cohort_1930$mori==0),]$histid_ct_1930))
non_mori_histid_1940=append(append(append(append(unique(cohort_1900[which(cohort_1900$mori==0),]$histid_ct_1940), unique(cohort_1910[which(cohort_1910$mori==0),]$histid_ct_1940)), unique(cohort_1920[which(cohort_1920$mori==0),]$histid_ct_1940)), unique(cohort_1930[which(cohort_1930$mori==0),]$histid_ct_1940)), unique(cohort_1940[which(cohort_1940$mori==0),]$histid_ct_1940))

non_mori_matched_1900=matched_1900[which(matched_1900$histid %in% non_mori_histid_1900),] #non
non_mori_matched_1910=matched_1910[which(matched_1910$histid %in% non_mori_histid_1910),] #non
non_mori_matched_1920=matched_1920[which(matched_1920$histid %in% non_mori_histid_1920),] #non
non_mori_matched_1930=matched_1930[which(matched_1930$histid %in% non_mori_histid_1930),] #non
non_mori_matched_1940=matched_1940[which(matched_1940$histid %in% non_mori_histid_1940),] #non


household_sample_mori_1900=matched_1900[which(matched_1900$serial %in% unique(mori_matched_1900$serial)),]
household_sample_mori_1910=matched_1910[which(matched_1910$serial %in% unique(mori_matched_1910$serial)),]
household_sample_mori_1920=matched_1920[which(matched_1920$serial %in% unique(mori_matched_1920$serial)),]
household_sample_mori_1930=matched_1930[which(matched_1930$serial %in% unique(mori_matched_1930$serial)),]
household_sample_mori_1940=matched_1940[which(matched_1940$serial %in% unique(mori_matched_1940$serial)),]

household_sample_non_mori_1900=matched_1900[which(matched_1900$serial %in% unique(non_mori_matched_1900$serial)),]
household_sample_non_mori_1910=matched_1910[which(matched_1910$serial %in% unique(non_mori_matched_1910$serial)),]
household_sample_non_mori_1920=matched_1920[which(matched_1920$serial %in% unique(non_mori_matched_1920$serial)),]
household_sample_non_mori_1930=matched_1930[which(matched_1930$serial %in% unique(non_mori_matched_1930$serial)),]
household_sample_non_mori_1940=matched_1940[which(matched_1940$serial %in% unique(non_mori_matched_1940$serial)),]

household_sample_cutrera_1900_1900=matched_1900[which(matched_1900$serial %in% unique(cutrera_1900_matched_1900$serial)),]
household_sample_cutrera_1900_1910=matched_1910[which(matched_1910$serial %in% unique(cutrera_1900_matched_1910$serial)),]
household_sample_cutrera_1900_1920=matched_1920[which(matched_1920$serial %in% unique(cutrera_1900_matched_1920$serial)),]
household_sample_cutrera_1900_1930=matched_1930[which(matched_1930$serial %in% unique(cutrera_1900_matched_1930$serial)),]
household_sample_cutrera_1900_1940=matched_1940[which(matched_1940$serial %in% unique(cutrera_1900_matched_1940$serial)),]

household_sample_damiani_1885_1900=matched_1900[which(matched_1900$serial %in% unique(damiani_1885_matched_1900$serial)),]
household_sample_damiani_1885_1910=matched_1910[which(matched_1910$serial %in% unique(damiani_1885_matched_1910$serial)),]
household_sample_damiani_1885_1920=matched_1920[which(matched_1920$serial %in% unique(damiani_1885_matched_1920$serial)),]
household_sample_damiani_1885_1930=matched_1930[which(matched_1930$serial %in% unique(damiani_1885_matched_1930$serial)),]
household_sample_damiani_1885_1940=matched_1940[which(matched_1940$serial %in% unique(damiani_1885_matched_1940$serial)),]

household_sample_cutrera_or_damiani_maps_1900=matched_1900[which(matched_1900$serial %in% unique(cutrera_or_damiani_maps_matched_1900$serial)),]
household_sample_cutrera_or_damiani_maps_1910=matched_1910[which(matched_1910$serial %in% unique(cutrera_or_damiani_maps_matched_1910$serial)),]
household_sample_cutrera_or_damiani_maps_1920=matched_1920[which(matched_1920$serial %in% unique(cutrera_or_damiani_maps_matched_1920$serial)),]
household_sample_cutrera_or_damiani_maps_1930=matched_1930[which(matched_1930$serial %in% unique(cutrera_or_damiani_maps_matched_1930$serial)),]
household_sample_cutrera_or_damiani_maps_1940=matched_1940[which(matched_1940$serial %in% unique(cutrera_or_damiani_maps_matched_1940$serial)),]

household_sample_1900=matched_1900
household_sample_1910=matched_1910
household_sample_1920=matched_1920
household_sample_1930=matched_1930
household_sample_1940=matched_1940

save(household_sample_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_1900.rda")
save(household_sample_mori_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_mori_1900.rda")
save(household_sample_non_mori_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_non_mori_1900.rda")
save(household_sample_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_1910.rda")
save(household_sample_mori_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_mori_1910.rda")
save(household_sample_non_mori_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_non_mori_1910.rda")
save(household_sample_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_1920.rda")
save(household_sample_mori_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_mori_1920.rda")
save(household_sample_non_mori_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_non_mori_1920.rda")
save(household_sample_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_1930.rda")
save(household_sample_mori_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_mori_1930.rda")
save(household_sample_non_mori_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_non_mori_1930.rda")
save(household_sample_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_1940.rda")
save(household_sample_mori_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_mori_1940.rda")
save(household_sample_non_mori_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_non_mori_1940.rda")
save(household_sample_cutrera_1900_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1900.rda")
save(household_sample_cutrera_1900_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1910.rda")
save(household_sample_cutrera_1900_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1920.rda")
save(household_sample_cutrera_1900_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1930.rda")
save(household_sample_cutrera_1900_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1940.rda")
save(household_sample_damiani_1885_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1900.rda")
save(household_sample_damiani_1885_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1910.rda")
save(household_sample_damiani_1885_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1920.rda")
save(household_sample_damiani_1885_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1930.rda")
save(household_sample_damiani_1885_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1940.rda")
save(household_sample_cutrera_or_damiani_maps_1900, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1900.rda")
save(household_sample_cutrera_or_damiani_maps_1910, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1910.rda")
save(household_sample_cutrera_or_damiani_maps_1920, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1920.rda")
save(household_sample_cutrera_or_damiani_maps_1930, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1930.rda")
save(household_sample_cutrera_or_damiani_maps_1940, file="intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1940.rda")

rm(list = ls())
gc()

# ---- 1900 ----
load("intermediate_outputs/outputs_for_hexagons/household_sample_1900.rda")
load("intermediate_outputs/outputs_for_hexagons/household_sample_mori_1900.rda")
household_sample_1900$mori <- as.integer(household_sample_1900$histid %in% household_sample_mori_1900$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1900.rda")
household_sample_1900$cutrera_1900 <- as.integer(household_sample_1900$histid %in% household_sample_cutrera_1900_1900$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1900.rda")
household_sample_1900$damiani_1885 <- as.integer(household_sample_1900$histid %in% household_sample_damiani_1885_1900$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1900.rda")
household_sample_1900$cutrera_or_damiani_maps <- as.integer(household_sample_1900$histid %in% household_sample_cutrera_or_damiani_maps_1900$histid)

save(household_sample_1900, file = "intermediate_outputs/outputs_for_hexagons/household_sample_1900.rda")

# ---- 1910 ----
load("intermediate_outputs/outputs_for_hexagons/household_sample_1910.rda")
load("intermediate_outputs/outputs_for_hexagons/household_sample_mori_1910.rda")
household_sample_1910$mori <- as.integer(household_sample_1910$histid %in% household_sample_mori_1910$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1910.rda")
household_sample_1910$cutrera_1900 <- as.integer(household_sample_1910$histid %in% household_sample_cutrera_1900_1910$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1910.rda")
household_sample_1910$damiani_1885 <- as.integer(household_sample_1910$histid %in% household_sample_damiani_1885_1910$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1910.rda")
household_sample_1910$cutrera_or_damiani_maps <- as.integer(household_sample_1910$histid %in% household_sample_cutrera_or_damiani_maps_1910$histid)

save(household_sample_1910, file = "intermediate_outputs/outputs_for_hexagons/household_sample_1910.rda")

# ---- 1920 ----
load("intermediate_outputs/outputs_for_hexagons/household_sample_1920.rda")
load("intermediate_outputs/outputs_for_hexagons/household_sample_mori_1920.rda")
household_sample_1920$mori <- as.integer(household_sample_1920$histid %in% household_sample_mori_1920$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1920.rda")
household_sample_1920$cutrera_1900 <- as.integer(household_sample_1920$histid %in% household_sample_cutrera_1900_1920$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1920.rda")
household_sample_1920$damiani_1885 <- as.integer(household_sample_1920$histid %in% household_sample_damiani_1885_1920$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1920.rda")
household_sample_1920$cutrera_or_damiani_maps <- as.integer(household_sample_1920$histid %in% household_sample_cutrera_or_damiani_maps_1920$histid)

save(household_sample_1920, file = "intermediate_outputs/outputs_for_hexagons/household_sample_1920.rda")

# ---- 1930 ----
load("intermediate_outputs/outputs_for_hexagons/household_sample_1930.rda")
load("intermediate_outputs/outputs_for_hexagons/household_sample_mori_1930.rda")
household_sample_1930$mori <- as.integer(household_sample_1930$histid %in% household_sample_mori_1930$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1930.rda")
household_sample_1930$cutrera_1900 <- as.integer(household_sample_1930$histid %in% household_sample_cutrera_1900_1930$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1930.rda")
household_sample_1930$damiani_1885 <- as.integer(household_sample_1930$histid %in% household_sample_damiani_1885_1930$histid)
load("intermediate_outputs/outputs_for_hexagons/ousehold_sample_cutrera_or_damiani_maps_1930.rda")
household_sample_1930$cutrera_or_damiani_maps <- as.integer(household_sample_1930$histid %in% household_sample_cutrera_or_damiani_maps_1930$histid)

save(household_sample_1930, file = "intermediate_outputs/outputs_for_hexagons/household_sample_1930.rda")

# ---- 1940 ----
load("intermediate_outputs/outputs_for_hexagons/household_sample_1940.rda")
load("intermediate_outputs/outputs_for_hexagons/household_sample_mori_1940.rda")
household_sample_1940$mori <- as.integer(household_sample_1940$histid %in% household_sample_mori_1940$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_1900_1940.rda")
household_sample_1940$cutrera_1900 <- as.integer(household_sample_1940$histid %in% household_sample_cutrera_1900_1940$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_damiani_1885_1940.rda")
household_sample_1940$damiani_1885 <- as.integer(household_sample_1940$histid %in% household_sample_damiani_1885_1940$histid)
load("intermediate_outputs/outputs_for_hexagons/household_sample_cutrera_or_damiani_maps_1940.rda")
household_sample_1940$cutrera_or_damiani_maps <- as.integer(household_sample_1940$histid %in% household_sample_cutrera_or_damiani_maps_1940$histid)

save(household_sample_1940, file = "intermediate_outputs/outputs_for_hexagons/household_sample_1940.rda")

common_cols <- Reduce(intersect, list(names(household_sample_1900), names(household_sample_1910), names(household_sample_1920), names(household_sample_1930), names(household_sample_1940)))
final_cols <- common_cols[-(42:46)]
household_sample_1900 <- household_sample_1900[ , final_cols]
household_sample_1910 <- household_sample_1910[ , final_cols]
household_sample_1920 <- household_sample_1920[ , final_cols]
household_sample_1930 <- household_sample_1930[ , final_cols]
household_sample_1940 <- household_sample_1940[ , final_cols]

household_sample <- rbind(
  household_sample_1900,
  household_sample_1910,
  household_sample_1920,
  household_sample_1930,
  household_sample_1940
)
household_sample=as.data.frame(household_sample)
save(household_sample, file = "intermediate_outputs/outputs_for_hexagons/household_sample.rda") #142693 records. 

rm(list=ls())

gc()