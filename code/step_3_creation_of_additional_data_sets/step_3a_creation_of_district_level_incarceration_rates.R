#################################################################################################################################
#################### STEP 3a: CONSTRUCTION OF ENUMERATION DISTRICT LEVEL INCARCERATION RATES. THIS TAKES        #################
####################          A LIST OF CENSUS RECORDS IDENTIFIED AS INCARCERATED PROIVED BY THE TEAM BEHND     #################
####################          ABRARMITZKY ET AL (2024) AND CENSUS LINKAGES AS INPUT DATA                        ################
#################################################################################################################################

library(haven)


#1910

load("input_data/step_3_input_data/prisoner_records.rda")
#1910 rates calculation
prisoner_records_1910=prisoner_records[which(prisoner_records$year==1910),]
prisoner_records_1900=prisoner_records[which(prisoner_records$year==1900),]
census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
abe_cross=read.csv(file="input_data/step_3_input_data/abe_crosswalks/crosswalk_1900_1910_csv/crosswalk_1900_1910.csv", header = TRUE)
census_tree=read.csv(file="input_data/census_tree_crosswalks/1900_1910.csv", header = TRUE)

gc()
colnames(census_1900)=tolower(colnames(census_1900))
colnames(census_1910)=tolower(colnames(census_1910))

#first abe
#generating ed ID
census_1900$ed_id=paste0(census_1900$stateicp,"_",census_1900$countyicp, "_", census_1900$enumdist)




#incarcerated subsamples
incarcerated_1900=census_1900[which(census_1900$histid %in% prisoner_records_1900$histid),] 
incarcerated_1910=census_1910[which(census_1910$histid %in% prisoner_records_1910$histid),]

gc()

#limiting to standard ABE crosswalk
abe_cross=abe_cross[which(abe_cross[,4]==1),]
census_1910$id_1900=abe_cross[match(census_1910$histid, abe_cross[,2]),1] #histid match
incarcerated_1910$id_1900=abe_cross[match(incarcerated_1910$histid, abe_cross[,2]),1] #histid match

#subsetting for match, those with a match across census
census_1910_identifiable=census_1910[which(is.na(census_1910$id_1900)==FALSE),]
#incarcaerated subset of those with across census match
incarcerated_1910=incarcerated_1910[which(is.na(incarcerated_1910$id_1900)==FALSE),]

#incarecerated subsample that was not incarcerated at last census
incarcerated_1910=incarcerated_1910[which((incarcerated_1910$id_1900 %in% incarcerated_1900$histid)==FALSE),]

#prepping ABE match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1900$ed_id)))
ed_incarceration_rates[,1]=unique(census_1900$ed_id)
census_1900_linkable=census_1900[which((census_1900$histid %in% census_1910$id_1900)==TRUE),]
census_1900_linkable=census_1900_linkable[which((census_1900_linkable$histid %in% incarcerated_1900$histid)==FALSE),]
incarcerated_1910$ed_id=census_1900_linkable[match(incarcerated_1910$id_1900, census_1900_linkable$histid),"ed_id"]

#populating matrix
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1910[which(incarcerated_1910$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1900_linkable[which(census_1900_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_abe=ed_incarceration_rates
save(ed_incarceration_rates_abe, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_abe_1910.rda")

#IPUMS Crosswalk second

incarcerated_1900=census_1900[which(census_1900$histid %in% prisoner_records_1900$histid),] 
incarcerated_1910=census_1910[which(census_1910$histid %in% prisoner_records_1910$histid),]
incarcerated_1910_link_ids=unique(incarcerated_1910[which(incarcerated_1910$link1900==1), "hik"]) #NOW HIK
#removing those incarcerated in 1900
incarcerated_1910_link_ids=incarcerated_1910_link_ids[which((incarcerated_1910_link_ids %in% 
                                                               incarcerated_1900$hik)==FALSE)]
incarcerated_1910_linkable_1900_census=census_1900[which((census_1900$hik %in% incarcerated_1910_link_ids)==TRUE),]
all_1910_link_ids=unique(census_1910[which(census_1910$link1900==1), "hik"])
all_1910_linkable_1900_census=census_1900[which((census_1900$hik %in% all_1910_link_ids)==TRUE),]
#excluding 1900 incarcerated
all_1910_linkable_1900_census=all_1910_linkable_1900_census[which((all_1910_linkable_1900_census$hik %in% 
                                                                     incarcerated_1900$hik)==FALSE),]

#building matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1900$ed_id)))
ed_incarceration_rates[,1]=unique(census_1900$ed_id)
gc()
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1910_linkable_1900_census[which(
    incarcerated_1910_linkable_1900_census$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(all_1910_linkable_1900_census[which(
    all_1910_linkable_1900_census$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_ipums=ed_incarceration_rates
save(ed_incarceration_rates_ipums, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_ipums_1910.rda")

#Census tree Crosswalk is third
rm(list=c("all_1910_linkable_1900_census", "census_1900_linkable", "census_1910_identifiable", 
          "incarcerated_1910_linkable_1900_census", "abe_cross"))
gc()

census_1910$id_1900=census_tree[match(census_1910$histid, census_tree[,2]),1] #histid match
incarcerated_1910$id_1900=census_tree[match(incarcerated_1910$histid, census_tree[,2]),1] #histid match
incarcerated_1910=incarcerated_1910[which(is.na(incarcerated_1910$id_1900)==FALSE),] #linkable
#not incarcerated last census
incarcerated_1910=incarcerated_1910[which((incarcerated_1910$id_1900 %in% incarcerated_1900$histid)==FALSE),]

#prepping census tree match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1900$ed_id)))
ed_incarceration_rates[,1]=unique(census_1900$ed_id)
census_1900_linkable=census_1900[which((census_1900$histid %in% census_1910$id_1900)==TRUE),]
census_1900_linkable=census_1900_linkable[which((census_1900_linkable$histid %in% incarcerated_1900$histid)==FALSE),]
incarcerated_1910$ed_id=census_1900_linkable[match(incarcerated_1910$id_1900, census_1900_linkable$histid),"ed_id"]
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

#populating matrix
timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1910[which(incarcerated_1910$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1900_linkable[which(census_1900_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_census_tree=ed_incarceration_rates
save(ed_incarceration_rates_census_tree, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_census_tree_1910.rda")


ed_incarceration_rates_1910=matrix(nrow=nrow(ed_incarceration_rates_abe), ncol=11)
ed_incarceration_rates_1910=as.data.frame(ed_incarceration_rates_1910)
colnames(ed_incarceration_rates_1910)=c("ed_state_county_id", "enum_dist", "num_incarc_abe", "pop_abe",
                                        "rate_abe", "num_incarc_ipums", "pop_ipums", "rate_ipums",
                                        "num_incarc_tree", "pop_tree", "rate_tree")
ed_incarceration_rates_1910[,1]=ed_incarceration_rates_abe[,1]
ed_incarceration_rates_1910[,2]=ed_incarceration_rates_abe[,5]
ed_incarceration_rates_1910[,3]=ed_incarceration_rates_abe[,2]
ed_incarceration_rates_1910[,4]=ed_incarceration_rates_abe[,3]
ed_incarceration_rates_1910[,5]=ed_incarceration_rates_abe[,4]
ed_incarceration_rates_1910[,6]=ed_incarceration_rates_ipums[,2]
ed_incarceration_rates_1910[,7]=ed_incarceration_rates_ipums[,3]
ed_incarceration_rates_1910[,8]=ed_incarceration_rates_ipums[,4]
ed_incarceration_rates_1910[,9]=ed_incarceration_rates_census_tree[,2]
ed_incarceration_rates_1910[,10]=ed_incarceration_rates_census_tree[,3]
ed_incarceration_rates_1910[,11]=ed_incarceration_rates_census_tree[,4]

save(ed_incarceration_rates_1910, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_1910.rda")
rm(list = ls())
gc()


#1920
load("input_data/step_3_input_data/prisoner_records.rda")

#1920 rates calculation
prisoner_records_1920=prisoner_records[which(prisoner_records$year==1920),]
prisoner_records_1910=prisoner_records[which(prisoner_records$year==1910),]
census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
abe_cross=read.csv(file="input_data/step_3_input_data/abe_crosswalks/crosswalk_1910_1920_csv/crosswalk_1910_1920.csv", header = TRUE)
census_tree=read.csv(file="input_data/census_tree_crosswalks/1910_1920.csv", header = TRUE)

gc()
colnames(census_1910)=tolower(colnames(census_1910))
colnames(census_1920)=tolower(colnames(census_1920))

#first abe
#generating ed ID
census_1910$ed_id=paste0(census_1910$stateicp,"_",census_1910$countyicp, "_", census_1910$enumdist)




#incarcerated subsamples
incarcerated_1910=census_1910[which(census_1910$histid %in% prisoner_records_1910$histid),] 
incarcerated_1920=census_1920[which(census_1920$histid %in% prisoner_records_1920$histid),]

gc()

#limiting to standard ABE crosswalk
abe_cross=abe_cross[which(abe_cross[,4]==1),]
census_1920$id_1910=abe_cross[match(census_1920$histid, abe_cross[,2]),1] #histid match
incarcerated_1920$id_1910=abe_cross[match(incarcerated_1920$histid, abe_cross[,2]),1] #histid match

#subsetting for match, those with a match across census
census_1920_identifiable=census_1920[which(is.na(census_1920$id_1910)==FALSE),]
#incarcaerated subset of those with across census match
incarcerated_1920=incarcerated_1920[which(is.na(incarcerated_1920$id_1910)==FALSE),]

#incarecerated subsample that was not incarcerated at last census
incarcerated_1920=incarcerated_1920[which((incarcerated_1920$id_1910 %in% incarcerated_1910$histid)==FALSE),]

#prepping ABE match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1910$ed_id)))
ed_incarceration_rates[,1]=unique(census_1910$ed_id)
census_1910_linkable=census_1910[which((census_1910$histid %in% census_1920$id_1910)==TRUE),]
census_1910_linkable=census_1910_linkable[which((census_1910_linkable$histid %in% incarcerated_1910$histid)==FALSE),]
incarcerated_1920$ed_id=census_1910_linkable[match(incarcerated_1920$id_1910, census_1910_linkable$histid),"ed_id"]

#populating matrix
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1920[which(incarcerated_1920$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1910_linkable[which(census_1910_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_abe=ed_incarceration_rates
save(ed_incarceration_rates_abe, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_abe_1920.rda")


#IPUMS Crosswalk second

incarcerated_1910=census_1910[which(census_1910$histid %in% prisoner_records_1910$histid),] 
incarcerated_1920=census_1920[which(census_1920$histid %in% prisoner_records_1920$histid),]
incarcerated_1920_link_ids=unique(incarcerated_1920[which(incarcerated_1920$link1910==1), "hik"]) #NOW HIK
#removing those incarcerated in 1910
incarcerated_1920_link_ids=incarcerated_1920_link_ids[which((incarcerated_1920_link_ids %in% 
                                                               incarcerated_1910$hik)==FALSE)]
incarcerated_1920_linkable_1910_census=census_1910[which((census_1910$hik %in% incarcerated_1920_link_ids)==TRUE),]
all_1920_link_ids=unique(census_1920[which(census_1920$link1910==1), "hik"])
all_1920_linkable_1910_census=census_1910[which((census_1910$hik %in% all_1920_link_ids)==TRUE),]
#excluding 1910 incarcerated
all_1920_linkable_1910_census=all_1920_linkable_1910_census[which((all_1920_linkable_1910_census$hik %in% 
                                                                     incarcerated_1910$hik)==FALSE),]

#building matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1910$ed_id)))
ed_incarceration_rates[,1]=unique(census_1910$ed_id)
gc()
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1920_linkable_1910_census[which(
    incarcerated_1920_linkable_1910_census$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(all_1920_linkable_1910_census[which(
    all_1920_linkable_1910_census$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_ipums=ed_incarceration_rates
save(ed_incarceration_rates_ipums, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_ipums_1920.rda")

#Census tree Crosswalk is third
rm(list=c("all_1920_linkable_1910_census", "census_1910_linkable", "census_1920_identifiable", 
          "incarcerated_1920_linkable_1910_census", "abe_cross"))
gc()

census_1920$id_1910=census_tree[match(census_1920$histid, census_tree[,2]),1] #histid match
incarcerated_1920$id_1910=census_tree[match(incarcerated_1920$histid, census_tree[,2]),1] #histid match
incarcerated_1920=incarcerated_1920[which(is.na(incarcerated_1920$id_1910)==FALSE),] #linkable
#not incarcerated last census
incarcerated_1920=incarcerated_1920[which((incarcerated_1920$id_1910 %in% incarcerated_1910$histid)==FALSE),]

#prepping census tree match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1910$ed_id)))
ed_incarceration_rates[,1]=unique(census_1910$ed_id)
census_1910_linkable=census_1910[which((census_1910$histid %in% census_1920$id_1910)==TRUE),]
census_1910_linkable=census_1910_linkable[which((census_1910_linkable$histid %in% incarcerated_1910$histid)==FALSE),]
incarcerated_1920$ed_id=census_1910_linkable[match(incarcerated_1920$id_1910, census_1910_linkable$histid),"ed_id"]
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

#populating matrix
timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1920[which(incarcerated_1920$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1910_linkable[which(census_1910_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_census_tree=ed_incarceration_rates
save(ed_incarceration_rates_census_tree, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_census_tree_1920.rda")


ed_incarceration_rates_1920=matrix(nrow=nrow(ed_incarceration_rates_abe), ncol=11)
ed_incarceration_rates_1920=as.data.frame(ed_incarceration_rates_1920)
colnames(ed_incarceration_rates_1920)=c("ed_state_county_id", "enum_dist", "num_incarc_abe", "pop_abe",
                                        "rate_abe", "num_incarc_ipums", "pop_ipums", "rate_ipums",
                                        "num_incarc_tree", "pop_tree", "rate_tree")
ed_incarceration_rates_1920[,1]=ed_incarceration_rates_abe[,1]
ed_incarceration_rates_1920[,2]=ed_incarceration_rates_abe[,5]
ed_incarceration_rates_1920[,3]=ed_incarceration_rates_abe[,2]
ed_incarceration_rates_1920[,4]=ed_incarceration_rates_abe[,3]
ed_incarceration_rates_1920[,5]=ed_incarceration_rates_abe[,4]
ed_incarceration_rates_1920[,6]=ed_incarceration_rates_ipums[,2]
ed_incarceration_rates_1920[,7]=ed_incarceration_rates_ipums[,3]
ed_incarceration_rates_1920[,8]=ed_incarceration_rates_ipums[,4]
ed_incarceration_rates_1920[,9]=ed_incarceration_rates_census_tree[,2]
ed_incarceration_rates_1920[,10]=ed_incarceration_rates_census_tree[,3]
ed_incarceration_rates_1920[,11]=ed_incarceration_rates_census_tree[,4]

save(ed_incarceration_rates_1920, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_1920.rda")
rm(list = ls())
gc()

#1930
load("input_data/step_3_input_data/prisoner_records.rda")

#1930 rates calculation
prisoner_records_1930=prisoner_records[which(prisoner_records$year==1930),]
prisoner_records_1920=prisoner_records[which(prisoner_records$year==1920),]
census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
abe_cross=read.csv(file="input_data/step_3_input_data/abe_crosswalks/crosswalk_1920_1930.csv", header = TRUE)
census_tree=read.csv(file="input_data/census_tree_crosswalks/1920_1930.csv", header = TRUE)

gc()
colnames(census_1920)=tolower(colnames(census_1920))
colnames(census_1930)=tolower(colnames(census_1930))

#first abe
#generating ed ID
census_1920$ed_id=paste0(census_1920$stateicp,"_",census_1920$countyicp, "_", census_1920$enumdist)




#incarcerated subsamples
incarcerated_1920=census_1920[which(census_1920$histid %in% prisoner_records_1920$histid),] 
incarcerated_1930=census_1930[which(census_1930$histid %in% prisoner_records_1930$histid),]

gc()

#limiting to standard ABE crosswalk
abe_cross=abe_cross[which(abe_cross[,4]==1),]
census_1930$id_1920=abe_cross[match(census_1930$histid, abe_cross[,2]),1] #histid match
incarcerated_1930$id_1920=abe_cross[match(incarcerated_1930$histid, abe_cross[,2]),1] #histid match

#subsetting for match, those with a match across census
census_1930_identifiable=census_1930[which(is.na(census_1930$id_1920)==FALSE),]
#incarcaerated subset of those with across census match
incarcerated_1930=incarcerated_1930[which(is.na(incarcerated_1930$id_1920)==FALSE),]

#incarecerated subsample that was not incarcerated at last census
incarcerated_1930=incarcerated_1930[which((incarcerated_1930$id_1920 %in% incarcerated_1920$histid)==FALSE),]

#prepping ABE match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1920$ed_id)))
ed_incarceration_rates[,1]=unique(census_1920$ed_id)
census_1920_linkable=census_1920[which((census_1920$histid %in% census_1930$id_1920)==TRUE),]
census_1920_linkable=census_1920_linkable[which((census_1920_linkable$histid %in% incarcerated_1920$histid)==FALSE),]
incarcerated_1930$ed_id=census_1920_linkable[match(incarcerated_1930$id_1920, census_1920_linkable$histid),"ed_id"]

#populating matrix
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1930[which(incarcerated_1930$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1920_linkable[which(census_1920_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_abe=ed_incarceration_rates
save(ed_incarceration_rates_abe, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_abe_1930.rda")


#IPUMS Crosswalk second

incarcerated_1920=census_1920[which(census_1920$histid %in% prisoner_records_1920$histid),] 
incarcerated_1930=census_1930[which(census_1930$histid %in% prisoner_records_1930$histid),]
incarcerated_1930_link_ids=unique(incarcerated_1930[which(incarcerated_1930$link1920==1), "hik"]) #NOW HIK
#removing those incarcerated in 1920
incarcerated_1930_link_ids=incarcerated_1930_link_ids[which((incarcerated_1930_link_ids %in% 
                                                               incarcerated_1920$hik)==FALSE)]
incarcerated_1930_linkable_1920_census=census_1920[which((census_1920$hik %in% incarcerated_1930_link_ids)==TRUE),]
all_1930_link_ids=unique(census_1930[which(census_1930$link1920==1), "hik"])
all_1930_linkable_1920_census=census_1920[which((census_1920$hik %in% all_1930_link_ids)==TRUE),]
#excluding 1920 incarcerated
all_1930_linkable_1920_census=all_1930_linkable_1920_census[which((all_1930_linkable_1920_census$hik %in% 
                                                                     incarcerated_1920$hik)==FALSE),]

#building matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1920$ed_id)))
ed_incarceration_rates[,1]=unique(census_1920$ed_id)
gc()
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1930_linkable_1920_census[which(
    incarcerated_1930_linkable_1920_census$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(all_1930_linkable_1920_census[which(
    all_1930_linkable_1920_census$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_ipums=ed_incarceration_rates
save(ed_incarceration_rates_ipums, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_ipums_1930.rda")

#Census tree Crosswalk is third
rm(list=c("all_1930_linkable_1920_census", "census_1920_linkable", "census_1930_identifiable", 
          "incarcerated_1930_linkable_1920_census", "abe_cross"))
gc()

census_1930$id_1920=census_tree[match(census_1930$histid, census_tree[,2]),1] #histid match
incarcerated_1930$id_1920=census_tree[match(incarcerated_1930$histid, census_tree[,2]),1] #histid match
incarcerated_1930=incarcerated_1930[which(is.na(incarcerated_1930$id_1920)==FALSE),] #linkable
#not incarcerated last census
incarcerated_1930=incarcerated_1930[which((incarcerated_1930$id_1920 %in% incarcerated_1920$histid)==FALSE),]

#prepping census tree match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1920$ed_id)))
ed_incarceration_rates[,1]=unique(census_1920$ed_id)
census_1920_linkable=census_1920[which((census_1920$histid %in% census_1930$id_1920)==TRUE),]
census_1920_linkable=census_1920_linkable[which((census_1920_linkable$histid %in% incarcerated_1920$histid)==FALSE),]
incarcerated_1930$ed_id=census_1920_linkable[match(incarcerated_1930$id_1920, census_1920_linkable$histid),"ed_id"]
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

#populating matrix
timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1930[which(incarcerated_1930$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1920_linkable[which(census_1920_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_census_tree=ed_incarceration_rates
save(ed_incarceration_rates_census_tree, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_census_tree_1930.rda")


ed_incarceration_rates_1930=matrix(nrow=nrow(ed_incarceration_rates_abe), ncol=11)
ed_incarceration_rates_1930=as.data.frame(ed_incarceration_rates_1930)
colnames(ed_incarceration_rates_1930)=c("ed_state_county_id", "enum_dist", "num_incarc_abe", "pop_abe",
                                        "rate_abe", "num_incarc_ipums", "pop_ipums", "rate_ipums",
                                        "num_incarc_tree", "pop_tree", "rate_tree")
ed_incarceration_rates_1930[,1]=ed_incarceration_rates_abe[,1]
ed_incarceration_rates_1930[,2]=ed_incarceration_rates_abe[,5]
ed_incarceration_rates_1930[,3]=ed_incarceration_rates_abe[,2]
ed_incarceration_rates_1930[,4]=ed_incarceration_rates_abe[,3]
ed_incarceration_rates_1930[,5]=ed_incarceration_rates_abe[,4]
ed_incarceration_rates_1930[,6]=ed_incarceration_rates_ipums[,2]
ed_incarceration_rates_1930[,7]=ed_incarceration_rates_ipums[,3]
ed_incarceration_rates_1930[,8]=ed_incarceration_rates_ipums[,4]
ed_incarceration_rates_1930[,9]=ed_incarceration_rates_census_tree[,2]
ed_incarceration_rates_1930[,10]=ed_incarceration_rates_census_tree[,3]
ed_incarceration_rates_1930[,11]=ed_incarceration_rates_census_tree[,4]

save(ed_incarceration_rates_1930, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_1930.rda")
rm(list = ls())
gc()

#1940
load("input_data/step_3_input_data/prisoner_records.rda")

#1940 rates calculation
prisoner_records_1940=prisoner_records[which(prisoner_records$year==1940),]
prisoner_records_1930=prisoner_records[which(prisoner_records$year==1930),]
census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))
abe_cross=read.csv(file="input_data/step_3_input_data/abe_crosswalks/crosswalk_1930_1940.csv", header = TRUE)
census_tree=read.csv(file="input_data/census_tree_crosswalks/1930_1940.csv", header = TRUE)

gc()
colnames(census_1930)=tolower(colnames(census_1930))
colnames(census_1940)=tolower(colnames(census_1940))

#first abe
#generating ed ID
census_1930$ed_id=paste0(census_1930$stateicp,"_",census_1930$countyicp, "_", census_1930$enumdist)




#incarcerated subsamples
incarcerated_1930=census_1930[which(census_1930$histid %in% prisoner_records_1930$histid),] 
incarcerated_1940=census_1940[which(census_1940$histid %in% prisoner_records_1940$histid),]

gc()

#limiting to standard ABE crosswalk
abe_cross=abe_cross[which(abe_cross[,4]==1),]
census_1940$id_1930=abe_cross[match(census_1940$histid, abe_cross[,2]),1] #histid match
incarcerated_1940$id_1930=abe_cross[match(incarcerated_1940$histid, abe_cross[,2]),1] #histid match

#subsetting for match, those with a match across census
census_1940_identifiable=census_1940[which(is.na(census_1940$id_1930)==FALSE),]
#incarcaerated subset of those with across census match
incarcerated_1940=incarcerated_1940[which(is.na(incarcerated_1940$id_1930)==FALSE),]

#incarecerated subsample that was not incarcerated at last census
incarcerated_1940=incarcerated_1940[which((incarcerated_1940$id_1930 %in% incarcerated_1930$histid)==FALSE),]

#prepping ABE match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1930$ed_id)))
ed_incarceration_rates[,1]=unique(census_1930$ed_id)
census_1930_linkable=census_1930[which((census_1930$histid %in% census_1940$id_1930)==TRUE),]
census_1930_linkable=census_1930_linkable[which((census_1930_linkable$histid %in% incarcerated_1930$histid)==FALSE),]
incarcerated_1940$ed_id=census_1930_linkable[match(incarcerated_1940$id_1930, census_1930_linkable$histid),"ed_id"]

#populating matrix
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1940[which(incarcerated_1940$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1930_linkable[which(census_1930_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_abe=ed_incarceration_rates
save(ed_incarceration_rates_abe, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_abe_1940.rda")


#IPUMS Crosswalk second

incarcerated_1930=census_1930[which(census_1930$histid %in% prisoner_records_1930$histid),] 
incarcerated_1940=census_1940[which(census_1940$histid %in% prisoner_records_1940$histid),]
incarcerated_1940_link_ids=unique(incarcerated_1940[which(incarcerated_1940$link1930==1), "hik"]) #NOW HIK
#removing those incarcerated in 1930
incarcerated_1940_link_ids=incarcerated_1940_link_ids[which((incarcerated_1940_link_ids %in% 
                                                               incarcerated_1930$hik)==FALSE)]
incarcerated_1940_linkable_1930_census=census_1930[which((census_1930$hik %in% incarcerated_1940_link_ids)==TRUE),]
all_1940_link_ids=unique(census_1940[which(census_1940$link1930==1), "hik"])
all_1940_linkable_1930_census=census_1930[which((census_1930$hik %in% all_1940_link_ids)==TRUE),]
#excluding 1930 incarcerated
all_1940_linkable_1930_census=all_1940_linkable_1930_census[which((all_1940_linkable_1930_census$hik %in% 
                                                                     incarcerated_1930$hik)==FALSE),]

#building matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1930$ed_id)))
ed_incarceration_rates[,1]=unique(census_1930$ed_id)
gc()
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1940_linkable_1930_census[which(
    incarcerated_1940_linkable_1930_census$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(all_1940_linkable_1930_census[which(
    all_1940_linkable_1930_census$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_ipums=ed_incarceration_rates
save(ed_incarceration_rates_ipums, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_ipums_1940.rda")

#Census tree Crosswalk is third
rm(list=c("all_1940_linkable_1930_census", "census_1930_linkable", "census_1940_identifiable", 
          "incarcerated_1940_linkable_1930_census", "abe_cross"))
gc()

census_1940$id_1930=census_tree[match(census_1940$histid, census_tree[,2]),1] #histid match
incarcerated_1940$id_1930=census_tree[match(incarcerated_1940$histid, census_tree[,2]),1] #histid match
incarcerated_1940=incarcerated_1940[which(is.na(incarcerated_1940$id_1930)==FALSE),] #linkable
#not incarcerated last census
incarcerated_1940=incarcerated_1940[which((incarcerated_1940$id_1930 %in% incarcerated_1930$histid)==FALSE),]

#prepping census tree match incarceration rate matrix
ed_incarceration_rates=matrix(ncol=4, nrow=length(unique(census_1930$ed_id)))
ed_incarceration_rates[,1]=unique(census_1930$ed_id)
census_1930_linkable=census_1930[which((census_1930$histid %in% census_1940$id_1930)==TRUE),]
census_1930_linkable=census_1930_linkable[which((census_1930_linkable$histid %in% incarcerated_1930$histid)==FALSE),]
incarcerated_1940$ed_id=census_1930_linkable[match(incarcerated_1940$id_1930, census_1930_linkable$histid),"ed_id"]
ed_incarceration_rates=as.data.frame(ed_incarceration_rates)

#populating matrix
timeNow <- Sys.time()
for(i in 1:nrow(ed_incarceration_rates)){
  ed_incarceration_rates[i,2]=nrow(incarcerated_1940[which(incarcerated_1940$ed_id==ed_incarceration_rates[i,1]),])
  ed_incarceration_rates[i,3]=nrow(census_1930_linkable[which(census_1930_linkable$ed_id==ed_incarceration_rates[i,1]),])
  cat("\r", round(i*100/nrow(ed_incarceration_rates), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

ed_incarceration_rates=as.data.frame(ed_incarceration_rates)
ed_incarceration_rates[,4]=ifelse(ed_incarceration_rates[,3]==0, 0, as.numeric(ed_incarceration_rates[,2])/as.numeric(ed_incarceration_rates[,3]))   
ed_incarceration_rates$enum_dist=sub(".*_", "", ed_incarceration_rates[,1])
colnames(ed_incarceration_rates)[1]="ed_state_county_id"
colnames(ed_incarceration_rates)[2]="num_incarc"
colnames(ed_incarceration_rates)[3]="pop"
colnames(ed_incarceration_rates)[4]="rate"

ed_incarceration_rates_census_tree=ed_incarceration_rates
save(ed_incarceration_rates_census_tree, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_census_tree_1940.rda")


ed_incarceration_rates_1940=matrix(nrow=nrow(ed_incarceration_rates_abe), ncol=11)
ed_incarceration_rates_1940=as.data.frame(ed_incarceration_rates_1940)
colnames(ed_incarceration_rates_1940)=c("ed_state_county_id", "enum_dist", "num_incarc_abe", "pop_abe",
                                        "rate_abe", "num_incarc_ipums", "pop_ipums", "rate_ipums",
                                        "num_incarc_tree", "pop_tree", "rate_tree")
ed_incarceration_rates_1940[,1]=ed_incarceration_rates_abe[,1]
ed_incarceration_rates_1940[,2]=ed_incarceration_rates_abe[,5]
ed_incarceration_rates_1940[,3]=ed_incarceration_rates_abe[,2]
ed_incarceration_rates_1940[,4]=ed_incarceration_rates_abe[,3]
ed_incarceration_rates_1940[,5]=ed_incarceration_rates_abe[,4]
ed_incarceration_rates_1940[,6]=ed_incarceration_rates_ipums[,2]
ed_incarceration_rates_1940[,7]=ed_incarceration_rates_ipums[,3]
ed_incarceration_rates_1940[,8]=ed_incarceration_rates_ipums[,4]
ed_incarceration_rates_1940[,9]=ed_incarceration_rates_census_tree[,2]
ed_incarceration_rates_1940[,10]=ed_incarceration_rates_census_tree[,3]
ed_incarceration_rates_1940[,11]=ed_incarceration_rates_census_tree[,4]

save(ed_incarceration_rates_1940, file="intermediate_outputs/incarceration_rates/ed_incarceration_rates_1940.rda")
rm(list = ls())
gc()
gc()