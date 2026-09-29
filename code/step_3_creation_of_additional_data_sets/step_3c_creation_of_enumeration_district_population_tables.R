#################################################################################################################################
#################### STEP 3C: CREATION OF SUPPLEMENTARY FILES FOR BASIC ENUMERATION DISTRICT POPULAITON         #################
####################          AND RELATED METRICS THIS TAKES FULL COUNT CENSUSES AS INPUTS AND COMPUTES         #################
####################          SHARES AND SIMPLE POPULATION TABLE                                                #################
#################################################################################################################################
library(haven)
library(stringr)


      ###### LOADING PRELIMINARY POPULATION DATA FROM INCARCERATION RATE FILES ALREADY CONSTRUCTED
load("intermediate_outputs/incarceration_rates/ed_incarceration_rates_1940.rda")
inc_rate_1940=ed_incarceration_rates_1940#provides 1930 pop
load("intermediate_outputs/incarceration_rates/ed_incarceration_rates_1920.rda")
inc_rate_1920=ed_incarceration_rates_1920#provides 1910 pop
load("intermediate_outputs/incarceration_rates/ed_incarceration_rates_1930.rda")
inc_rate_1930=ed_incarceration_rates_1930 #provides 1920 pop
load("intermediate_outputs/incarceration_rates/ed_incarceration_rates_1910.rda")
inc_rate_1910=ed_incarceration_rates_1910 #provides 1900 pop

pop_1930=matrix(nrow=nrow(inc_rate_1940), ncol=5)
pop_1930[,1]=inc_rate_1940$ed_state_county_id
pop_1920=matrix(nrow=nrow(inc_rate_1930), ncol=5)
pop_1920[,1]=inc_rate_1930$ed_state_county_id
pop_1910=matrix(nrow=nrow(inc_rate_1920), ncol=5)
pop_1910[,1]=inc_rate_1920$ed_state_county_id
pop_1900=matrix(nrow=nrow(inc_rate_1910), ncol=5)
pop_1900[,1]=inc_rate_1910$ed_state_county_id

rm(inc_rate_1920)
rm(inc_rate_1930)
rm(inc_rate_1940)
rm(inc_rate_1910)
rm(ed_incarceration_rates_1910)
rm(ed_incarceration_rates_1920)
rm(ed_incarceration_rates_1930)
rm(ed_incarceration_rates_1940)
gc()
  
    #####  PROCEEDING YEAR BY YEAR CONSTRUCTING FULL POPULATION OBJECTS
#1900
italians_1900=read.csv("input_data/census_restricted/census_1900.csv")
census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
census_1900=as.data.frame(census_1900)
census_1900$enum_dist_id=paste0(as.character(census_1900[,6]), "_", as.character(census_1900[,8]),"_",as.character(census_1900[,17]))
italians_1900$enum_dist_id=census_1900[match(italians_1900$histid, census_1900$HISTID), 50]

timeNow <- Sys.time()
for (i in 1:nrow(pop_1900)){
  pop_1900[i,2]=nrow(census_1900[which(census_1900$enum_dist_id==pop_1900[i,1]),])
  pop_1900[i,3]=nrow(italians_1900[which(italians_1900$enum_dist_id==pop_1900[i,1]),])
  cat("\r", round(i*100/nrow(pop_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
pop_1900=as.data.frame(pop_1900)
pop_1900[,2]=as.numeric(pop_1900[,2])
pop_1900[,3]=as.numeric(pop_1900[,3])
pop_1900[,4]=ifelse(pop_1900[,2]==0, 0, pop_1900[,3]/pop_1900[,2])
colnames(pop_1900)=c("enum_dist_id", "population", "italian_population", "italian_proportion", "median_home_value")
pop_1900[,5]=NA
save(pop_1900, file="intermediate_outputs/population_1900.rda")

rm(census_1900)
rm(italians_1900)
gc()

#1910
italians_1910=read.csv("input_data/census_restricted/census_1910.csv")
census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
census_1910=as.data.frame(census_1910)
census_1910$enum_dist_id=paste0(as.character(census_1910[,6]), "_", as.character(census_1910[,8]),"_",as.character(census_1910[,17]))
italians_1910$enum_dist_id=census_1910[match(italians_1910$histid, census_1910$HISTID), 49]

timeNow <- Sys.time()
for (i in 1:nrow(pop_1910)){
  pop_1910[i,2]=nrow(census_1910[which(census_1910$enum_dist_id==pop_1910[i,1]),])
  pop_1910[i,3]=nrow(italians_1910[which(italians_1910$enum_dist_id==pop_1910[i,1]),])
  cat("\r", round(i*100/nrow(pop_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
pop_1910=as.data.frame(pop_1910)
pop_1910[,2]=as.numeric(pop_1910[,2])
pop_1910[,3]=as.numeric(pop_1910[,3])
pop_1910[,4]=ifelse(pop_1910[,2]==0, 0, pop_1910[,3]/pop_1910[,2])
colnames(pop_1910)=c("enum_dist_id", "population", "italian_population", "italian_proportion", "median_home_value")
pop_1910[,5]=NA
save(pop_1910, file="intermediate_outputs/population_1910.rda")

rm(census_1910)
rm(italians_1910)
gc()

#1920
italians_1920=read.csv("input_data/census_restricted/census_1920.csv")
census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
census_1920=as.data.frame(census_1920)
census_1920$enum_dist_id=paste0(as.character(census_1920[,6]), "_", as.character(census_1920[,8]),"_",as.character(census_1920[,21]))
italians_1920$enum_dist_id=census_1920[match(italians_1920$histid, census_1920$HISTID), 53]

timeNow <- Sys.time()
for (i in 1:nrow(pop_1920)){
  pop_1920[i,2]=nrow(census_1920[which(census_1920$enum_dist_id==pop_1920[i,1]),])
  pop_1920[i,3]=nrow(italians_1920[which(italians_1920$enum_dist_id==pop_1920[i,1]),])
  cat("\r", round(i*100/nrow(pop_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
pop_1920=as.data.frame(pop_1920)
pop_1920[,2]=as.numeric(pop_1920[,2])
pop_1920[,3]=as.numeric(pop_1920[,3])
pop_1920[,4]=ifelse(pop_1920[,2]==0, 0, pop_1920[,3]/pop_1920[,2])
colnames(pop_1920)=c("enum_dist_id", "population", "italian_population", "italian_proportion", "median_home_value")
pop_1920[,5]=NA
save(pop_1920, file="intermediate_outputs/population_1920.rda")


rm(census_1920)
rm(italians_1920)
gc()


#1930
italians_1930=read.csv("input_data/census_restricted/census_1930.csv")
census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
census_1930=as.data.frame(census_1930)
census_1930$enum_dist_id=paste0(as.character(census_1930[,6]), "_", as.character(census_1930[,8]),"_",as.character(census_1930[,20]))
italians_1930$enum_dist_id=census_1930[match(italians_1930$histid, census_1930$HISTID), 53]


timeNow <- Sys.time()
for (i in 1:nrow(pop_1930)){
  pop_1930[i,2]=nrow(census_1930[which(census_1930$enum_dist_id==pop_1930[i,1]),])
  pop_1930[i,3]=nrow(italians_1930[which(italians_1930$enum_dist_id==pop_1930[i,1]),])
  pop_1930[i,5]=median(census_1930[which(census_1930$enum_dist_id==pop_1930[i,1]), "VALUEH"], na.rm = TRUE)
  cat("\r", round(i*100/nrow(pop_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
pop_1930=as.data.frame(pop_1930)
pop_1930[,2]=as.numeric(pop_1930[,2])
pop_1930[,3]=as.numeric(pop_1930[,3])
pop_1930[,4]=ifelse(pop_1930[,2]==0, 0, pop_1930[,3]/pop_1930[,2])
colnames(pop_1930)=c("enum_dist_id", "population", "italian_population", "italian_proportion", "median_home_value")
save(pop_1930, file="intermediate_outputs/population_1930.rda")


rm(census_1930)
rm(italians_1930)
gc()

#1940
italians_1940=read.csv("input_data/census_restricted/census_1940.csv")
census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))
census_1940=as.data.frame(census_1940)
census_1940$enum_dist_id=paste0(as.character(census_1940[,6]), "_", as.character(census_1940[,8]),"_",as.character(census_1940[,21]))
italians_1940$enum_dist_id=census_1940[match(italians_1940$histid, census_1940$HISTID), 52]
pop_1940=matrix(nrow=length(unique(census_1940$enum_dist_id)), ncol=5)
pop_1940[,1]=unique(census_1940$enum_dist_id)
timeNow <- Sys.time()
for (i in 1:nrow(pop_1940)){
  pop_1940[i,2]=nrow(census_1940[which(census_1940$enum_dist_id==pop_1940[i,1]),])
  pop_1940[i,3]=nrow(italians_1940[which(italians_1940$enum_dist_id==pop_1940[i,1]),])
  pop_1940[i,5]=median(census_1940[which(census_1940$enum_dist_id==pop_1940[i,1]), "VALUEH"], na.rm = TRUE)
  cat("\r", round(i*100/nrow(pop_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
pop_1940=as.data.frame(pop_1940)
pop_1940[,2]=as.numeric(pop_1940[,2])
pop_1940[,3]=as.numeric(pop_1940[,3])
pop_1940[,4]=ifelse(pop_1940[,2]==0, 0, pop_1940[,3]/pop_1940[,2])
colnames(pop_1940)=c("enum_dist_id", "population", "italian_population", "italian_proportion", "median_home_value")
save(pop_1940, file="intermediate_outputs/population_1940.rda")

rm(census_1940)
rm(italians_1940)
gc()


#### I AM AT THIS POINT
    ##### LIMITING TO CITIES IN OUR GEOGRAPHY
#baltimore is 52_50
#boston is 3_250
#brooklyn is 13_470
#chicago is 21_310
#cincinatti is 24_610
#cleevalnd is 24_350
#detroit is 23_1630
#manhattan is 13_610
#philadelphia is 14_1010
#pittsburgh is 14_30
#st louis is 34_5100

set=c("52_", "13_", "21_", "24_", "23_", "14_", "34_")


load("intermediate_outputs/population_1910.rda")

pop_1910_relevant=pop_1910[which(substr(pop_1910$enum_dist_id, 1, 3) %in% set| substr(pop_1910$enum_dist_id,1, 2)=="3_"),]
pop_1910_relevant$state_icp=substr(pop_1910_relevant$enum_dist_id, 1, 2)
pop_1910_relevant$state_icp=gsub("_","",pop_1910_relevant$state_icp)
pop_1910_relevant$county_icp=str_extract(pop_1910_relevant$enum_dist_id, "_(.*?)_")
pop_1910_relevant$county_icp=gsub("_","", pop_1910_relevant$county_icp)
pop_1910_relevant$state_county=paste0(pop_1910_relevant$state_icp,"_",pop_1910_relevant$county_icp)
set=c("52_5100","3_250", "13_470", "21_310", "24_610", "24_350", "23_1630", "13_610", "14_1010", "14_30", "34_5100")
set_mat=matrix(nrow=length(set), ncol=2)
set_mat[,1]=set
set_mat[,2]=c("Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinatti", "Cleveland", "Detroit", "Manhattan", "Philadelphia", "Pittsburgh", "St. Louis")
pop_1910_relevant$city=set_mat[match(pop_1910_relevant$state_county, set_mat[,1]),2]
pop_1910_relevant=pop_1910_relevant[which(pop_1910_relevant$state_county %in% set),]
pop_1910_relevant$ED=substr(pop_1910_relevant$enum_dist_id, nchar(pop_1910_relevant$enum_dist_id)-4, nchar(pop_1910_relevant))
pop_1910_relevant$ED=as.numeric(pop_1910_relevant$ED)
pop_1910_relevant$ED=as.character(pop_1910_relevant$ED)
pop_1910_relevant$enum_dist_id=gsub("^([^_]*_[^_]*_).*$", "\\1", pop_1910_relevant$enum_dist_id)
pop_1910_relevant$enum_dist_id=paste0(pop_1910_relevant$enum_dist_id, pop_1910_relevant$ED)
save(pop_1910_relevant, file="intermediate_outputs/population_1910_relevant.rda")

load("intermediate_outputs/population_1920.rda")
set=c("52_", "13_", "21_", "24_", "23_", "14_", "34_")

pop_1920_relevant=pop_1920[which(substr(pop_1920$enum_dist_id, 1, 3) %in% set| substr(pop_1920$enum_dist_id,1, 2)=="3_"),]
pop_1920_relevant$state_icp=substr(pop_1920_relevant$enum_dist_id, 1, 2)
pop_1920_relevant$state_icp=gsub("_","",pop_1920_relevant$state_icp)
library(stringr)
pop_1920_relevant$county_icp=str_extract(pop_1920_relevant$enum_dist_id, "_(.*?)_")
pop_1920_relevant$county_icp=gsub("_","", pop_1920_relevant$county_icp)
pop_1920_relevant$state_county=paste0(pop_1920_relevant$state_icp,"_",pop_1920_relevant$county_icp)
set=c("52_5100","3_250", "13_470", "21_310", "24_610", "24_350", "23_1630", "13_610", "14_1010", "14_30", "34_5100")
set_mat=matrix(nrow=length(set), ncol=2)
set_mat[,1]=set
set_mat[,2]=c("Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinatti", "Cleveland", "Detroit", "Manhattan", "Philadelphia", "Pittsburgh", "St. Louis")
pop_1920_relevant$city=set_mat[match(pop_1920_relevant$state_county, set_mat[,1]),2]
pop_1920_relevant=pop_1920_relevant[which(pop_1920_relevant$state_county %in% set),]
pop_1920_relevant$ED=substr(pop_1920_relevant$enum_dist_id, nchar(pop_1920_relevant$enum_dist_id)-4, nchar(pop_1920_relevant))
pop_1920_relevant$ED=as.numeric(pop_1920_relevant$ED)
pop_1920_relevant$ED=as.character(pop_1920_relevant$ED)
pop_1920_relevant$enum_dist_id=gsub("^([^_]*_[^_]*_).*$", "\\1", pop_1920_relevant$enum_dist_id)
pop_1920_relevant$enum_dist_id=paste0(pop_1920_relevant$enum_dist_id, pop_1920_relevant$ED)
save(pop_1920_relevant, file="intermediate_outputs/population_1920_relevant.rda")

load("intermediate_outputs/population_1930.rda")
set=c("52_", "13_", "21_", "24_", "23_", "14_", "34_")

pop_1930_relevant=pop_1930[which(substr(pop_1930$enum_dist_id, 1, 3) %in% set| substr(pop_1930$enum_dist_id,1, 2)=="3_"),]
pop_1930_relevant$state_icp=substr(pop_1930_relevant$enum_dist_id, 1, 2)
pop_1930_relevant$state_icp=gsub("_","",pop_1930_relevant$state_icp)
library(stringr)
pop_1930_relevant$county_icp=str_extract(pop_1930_relevant$enum_dist_id, "_(.*?)_")
pop_1930_relevant$county_icp=gsub("_","", pop_1930_relevant$county_icp)
pop_1930_relevant$state_county=paste0(pop_1930_relevant$state_icp,"_",pop_1930_relevant$county_icp)
set=c("52_5100","3_250", "13_470", "21_310", "24_610", "24_350", "23_1630", "13_610", "14_1010", "14_30", "34_5100")
set_mat=matrix(nrow=length(set), ncol=2)
set_mat[,1]=set
set_mat[,2]=c("Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinatti", "Cleveland", "Detroit", "Manhattan", "Philadelphia", "Pittsburgh", "St. Louis")
pop_1930_relevant$city=set_mat[match(pop_1930_relevant$state_county, set_mat[,1]),2]
pop_1930_relevant=pop_1930_relevant[which(pop_1930_relevant$state_county %in% set),]
pop_1930_relevant$ED=substr(pop_1930_relevant$enum_dist_id, nchar(pop_1930_relevant$enum_dist_id)-4, nchar(pop_1930_relevant))
pop_1930_relevant$ED=as.numeric(pop_1930_relevant$ED)
pop_1930_relevant$ED=as.character(pop_1930_relevant$ED)
pop_1930_relevant$enum_dist_id=gsub("^([^_]*_[^_]*_).*$", "\\1", pop_1930_relevant$enum_dist_id)
pop_1930_relevant$enum_dist_id=paste0(pop_1930_relevant$enum_dist_id, pop_1930_relevant$ED)
save(pop_1930_relevant, file="intermediate_outputs/population_1930_relevant.rda")

load("intermediate_outputs/population_1940.rda")
set=c("52_", "13_", "21_", "24_", "23_", "14_", "34_")

pop_1940_relevant=pop_1940[which(substr(pop_1940$enum_dist_id, 1, 3) %in% set| substr(pop_1940$enum_dist_id,1, 2)=="3_"),]
pop_1940_relevant$state_icp=substr(pop_1940_relevant$enum_dist_id, 1, 2)
pop_1940_relevant$state_icp=gsub("_","",pop_1940_relevant$state_icp)
library(stringr)
pop_1940_relevant$county_icp=str_extract(pop_1940_relevant$enum_dist_id, "_(.*?)_")
pop_1940_relevant$county_icp=gsub("_","", pop_1940_relevant$county_icp)
pop_1940_relevant$state_county=paste0(pop_1940_relevant$state_icp,"_",pop_1940_relevant$county_icp)
set=c("52_5100","3_250", "13_470", "21_310", "24_610", "24_350", "23_1630", "13_610", "14_1010", "14_30", "34_5100")
set_mat=matrix(nrow=length(set), ncol=2)
set_mat[,1]=set
set_mat[,2]=c("Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinatti", "Cleveland", "Detroit", "Manhattan", "Philadelphia", "Pittsburgh", "St. Louis")
pop_1940_relevant$city=set_mat[match(pop_1940_relevant$state_county, set_mat[,1]),2]
pop_1940_relevant=pop_1940_relevant[which(pop_1940_relevant$state_county %in% set),]
pop_1940_relevant$ED=substr(pop_1940_relevant$enum_dist_id, nchar(pop_1940_relevant$enum_dist_id)-4, nchar(pop_1940_relevant))
pop_1940_relevant$ED=as.numeric(pop_1940_relevant$ED)
pop_1940_relevant$ED=as.character(pop_1940_relevant$ED)
pop_1940_relevant$enum_dist_id=gsub("^([^_]*_[^_]*_).*$", "\\1", pop_1940_relevant$enum_dist_id)
pop_1940_relevant$enum_dist_id=paste0(pop_1940_relevant$enum_dist_id, pop_1940_relevant$ED)
save(pop_1940_relevant, file="intermediate_outputs/population_1940_relevant.rda")

load("intermediate_outputs/population_1900.rda")
set=c("52_", "13_", "21_", "24_", "23_", "14_", "34_")

pop_1900_relevant=pop_1900[which(substr(pop_1900$enum_dist_id, 1, 3) %in% set| substr(pop_1900$enum_dist_id,1, 2)=="3_"),]
pop_1900_relevant$state_icp=substr(pop_1900_relevant$enum_dist_id, 1, 2)
pop_1900_relevant$state_icp=gsub("_","",pop_1900_relevant$state_icp)
library(stringr)
pop_1900_relevant$county_icp=str_extract(pop_1900_relevant$enum_dist_id, "_(.*?)_")
pop_1900_relevant$county_icp=gsub("_","", pop_1900_relevant$county_icp)
pop_1900_relevant$state_county=paste0(pop_1900_relevant$state_icp,"_",pop_1900_relevant$county_icp)
set=c("52_5100","3_250", "13_470", "21_310", "24_610", "24_350", "23_1630", "13_610", "14_1010", "14_30", "34_5100")
set_mat=matrix(nrow=length(set), ncol=2)
set_mat[,1]=set
set_mat[,2]=c("Baltimore", "Boston", "Brooklyn", "Chicago", "Cincinatti", "Cleveland", "Detroit", "Manhattan", "Philadelphia", "Pittsburgh", "St. Louis")
pop_1900_relevant$city=set_mat[match(pop_1900_relevant$state_county, set_mat[,1]),2]
pop_1900_relevant=pop_1900_relevant[which(pop_1900_relevant$state_county %in% set),]
pop_1900_relevant$ED=substr(pop_1900_relevant$enum_dist_id, nchar(pop_1900_relevant$enum_dist_id)-4, nchar(pop_1900_relevant))
pop_1900_relevant$ED=as.numeric(pop_1900_relevant$ED)
pop_1900_relevant$ED=as.character(pop_1900_relevant$ED)
pop_1900_relevant$enum_dist_id=gsub("^([^_]*_[^_]*_).*$", "\\1", pop_1900_relevant$enum_dist_id)
pop_1900_relevant$enum_dist_id=paste0(pop_1900_relevant$enum_dist_id, pop_1900_relevant$ED)
save(pop_1900_relevant, file="intermediate_outputs/population_1900_relevant.rda")
