###############################################################################################################################
############## Step 1a: identifying "matched" sicilian commune and determinig which commune need manual RA matching ############
###############################################################################################################################

library(haven)
italy <- read_dta("input_data/step_1_misc_data/italy.dta") #Italian numident sample provided by Joe Price
italy=as.data.frame(italy)
data=italy[which(italy$pr_bir_place_orig != "italy"),]
data$origin=sub(",.*", "", data$pr_bir_place_orig)
data=data[which(data$origin != "italy"),]
data=data[which(data$origin != "italia"),]
data=data[which(data$origin != "itali"),]
data=data[which(data$origin != "sicily"),] 

library(readxl)
Sicily1853_1951_1981 <- read_excel("input_data/step_1_misc_data/Sicily1853-1951-1981.xlsx") #from Acemoglu et al. 2020 Replication for data
sicily=as.data.frame(Sicily1853_1951_1981)

library(stringdist)
sicily_muni=append(sicily$comune1951, sicily$comune1853)
distance_matrix=stringdistmatrix(data$origin, sicily_muni, method = "lv") 

direct_match=vector(mode="character", length=nrow(data))
timeNow <- Sys.time()
for(i in 1:nrow(data)){
  if (grepl("\\*", data$origin[i])==TRUE & grepl(" ", data$origin[i])==TRUE){
    tmp=grepl(gsub(" .*", "", data$origin[i]), sicily_muni)
    if (length(tmp[which(tmp==TRUE)]>0)){
      direct_match[i]=sicily_muni[which(tmp==TRUE)][1]
    }
    else{
      direct_match[i]=NA
    }
  } else{
    tmp=grepl(gsub("\\*", "", data$origin[i]), sicily_muni)
    if (length(tmp[which(tmp==TRUE)]>0)){
      direct_match[i]=sicily_muni[which(tmp==TRUE)][1]
    }
    else{
      direct_match[i]=NA
    }
  }
  cat("\r", round(i*100/(nrow(data)), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

length(direct_match[which(direct_match != "NA")])

data$origin2=ifelse((grepl("\\*", data$origin)==TRUE & grepl(" ", data$origin)==TRUE),  gsub(" .*", "", data$origin), data$origin)
distance_matrix=stringdistmatrix(data$origin2, sicily_muni, method = "lv") 
indirect_match=vector(mode="character", length=nrow(data))
distance=vector(mode="character", length=nrow(data))
timeNow <- Sys.time()
for(i in 1:nrow(distance_matrix)){
  indirect_match[i]=sicily_muni[which.min(distance_matrix[i,])]
  distance[i]=distance_matrix[i,which.min(distance_matrix[i,])]
  cat("\r", round(i*100/(nrow(data)), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

data$origin2=NULL
matches=cbind.data.frame(data$pr_bir_place_orig, data$origin, direct_match, indirect_match, as.numeric(distance))
colnames(matches)=c("numident_origin", "shortened_origin", "direct_match", "closest_indirect_match", "indirect_distance")
save(matches, file="intermediate_outputs/matches.rda")
test=matches[which(matches$indirect_distance==1),] #close matches only
test=test[which(substr(test$numident_origin,1,1)==substr(test$closest_indirect_match,1,1)),] #first letter must match
test=test[which(is.na(test$direct_match)==TRUE),] #only matches without direct match
test=unique(test)
manual_match_set=test
save(manual_match_set, file="manual_match_set.rda") #This file is operated on by RAs manually to identify unmatched commune as Sicilian or Not
 
################################################################################################
######## Step 2 assigning numident records a matched Siiclian commune if applicable ############
################################################################################################

library(haven)
italy <- as.data.frame(read_dta("input_data/step_1_misc_data/italy.dta")) #this is italian numident provided by Joe Price 
communes=as.data.frame(read.csv("input_data/step_1_misc_data/manual_match_set-2.csv")) #built manually by RAs for comune name crosswalk from unmatched object in previous step
communes=communes[which((communes$Correct %in% c("Yes", "yes", "Yes "))|(communes$Valid==1)),]
load("intermediate_outputs/matches.rda")
matches$verified_fuzzy=communes[match(matches$numident_origin, communes$numident_origin),"closest_indirect_match"]
fuzzy=matches[which(is.na(matches$verified_fuzzy)==FALSE),]
fuzzy=unique(fuzzy)
direct=matches[which(is.na(matches$direct_match)==FALSE),]
direct=unique(direct)
direct=direct[which(direct$numident_origin != "rome, italy" & direct$numident_origin != "rom, italy"),]
direct$match=direct$direct_match
direct$direct_match=1
direct$fuzzy_match=0
fuzzy$direct_match=0
fuzzy$fuzzy_match=1
fuzzy$match=fuzzy$verified_fuzzy
fuzzy=fuzzy[,c(1,2,8,3,7)]
direct=direct[,c(1,2,7,3,8)]
matches_communes=rbind.data.frame(direct, fuzzy)
save(matches_communes, file="intermediate_outputs/matches_communes.rda")

italy=italy[which(italy$pr_bir_place_orig %in% matches_communes$numident_origin),]
test=italy[which(italy$pr_bir_place_orig %in% matches_communes$numident_origin),]
test=test[,c(1,2,3,4,5,6,7,8,9,10,11)]
colnames(test)=c("numident_origin", "ark", "first_name", "surname","father_name","father_surname", 
                 "mother_name", "mother_surname", "sex", "birth_date","death_date")

test$matched_commune=matches_communes[match(test$numident_origin, matches_communes$numident_origin), "match"]

numident=test
save(numident, file="intermediate_outputs/sicilian_numident.rda")

##########################################################################
########  Step 3 adding mori and non-mori mafia indicators  ##############
##########################################################################

ConLaMafiaAiFerriCorti_Data <- read.csv("input_data/step_1_misc_data/ConLaMafiaAiFerriCorti_Data.csv") #RA generated timeline of Mori campaign
mori=as.data.frame(ConLaMafiaAiFerriCorti_Data)
ADD_Mafia_municipality <- read_dta("input_data/step_1_misc_data/ADD_Mafia_municipality.dta") #from Acemoglu et al 2020 replication data (Cutrera and Damiani maps)
maps=as.data.frame(ADD_Mafia_municipality)
rm(ADD_Mafia_municipality)
rm(ConLaMafiaAiFerriCorti_Data)
name_walk=as.data.frame(readxl::read_xlsx("input_data/step_1_misc_data/Sicily1853-1951-1981.xlsx")) #Acemoglu et al. 2020 municipality name crosswalk
colnames(mori)=c("city", "province")
mori$city=tolower(mori$city)
name_walk$comune1951=tolower(name_walk$comune1951)
name_walk$comune1853=tolower(name_walk$comune1853)
name_walk$NAME_3=tolower(name_walk$NAME_3)
mori$comune1951=0
mori$comune1853=0
mori$other_name=0
colnames=c("comune1951", "comune1853", "NAME_3")
for (i in 1:nrow(mori)){
  tmp=vector(mode="numeric", length = 3)
  for (j in 1:length(colnames)){
    tmp[j]=ifelse(length(which(name_walk[,colnames[j]]==mori[i,"city"]))==0, 0, which(name_walk[,colnames[j]]==mori[i,"city"]))
  }
  if (sum(tmp)>0){
    if (tmp[1]>0){
      mori$comune1951[i]=name_walk$comune1951[tmp[1]]
    } else{
      mori$comune1951[i]=name_walk[tmp[which(tmp>0)[1]],colnames[1]]
    }
    if (tmp[2]>0){
      mori$comune1853[i]=name_walk$comune1853[tmp[2]]
    } else{
      mori$comune1853[i]=name_walk[tmp[which(tmp>0)[1]],colnames[2]]
    }
    if (tmp[3]>0){
      mori$other_name[i]=name_walk$NAME_3[tmp[3]]
    } else{
      mori$other_name[i]=name_walk[tmp[which(tmp>0)[1]],colnames[3]]
    }
  } else {
  }
}
mori$matched=ifelse(mori$comune1951==0 & mori$comune1853==0 & mori$other_name==0, 0, 1)
mori$alt_name=0
#fuzzy matching for some misspelling issues
for (i in which(mori$matched==0)){
  tmp=vector(mode="numeric", length = 3)
  for (j in 1:length(colnames)){
    tmp[j]=ifelse(length(agrep(mori[i,"city"], name_walk[,colnames[j]], max.distance = 0.1))==0, 0, agrep(mori[i,"city"], name_walk[,colnames[j]], max.distance = 0.1))
  }
  if (sum(tmp)>0){
    if (tmp[1]>0){
      mori$comune1951[i]=name_walk$comune1951[tmp[1]]
    } else{
      mori$comune1951[i]=name_walk[tmp[which(tmp>0)[1]],colnames[1]]
    }
    if (tmp[2]>0){
      mori$comune1853[i]=name_walk$comune1853[tmp[2]]
    } else{
      mori$comune1853[i]=name_walk[tmp[which(tmp>0)[1]],colnames[2]]
    }
    if (tmp[3]>0){
      mori$other_name[i]=name_walk$NAME_3[tmp[3]]
    } else{
      mori$other_name[i]=name_walk[tmp[which(tmp>0)[1]],colnames[3]]
    }
  } else {
  }
}
mori$matched=ifelse(mori$comune1951==0 & mori$comune1853==0 & mori$other_name==0, 0, 1)
mori[which(mori$matched==0),"alt_name"]=c("palermo") #from historical records
for (i in which(mori$matched==0)){
  tmp=vector(mode="numeric", length = 3)
  for (j in 1:length(colnames)){
    tmp[j]=ifelse(length(which(name_walk[,colnames[j]]==mori[i,"alt_name"]))==0, 0, which(name_walk[,colnames[j]]==mori[i,"alt_name"]))
  }
  if (sum(tmp)>0){
    if (tmp[1]>0){
      mori$comune1951[i]=name_walk$comune1951[tmp[1]]
    } else{
      mori$comune1951[i]=name_walk[tmp[which(tmp>0)[1]],colnames[1]]
    }
    if (tmp[2]>0){
      mori$comune1853[i]=name_walk$comune1853[tmp[2]]
    } else{
      mori$comune1853[i]=name_walk[tmp[which(tmp>0)[1]],colnames[2]]
    }
    if (tmp[3]>0){
      mori$other_name[i]=name_walk$NAME_3[tmp[3]]
    } else{
      mori$other_name[i]=name_walk[tmp[which(tmp>0)[1]],colnames[3]]
    }
  } else {
  }
}
mori$matched=ifelse(mori$comune1951==0 & mori$comune1853==0 & mori$other_name==0, 0, 1)
mori$alt_name=NULL
mori$matched=NULL
mori$mafia_1885=maps[match(mori$comune1853, maps$comune1853),4]
mori$mafia_1900=maps[match(mori$comune1853, maps$comune1853),5]
mori$mafia_1885_or_1900=ifelse((mori$mafia_1885+mori$mafia_1900)>0, 1, 0)
length(unique(mori$other_name)) #29 unique municipalities
length(unique(mori[which(mori$mafia_1885>0),]$other_name)) #8/29 were in 1885 map (27.5%)
length(unique(mori[which(mori$mafia_1900>0),]$other_name)) #20/29 were in 1900 map (69%)
length(unique(mori[which(mori$mafia_1885_or_1900>0),]$other_name)) #24/29 were in either map (82.8%)


maps=maps[,c(1:4,7)]
colnames(maps)=tolower(colnames(maps))
maps$mafia_1885_or_1900=ifelse((maps$mafia1885+maps$mafia1900)>0, 1, 0)
length(unique(maps$comune1853)) #333 unique municipalities
length(unique(maps[which(maps$mafia1885>0),]$comune1853)) #97/333 were in 1885 map (29.1%)
length(unique(maps[which(maps$mafia1900>0),]$comune1853)) #193/333 were in 1900 map (58%)
length(unique(maps[which(maps$mafia_1885_or_1900>0),]$comune1853)) #206/333 were in either map (61.9%)

library(sf)
commune=read_sf("input_data/step_1_misc_data/Municipal_Boundaries_of_Italy_2019/") #official shapefile of contemporary sicililian municipal boundaries

sicilia=commune[which(commune$COD_REG==19),]
sicilia$match_name=gsub(" ","",trimws(tolower(sicilia$COMUNE)))  
mori[which((mori$comune1951 %in% sicilia$match_name)==FALSE),3]

map_names=tolower(unique(mori$map_name))

sicilia_2=as.data.frame(sicilia)
sicilia_2$geometry=NULL
matching_matrix=matrix(nrow=nrow(sicilia_2), ncol=2)
matching_matrix[,1]=as.character(sicilia_2[,16])
matching_matrix[,2]=ifelse((tolower(matching_matrix[,1]) %in% mori$comune1951)==TRUE, 1, 0)

sicilia_2$mori=matching_matrix[match(sicilia_2$match_name, matching_matrix[,1]),2]
sicilia_2$mori=as.numeric(sicilia_2$mori)

sicilia$mori=sicilia_2$mori
rm(sicilia_2)
rm(matching_matrix)

    #########################################################
    ########  FIGURE ????, map of MORI RAID LOCATIONS   #####
    #########################################################
library(ggplot2)
ggplot() + 
  geom_sf(data = sicilia, size = .5, color = "black", fill = ifelse(sicilia$mori==0,"white", "red"), aes(geometry = geometry))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())+
  ggtitle("Locations of Comune Raided by Mori")

ggplot() + 
  geom_sf(data = sicilia, size = .5, color = "black", fill = ifelse(sicilia$mori==0,"white", "red"), aes(geometry = geometry))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

maps$comune1951=name_walk[match(maps$comune1853, name_walk$comune1853),"comune1951"]
maps$match_to_map=ifelse(((maps[,7]) %in%  unique(tolower(sicilia$match_name)))==FALSE, 0, 1)
maps$fuzzy_name=0
sicilia2=as.data.frame(sicilia)
sicilia2$geometry=NULL
for (i in 1:nrow(maps)){
  maps$fuzzy_name[i]=ifelse(maps$match_to_map[i]==0, sicilia[agrep(maps$comune1951[i], sicilia2$match_name, max.distance = 0.2)[1],"match_name"], maps$comune1951[i])
}
maps[which(maps$comune1951=="letojannigallodoro"),"fuzz_name"]="letojanni" #from manual inspection
sicilia$cutrera=ifelse(tolower(sicilia$match_name) %in% unique(maps$fuzzy_name[which(maps$mafia1900>0)]), 1, 0)
sicilia$damiani=ifelse(tolower(sicilia$match_name) %in% unique(maps$fuzzy_name[which(maps$mafia1885>0)]), 1, 0)
sicilia$either_mafia_map=ifelse((tolower(sicilia$match_name) %in% unique(maps$fuzzy_name[which(maps$mafia1885>0)]))|(tolower(sicilia$match_name) %in% unique(maps$fuzzy_name[which(maps$mafia1900>0)])), 1, 0)



load("/intermediate_outputs/sicilian_numident.rda")


mori_city=unique(append(append(append(mori$city, mori$other_name), mori$comune1853), mori$comune1951))
sicily_city=unique(numident$matched_commune)
mori_city[which((mori_city %in% sicily_city)==FALSE)]
numident$matched_commune=ifelse(grepl("termini imer*", numident$numident_origin)==TRUE, "termini imerese", numident$matched_commune)
mori_city[which((mori_city %in% sicily_city)==FALSE)]=c("palazzoadriano", "balestrate","piazzaarmerina", "agira", "sangiuseppejato", "sancipirello",
                                                        "bisacquino", "contessaentellina", "chiusasclafani", "francavilladisicilia", "palermo", 
                                                        "sancipirello", "francavilladisicilia", "piazzaarmerina", "agira", "aragona", "san giuseppe jato", "francavilladisicilia")
mori_city=unique(mori_city)
numident$mori=ifelse(numident$matched_commune %in% mori_city, 1, 0)

sum(numident$mori)/nrow(numident) #21.5% of sicilians in numident are from mori municipality, or 10,901
sum(numident[which(numident$matched_commune != "palermo"),]$mori)/nrow(numident) #4151 or 8.2% are non-palermo mori

numident$cutrera_1900=ifelse(numident$matched_commune %in% maps[which(maps$mafia1900>0),"comune1853"],1,0)
numident$damiani_1885=ifelse(numident$matched_commune %in% maps[which(maps$mafia1885>0),"comune1853"],1,0)
numident$cutrera_or_damiani_maps=ifelse(numident$matched_commune %in% maps[which(maps$mafia_1885_or_1900>0),"comune1853"],1,0)
sum(numident$cutrera_1900)/nrow(numident) #46% of sicilians in numident are from cutrera map municipalities or 23279
sum(numident$damiani_1885)/nrow(numident) #27.5 of sicilians in numident are from damiani map municipalities or 13925
sum(numident$cutrera_or_damiani_maps)/nrow(numident) #46.94% of sicilians in numident are from cutrera or damiani map municipalities or 23761
nrow(numident[which(numident$cutrera_or_damiani_maps==0 & numident$mori==1),])/nrow(numident) #2.8% of sicilians in numident are from mori areas but not damiani or cutrera areas or 1438
nrow(numident[which(numident$cutrera_or_damiani_maps==0 & numident$mori==1),])/nrow(numident[which(numident$mori==1),]) #13.2% of mori in numident are not damiani or cutrera areas or 1438


save(numident, file="intermediate_outputs/sicilian_numident.rda")