########################################################################################################
################# STEP 3K: CONSTRUCTION OF CHICAGO HOMICIDE DATA  ######################################
########################################################################################################

library(lubridate)
library(tmaptools)
library(ggmap)
library(sf)
library(spdep)

data=read.csv(file="input_data/step_3_input_data/chicago_homicide.csv") #11018 total, 5107 from 1920 on, only 1 in 1931, data from Bienen and Rottinghause 2001
data$date=as.Date(lubridate::mdy(data$CircumstancesDateOfOffense))
data=data[which(nchar(data$CircumstancesAddress)>0),]

data$address=paste0(data$CircumstancesAddress, " Chicago, Il")

API_KEY=KEY ############## REPLICATING RESEARCHER NEEDS TO INSERT GOOGLE API TOKEN
register_google(API_KEY, write = TRUE) 
geocoded=geocode(data$address[2], output = "latlon")
geocoded=as.data.frame(geocoded)

local_list=list()
for (i in 1:nrow(data)){
  result <- tryCatch({
    geocoded=geocode(data$address[i], output = "latlon")
    geocoded=as.data.frame(geocoded)
    local_list[[i]]=geocoded
    
  }, error = function(e){})
}

data$lat=NA
data$lon=NA
for (i in 1:nrow(data)){
  data$lat[i]=as.numeric(local_list[[i]][2])
  data$lon[i]=as.numeric(local_list[[i]][1])
}
data=data[which(is.na(data$date)==FALSE),]
data=data[which(is.na(data$lon)==FALSE),] #(8956 geocded successfully, 81.3% success)

data=data[,c(116:119, 5, 17, 18, 22,23,47,56,104,113)] #note romaine rivera used data in a paper

save(data, file="intermediate_outputs/full_clean_geocoded_chicago_homicides.rda")
data2=data[which(lubridate::year(data$date)>=1920),] #4836 geocoded successfuly, 94.7% success rate
save(data2, file="intermediate_outputs/clean_geocoded_chicago_homicides_1920_to_1930.rda")


load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
load("intermediate_outputs/step_3_hex_mats/hex_mat_Chicago_1920.rda")


mapped_points=st_as_sf(data, coords=c("lon", "lat"), crs=st_crs(Chicago_hexgrid))
st_crs(mapped_points) <- 4326
mapped_points=st_transform(mapped_points, st_crs(Chicago_hexgrid))
hexgrid_chicago_homicide=st_intersection(mapped_points, Chicago_hexgrid)


n_hexgrid=nrow(Chicago_hexgrid)
hexgrid_chicago_homicide$year=lubridate::year(hexgrid_chicago_homicide$date)
n_years=length(unique(hexgrid_chicago_homicide$year))

mat=matrix(nrow=(n_years*n_hexgrid), ncol=10)
mat[,1]=rep(unique(Chicago_hexgrid$hex_id), each=n_years)
mat[,2]=rep(unique(hexgrid_chicago_homicide$year), by=n_hexgrid)
for (i in 1:nrow(mat)){
  mat[i,3]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2]),])
  mat[i,4]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 nchar(hexgrid_chicago_homicide$CircumstancesRelatedToOrganizedCrime)>0),])
  mat[i,5]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 hexgrid_chicago_homicide$DefendantEthnicity=="Italian"),])
  mat[i,6]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 hexgrid_chicago_homicide$VictimEthnicity=="Italian"),])
  mat[i,7]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 (hexgrid_chicago_homicide$DefendantEthnicity=="Italian" &
                                                    hexgrid_chicago_homicide$VictimEthnicity=="Italian")),])
  mat[i,8]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 (hexgrid_chicago_homicide$DefendantEthnicity=="Italian" |
                                                    hexgrid_chicago_homicide$VictimEthnicity=="Italian")),])
  mat[i,9]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                 hexgrid_chicago_homicide$year==mat[i,2] &
                                                 (hexgrid_chicago_homicide$DefendantEthnicity=="Italian" &
                                                    hexgrid_chicago_homicide$VictimEthnicity=="Italian") &
                                                 nchar(hexgrid_chicago_homicide$CircumstancesRelatedToOrganizedCrime)>0),])
  mat[i,10]=nrow(hexgrid_chicago_homicide[which(hexgrid_chicago_homicide$hex_id==mat[i,1] & 
                                                  hexgrid_chicago_homicide$year==mat[i,2] &
                                                  (hexgrid_chicago_homicide$DefendantEthnicity=="Italian" |
                                                     hexgrid_chicago_homicide$VictimEthnicity=="Italian") &
                                                  nchar(hexgrid_chicago_homicide$CircumstancesRelatedToOrganizedCrime)>0),])
}
mat=as.data.frame(mat)
colnames(mat)=c("hex_id", "year", "total_homicides", "organized_crime_homicides", "italian_defendent_homicides", 
                "italian_victim_homicides", "italian_defendent_or_victim_homicides", "italian_defendent_and_vicitim_homicides",
                "italian_defendent_or_victim_and_organized_crime_homicides", "italian_defendent_and_vicitim_and_organized_crime_homicides")

load("analysis_data/combined_df.rda")
combined_df=combined_df[which(combined_df$city=="Chicago" & combined_df$year==1920),]
mat=mat[which(mat$hex_id %in% combined_df$hex_id),]
mat$fixed_mori_set=combined_df[match(mat$hex_id, combined_df$hex_id), "fixed_mori_set"]
mat$any_damiani_or_cutrera=combined_df[match(mat$hex_id, combined_df$hex_id), "any_cutrera_or_damiani_maps_sicilians_1920"]
mat$mafia_pre_hex=combined_df[match(mat$hex_id, combined_df$hex_id), "mafia_pre_hex"]
mat$post_gangi=ifelse(mat$year>=1926, 1, 0)
mat$post_raids=ifelse(mat$year>=1927, 1, 0)
mat$post_treat_gangi=mat$post_gangi*mat$fixed_mori_set
mat$post_treat_raids=mat$post_raids*mat$fixed_mori_set
mat$post_treat_gangi_damiani_or_cutrera=mat$post_gangi*mat$any_damiani_or_cutrera
mat$post_treat_raids_damiani_or_cutrera=mat$post_raids*mat$any_damiani_or_cutrera
mat$post_treat_mafia_pre_hex=mat$post_gangi*mat$mafia_pre_hex
mat$post_treat_raids_mafia_pre_hex=mat$post_raids*mat$mafia_pre_hex

mat=mat[which(mat$year != 1931),]
murder_data=merge(hex_mat_Chicago_1920, mat, by.x="hex_id", by.y = "hex_id")

nb=poly2nb(Chicago_hexgrid)
nb_matrix=nb2mat(nb, style="B")
mori_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(mat[which(mat$fixed_mori_set==1),"hex_id"])),]
vec=vector(mode="numeric", length=ncol(mori_nb))
for (j in 1:ncol(mori_nb)){
  vec[j]=sum(mori_nb[,j])
}
neighbors=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors=st_drop_geometry(neighbors)
neighbors=neighbors[which((neighbors$hex_id %in% unique(mat[which(mat$fixed_mori_set==1),"hex_id"]))==FALSE),]
neighbors_2nd_degree_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(neighbors)),]
vec=vector(mode="numeric", length=ncol(neighbors_2nd_degree_nb))
for (j in 1:ncol(neighbors_2nd_degree_nb)){
  vec[j]=sum(neighbors_2nd_degree_nb[,j])
}
neighbors2=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors2=st_drop_geometry(neighbors2)
neighbors2=neighbors2[which((neighbors2$hex_id %in% unique(mat[which(mat$fixed_mori_set==1),"hex_id"]))==FALSE),]
neighbors2=neighbors2[which((neighbors2 %in% unique(neighbors))==FALSE)]
neighbors_1=neighbors
neighbors=unique(append(neighbors, neighbors2))
mat$mori_neighbor=ifelse(mat$hex_id %in% neighbors, 1, 0)
mat$mori_neighbor_1=ifelse(mat$hex_id %in% neighbors_1, 1, 0)
mat$mori_neighbor_2=ifelse(mat$hex_id %in% neighbors2, 1, 0)
mat$neighbors_post_raid=mat$mori_neighbor*mat$post_raids
mat$neighbors_post_gangi=mat$mori_neighbor*mat$post_gangi
mat$neighbors_post_raid_1=mat$mori_neighbor_1*mat$post_raids
mat$neighbors_post_gangi_1=mat$mori_neighbor_1*mat$post_gangi
mat$neighbors_post_raid_2=mat$mori_neighbor_2*mat$post_raids
mat$neighbors_post_gangi_2=mat$mori_neighbor_2*mat$post_gangi
#to placbeo damiani or cutrera
placebo_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(mat[which(mat$any_damiani_or_cutrera==1),"hex_id"])),]
vec=vector(mode="numeric", length=ncol(placebo_nb))
for (j in 1:ncol(placebo_nb)){
  vec[j]=sum(placebo_nb[,j])
}
neighbors=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors=st_drop_geometry(neighbors)
neighbors=neighbors[which((neighbors$hex_id %in% unique(mat[which(mat$any_damiani_or_cutrera==1),"hex_id"]))==FALSE),]
neighbors_2nd_degree_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(neighbors)),]
vec=vector(mode="numeric", length=ncol(neighbors_2nd_degree_nb))
for (j in 1:ncol(neighbors_2nd_degree_nb)){
  vec[j]=sum(neighbors_2nd_degree_nb[,j])
}
neighbors2=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors2=st_drop_geometry(neighbors2)
neighbors2=neighbors2[which((neighbors2$hex_id %in% unique(mat[which(mat$fixed_mori_set==1),"hex_id"]))==FALSE),]
neighbors2=neighbors2[which((neighbors2 %in% unique(neighbors))==FALSE)]
neighbors_1=neighbors
neighbors=unique(append(neighbors, neighbors2))
mat$damiani_or_cutrera_neighbor=ifelse(mat$hex_id %in% neighbors, 1, 0)
mat$damiani_or_cutrera_neighbor_1=ifelse(mat$hex_id %in% neighbors_1, 1, 0)
mat$damiani_or_cutrera_neighbor_2=ifelse(mat$hex_id %in% neighbors2, 1, 0)
mat$damiani_or_cutrera_neighbors_post_raid=mat$damiani_or_cutrera_neighbor*mat$post_raids
mat$damiani_or_cutrera_neighbors_post_gangi=mat$damiani_or_cutrera_neighbor*mat$post_gangi
mat$damiani_or_cutrera_neighbors_post_raid_1=mat$damiani_or_cutrera_neighbor_1*mat$post_raids
mat$damiani_or_cutrera_neighbors_post_gangi_1=mat$damiani_or_cutrera_neighbor_1*mat$post_gangi
mat$damiani_or_cutrera_neighbors_post_raid_2=mat$damiani_or_cutrera_neighbor_2*mat$post_raids
mat$damiani_or_cutrera_neighbors_post_gangi_2=mat$damiani_or_cutrera_neighbor_2*mat$post_gangi

#to placbeo pre-1926 crime
placebo_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(mat[which(mat$mafia_pre_hex==1),"hex_id"])),]
vec=vector(mode="numeric", length=ncol(placebo_nb))
for (j in 1:ncol(placebo_nb)){
  vec[j]=sum(placebo_nb[,j])
}
neighbors=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors=st_drop_geometry(neighbors)
neighbors=neighbors[which((neighbors$hex_id %in% unique(mat[which(mat$mafia_pre_hex==1),"hex_id"]))==FALSE),]
neighbors_2nd_degree_nb=nb_matrix[which(Chicago_hexgrid$hex_id %in% unique(neighbors)),]
vec=vector(mode="numeric", length=ncol(neighbors_2nd_degree_nb))
for (j in 1:ncol(neighbors_2nd_degree_nb)){
  vec[j]=sum(neighbors_2nd_degree_nb[,j])
}
neighbors2=Chicago_hexgrid[which(vec>0), "hex_id"]
neighbors2=st_drop_geometry(neighbors2)
neighbors2=neighbors2[which((neighbors2$hex_id %in% unique(mat[which(mat$fixed_mori_set==1),"hex_id"]))==FALSE),]
neighbors2=neighbors2[which((neighbors2 %in% unique(neighbors))==FALSE)]
neighbors_1=neighbors
neighbors=unique(append(neighbors, neighbors2))
mat$mafia_pre_hex_neighbor=ifelse(mat$hex_id %in% neighbors, 1, 0)
mat$mafia_pre_hex_neighbor_1=ifelse(mat$hex_id %in% neighbors_1, 1, 0)
mat$mafia_pre_hex_neighbor_2=ifelse(mat$hex_id %in% neighbors2, 1, 0)
mat$mafia_pre_hex_neighbors_post_raid=mat$mafia_pre_hex_neighbor*mat$post_raids
mat$mafia_pre_hex_neighbors_post_gangi=mat$mafia_pre_hex_neighbor*mat$post_gangi
mat$mafia_pre_hex_neighbors_post_raid_1=mat$mafia_pre_hex_neighbor_1*mat$post_raids
mat$mafia_pre_hex_neighbors_post_gangi_1=mat$mafia_pre_hex_neighbor_1*mat$post_gangi
mat$mafia_pre_hex_neighbors_post_raid_2=mat$mafia_pre_hex_neighbor_2*mat$post_raids
mat$mafia_pre_hex_neighbors_post_gangi_2=mat$mafia_pre_hex_neighbor_2*mat$post_gangi

save(mat, file="analysis_data/clean_geocoded_chicago_murders_with_neighbors.rda")
