########################################################################################################
################# STEP 3H: CREATING REDLINING AND HOUSING DATA FOR HEXAGONS  ###########################
########################################################################################################
library(sf)
library(rjson)

redline=st_read("input_data/step_3_input_data/redline_data/mappinginequality.json") ##### DATA FROM UNIVERSITY OF RICHMOND MAPPING INEQUALITY PROJECT
cities=c("Brooklyn", "Baltimore", "Manhattan", "Cleveland", "Chicago", "Pittsburgh", "Philadelphia", "St. Louis", "Detroit",
         "Boston") #no cincinatti
redline=redline[which(redline$city %in% cities),]
descriptions=read.csv("input_data/step_3_input_data/redline_data/holc_ad_data.csv") ##### DATA FROM UNIVERSITY OF RICHMOND MAPPING INEQUALITY PROJECT
forms=read.csv("input_data/step_3_input_data/redline_data/City_Form_ids.csv") ##### DATA FROM UNIVERSITY OF RICHMOND MAPPING INEQUALITY PROJECT
forms=forms[which(forms$city %in% cities),]
redline$form_id=forms[match(redline$city, forms$city), "form_id"]

json_data <- fromJSON(file="input_data/step_3_input_data/redline_data/ad_data.json")##### DATA FROM UNIVERSITY OF RICHMOND MAPPING INEQUALITY PROJECT
descriptions=matrix(nrow=length(unique(json_data)), ncol=10)
for (i in 1:length(json_data)){
  tmp=json_data[[i]]
  descriptions[i,1]=ifelse(is.null(tmp$area_id), 0, tmp$area_id)
  descriptions[i,2]=ifelse(is.null(tmp$city), 0 ,tmp$city)
  descriptions[i,3]=ifelse(is.null(tmp$favorable_influences), 0, tmp$favorable_influences)
  descriptions[i,4]=ifelse(is.null(tmp$detrimental_influences), 0, tmp$detrimental_influences)
  descriptions[i,5]=ifelse(is.null(tmp$foreign_born_nationality), 0, tmp$foreign_born_nationality)
  descriptions[i,6]=ifelse(is.null(tmp$infiltration_of), 0, tmp$infiltration_of)
  descriptions[i,7]=ifelse(is.null(tmp$clarifying_remarks), 0, tmp$clarifying_remarks)
  descriptions[i,8]=ifelse(is.null(tmp$all_fields), 0, tmp$all_fields)
  descriptions[i,9]=ifelse(is.null(tmp$grade), 0, tmp$grade)
  descriptions[i,10]=ifelse(is.null(tmp$security_grade), 0, tmp$security_grade)
}
descriptions=as.data.frame(descriptions)
colnames(descriptions)=c("area_id", "city", "favorable_influences", "detrimental_influences", "foreign_born_nationality",
                         "infiltration_of", "clarifying_remarks", "all_fields", "grade", "security_grade")
descriptions=descriptions[which(descriptions$area_id %in% unique(redline$area_id)),]
descriptions$italian=0
descriptions$crime=0
for(i in 1:nrow(descriptions)){
  vec=vector(length = 8)
  vec_2=vector(length = 8)
    for (j in 3:10){
    vec[j-2]=ifelse(grepl("Italians", descriptions[i,j], ignore.case = TRUE)==TRUE|
                      grepl("Italian", descriptions[i,j], ignore.case = TRUE)==TRUE, 1, 0)
    vec_2[j-2]=ifelse(grepl("crime", descriptions[i,j], ignore.case = TRUE)==TRUE|
                      grepl("criminal", descriptions[i,j], ignore.case = TRUE)==TRUE |
                              grepl("gangster", descriptions[i,j], ignore.case = TRUE)==TRUE |
                              grepl("gang", descriptions[i,j], ignore.case = TRUE)==TRUE|
                              grepl("gangsterism", descriptions[i,j], ignore.case = TRUE)==TRUE|
                              grepl("violent", descriptions[i,j], ignore.case = TRUE)==TRUE|
                              grepl("violence", descriptions[i,j], ignore.case = TRUE)==TRUE, 1, 0)
  }
  descriptions[i,11]=ifelse(sum(vec)>0, 1, 0)
  descriptions[i,12]=ifelse(sum(vec_2)>0, 1, 0)
}

redline$italian=descriptions[match(redline$area_id, descriptions$area_id), "italian"]
redline[is.na(redline$italian),"italian"]=0
redline$crime_perception=descriptions[match(redline$area_id, descriptions$area_id), "crime"]
redline[is.na(redline$crime_perception),"crime_perception"]=0

#mapping
load("intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Brooklyn_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Manhattan_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")
load("intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")
Baltimore_redline=redline[which(redline$city=="Baltimore"),]
Baltimore_redline=st_transform(Baltimore_redline, st_crs(Baltimore_hexgrid))
Baltimore_redline=st_make_valid(Baltimore_redline)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
Baltimore_intersects=st_intersection(Baltimore_hexgrid, Baltimore_redline)
Baltimore_intersects$area_intersected=as.numeric(st_area(Baltimore_intersects))
Baltimore_redline$redline_area=as.numeric(st_area(Baltimore_redline))
Baltimore_redline=st_drop_geometry(Baltimore_redline)
Baltimore_intersects$redline_area=as.numeric(Baltimore_redline[match(Baltimore_intersects$area_id, Baltimore_redline$area_id), "redline_area"])
Baltimore_intersects$proportion_intersected=Baltimore_intersects$area_intersected/Baltimore_intersects$redline_area

Boston_redline=redline[which(redline$city=="Boston"),]
Boston_redline=st_transform(Boston_redline, st_crs(Boston_hexgrid))
Boston_redline=st_make_valid(Boston_redline)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
Boston_intersects=st_intersection(Boston_hexgrid, Boston_redline)
Boston_intersects$area_intersected=as.numeric(st_area(Boston_intersects))
Boston_redline$redline_area=as.numeric(st_area(Boston_redline))
Boston_redline=st_drop_geometry(Boston_redline)
Boston_intersects$redline_area=as.numeric(Boston_redline[match(Boston_intersects$area_id, Boston_redline$area_id), "redline_area"])
Boston_intersects$proportion_intersected=Boston_intersects$area_intersected/Boston_intersects$redline_area

Brooklyn_redline=redline[which(redline$city=="Brooklyn"),]
Brooklyn_redline=st_transform(Brooklyn_redline, st_crs(Brooklyn_hexgrid))
Brooklyn_redline=st_make_valid(Brooklyn_redline)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
Brooklyn_intersects=st_intersection(Brooklyn_hexgrid, Brooklyn_redline)
Brooklyn_intersects$area_intersected=as.numeric(st_area(Brooklyn_intersects))
Brooklyn_redline$redline_area=as.numeric(st_area(Brooklyn_redline))
Brooklyn_redline=st_drop_geometry(Brooklyn_redline)
Brooklyn_intersects$redline_area=as.numeric(Brooklyn_redline[match(Brooklyn_intersects$area_id, Brooklyn_redline$area_id), "redline_area"])
Brooklyn_intersects$proportion_intersected=Brooklyn_intersects$area_intersected/Brooklyn_intersects$redline_area

Chicago_redline=redline[which(redline$city=="Chicago"),]
Chicago_redline=st_transform(Chicago_redline, st_crs(Chicago_hexgrid))
Chicago_redline=st_make_valid(Chicago_redline)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
Chicago_intersects=st_intersection(Chicago_hexgrid, Chicago_redline)
Chicago_intersects$area_intersected=as.numeric(st_area(Chicago_intersects))
Chicago_redline$redline_area=as.numeric(st_area(Chicago_redline))
Chicago_redline=st_drop_geometry(Chicago_redline)
Chicago_intersects$redline_area=as.numeric(Chicago_redline[match(Chicago_intersects$area_id, Chicago_redline$area_id), "redline_area"])
Chicago_intersects$proportion_intersected=Chicago_intersects$area_intersected/Chicago_intersects$redline_area

Cleveland_redline=redline[which(redline$city=="Cleveland"),]
Cleveland_redline=st_transform(Cleveland_redline, st_crs(Cleveland_hexgrid))
Cleveland_redline=st_make_valid(Cleveland_redline)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
Cleveland_intersects=st_intersection(Cleveland_hexgrid, Cleveland_redline)
Cleveland_intersects$area_intersected=as.numeric(st_area(Cleveland_intersects))
Cleveland_redline$redline_area=as.numeric(st_area(Cleveland_redline))
Cleveland_redline=st_drop_geometry(Cleveland_redline)
Cleveland_intersects$redline_area=as.numeric(Cleveland_redline[match(Cleveland_intersects$area_id, Cleveland_redline$area_id), "redline_area"])
Cleveland_intersects$proportion_intersected=Cleveland_intersects$area_intersected/Cleveland_intersects$redline_area

Detroit_redline=redline[which(redline$city=="Detroit"),]
Detroit_redline=st_transform(Detroit_redline, st_crs(Detroit_hexgrid))
Detroit_redline=st_make_valid(Detroit_redline)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
Detroit_intersects=st_intersection(Detroit_hexgrid, Detroit_redline)
Detroit_intersects$area_intersected=as.numeric(st_area(Detroit_intersects))
Detroit_redline$redline_area=as.numeric(st_area(Detroit_redline))
Detroit_redline=st_drop_geometry(Detroit_redline)
Detroit_intersects$redline_area=as.numeric(Detroit_redline[match(Detroit_intersects$area_id, Detroit_redline$area_id), "redline_area"])
Detroit_intersects$proportion_intersected=Detroit_intersects$area_intersected/Detroit_intersects$redline_area

Manhattan_redline=redline[which(redline$city=="Manhattan"),]
Manhattan_redline=st_transform(Manhattan_redline, st_crs(Manhattan_hexgrid))
Manhattan_redline=st_make_valid(Manhattan_redline)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
Manhattan_intersects=st_intersection(Manhattan_hexgrid, Manhattan_redline)
Manhattan_intersects$area_intersected=as.numeric(st_area(Manhattan_intersects))
Manhattan_redline$redline_area=as.numeric(st_area(Manhattan_redline))
Manhattan_redline=st_drop_geometry(Manhattan_redline)
Manhattan_intersects$redline_area=as.numeric(Manhattan_redline[match(Manhattan_intersects$area_id, Manhattan_redline$area_id), "redline_area"])
Manhattan_intersects$proportion_intersected=Manhattan_intersects$area_intersected/Manhattan_intersects$redline_area

Philadelphia_redline=redline[which(redline$city=="Philadelphia"),]
Philadelphia_redline=st_transform(Philadelphia_redline, st_crs(Philadelphia_hexgrid))
Philadelphia_redline=st_make_valid(Philadelphia_redline)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
Philadelphia_intersects=st_intersection(Philadelphia_hexgrid, Philadelphia_redline)
Philadelphia_intersects$area_intersected=as.numeric(st_area(Philadelphia_intersects))
Philadelphia_redline$redline_area=as.numeric(st_area(Philadelphia_redline))
Philadelphia_redline=st_drop_geometry(Philadelphia_redline)
Philadelphia_intersects$redline_area=as.numeric(Philadelphia_redline[match(Philadelphia_intersects$area_id, Philadelphia_redline$area_id), "redline_area"])
Philadelphia_intersects$proportion_intersected=Philadelphia_intersects$area_intersected/Philadelphia_intersects$redline_area

Pittsburgh_redline=redline[which(redline$city=="Pittsburgh"),]
Pittsburgh_redline=st_transform(Pittsburgh_redline, st_crs(Pittsburgh_hexgrid))
Pittsburgh_redline=st_make_valid(Pittsburgh_redline)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
Pittsburgh_intersects=st_intersection(Pittsburgh_hexgrid, Pittsburgh_redline)
Pittsburgh_intersects$area_intersected=as.numeric(st_area(Pittsburgh_intersects))
Pittsburgh_redline$redline_area=as.numeric(st_area(Pittsburgh_redline))
Pittsburgh_redline=st_drop_geometry(Pittsburgh_redline)
Pittsburgh_intersects$redline_area=as.numeric(Pittsburgh_redline[match(Pittsburgh_intersects$area_id, Pittsburgh_redline$area_id), "redline_area"])
Pittsburgh_intersects$proportion_intersected=Pittsburgh_intersects$area_intersected/Pittsburgh_intersects$redline_area

StLouis_redline=redline[which(redline$city=="StLouis"),]
StLouis_redline=st_transform(StLouis_redline, st_crs(StLouis_hexgrid))
StLouis_redline=st_make_valid(StLouis_redline)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
StLouis_intersects=st_intersection(StLouis_hexgrid, StLouis_redline)
StLouis_intersects$area_intersected=as.numeric(st_area(StLouis_intersects))
StLouis_redline$redline_area=as.numeric(st_area(StLouis_redline))
StLouis_redline=st_drop_geometry(StLouis_redline)
StLouis_intersects$redline_area=as.numeric(StLouis_redline[match(StLouis_intersects$area_id, StLouis_redline$area_id), "redline_area"])
StLouis_intersects$proportion_intersected=StLouis_intersects$area_intersected/StLouis_intersects$redline_area

intersects=rbind.data.frame(Baltimore_intersects, Boston_intersects)
intersects=rbind.data.frame(intersects, Brooklyn_intersects)
intersects=rbind.data.frame(intersects, Cleveland_intersects)
intersects=rbind.data.frame(intersects, Chicago_intersects)
intersects=rbind.data.frame(intersects, Detroit_intersects)
intersects=rbind.data.frame(intersects, Manhattan_intersects)
intersects=rbind.data.frame(intersects, Philadelphia_intersects)
intersects=rbind.data.frame(intersects, Pittsburgh_intersects)
intersects=rbind.data.frame(intersects, StLouis_intersects)


load("analysis_data/combined_df.rda")
combined_df_redline=combined_df[which(combined_df$year==1920 & combined_df$city != "Cincinnati"),] #NO REDLINE MAP EXISTS FOR CINCINNATI
intersects$city_hex=paste0(intersects$city, "_",intersects$hex_id)
intersects$grade=gsub(" ","",intersects$grade)
combined_df_redline$redline_grade_C=0
combined_df_redline$redline_grade_D=0
combined_df_redline$redline_italian=0
combined_df_redline$redline_crime=0
for (i in 1:nrow(combined_df_redline)){
  tmp=intersects[which(intersects$city_hex==combined_df_redline$city_hex[i]),]
  combined_df_redline$redline_grade_C[i]=ifelse("C" %in% unique(tmp$grade), 1, 0)
  combined_df_redline$redline_grade_D[i]=ifelse("D" %in% unique(tmp$grade), 1, 0)
  combined_df_redline$redline_italian[i]=ifelse(combined_df_redline$city[i]=="StLouis", NA, ifelse(sum(tmp$italian)>0, 1, 0))    
  combined_df_redline$redline_crime[i]=ifelse(combined_df_redline$city[i]=="StLouis", NA, ifelse(sum(tmp$crime)>0, 1, 0))   
}

#now housing (will need to drop Manhattan)
hpi=read.csv(file = "input_data/step_3_input_data/redline_data/hpi_at_bdl_tract.csv") ###### HOUSING PRICE INDEX DATA FROM FHFA (FEDERAL HOUSING FINANCE AGENCY)
fips=c("42101", "24510", "25025", "17031", "39035", "26163", "36061", "36047", "42003", "29510")
hpi=hpi[which(substr(hpi$tract, 1, 5)  %in% fips),]
hpi_mat=matrix(nrow=length(unique(hpi$tract)), ncol=6)
hpi_mat[,1]=unique(hpi$tract)
for (i in 1:nrow(hpi_mat)){
  tryCatch({
  hpi_mat[i,2]=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==1990),"hpi"]
  hpi_mat[i,3]=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2000),"hpi"]
  hpi_mat[i,4]=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2010),"hpi"]
  hpi_mat[i,5]=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2020),"hpi"]
  }, error=function(e){})
}
for (i in 1:nrow(hpi_mat)){
  hpi_mat[i,2]=tryCatch({
    result=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==1980),"hpi"]
    if(length(result) == 0) NA else as.numeric(result)
  }, error=function(e) NA)
  hpi_mat[i,3]=tryCatch({
    result=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==1990),"hpi"]
    if(length(result) == 0) NA else as.numeric(result)
  }, error=function(e) NA)
  hpi_mat[i,4]=tryCatch({
    result=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2000),"hpi"]
    if(length(result) == 0) NA else as.numeric(result)
  }, error=function(e) NA)
  hpi_mat[i,5]=tryCatch({
    result=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2010),"hpi"]
    if(length(result) == 0) NA else as.numeric(result)
  }, error=function(e) NA)
  hpi_mat[i,6]=tryCatch({
    result=hpi[which(hpi$tract==hpi_mat[i,1] & hpi$year==2020),"hpi"]
    if(length(result) == 0) NA else as.numeric(result)
  }, error=function(e) NA)
}
hpi_mat=as.data.frame(hpi_mat)
colnames(hpi_mat)=c("tract", "hpi_1980","hpi_1990", "hpi_2000", "hpi_2010", "hpi_2020")
tracts=read_sf("input_data/step_3_input_data/redline_data/nhgis0027_shape/nhgis0027_shapefile_tl2018_us_tract_2018") ###### CENSUS TRACT SHAPEFILE FOR SPATIAL MERGE
tracts=tracts[which(substr(tracts$GEOID, 1, 5) %in% fips),]
tracts$hpi_1980=hpi_mat[match(tracts$GEOID, hpi_mat$tract), "hpi_1980"]
tracts$hpi_1990=hpi_mat[match(tracts$GEOID, hpi_mat$tract), "hpi_1990"]
tracts$hpi_2000=hpi_mat[match(tracts$GEOID, hpi_mat$tract), "hpi_2000"]
tracts$hpi_2010=hpi_mat[match(tracts$GEOID, hpi_mat$tract), "hpi_2010"]
tracts$hpi_2020=hpi_mat[match(tracts$GEOID, hpi_mat$tract), "hpi_2020"]

#Baltimore
Baltimore_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="24510"),]
Baltimore_tracts=st_transform(Baltimore_tracts, st_crs(Baltimore_hexgrid))
Baltimore_tracts=st_make_valid(Baltimore_tracts)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
Baltimore_intersects=st_intersection(Baltimore_hexgrid, Baltimore_tracts)
Baltimore_intersects$area_intersected=as.numeric(st_area(Baltimore_intersects))
Baltimore_tracts$tract_area=as.numeric(st_area(Baltimore_tracts))
Baltimore_tracts=st_drop_geometry(Baltimore_tracts)
Baltimore_intersects$city_hex=paste0("Baltimore","_",Baltimore_intersects$hex_id)

#need to create weighted averages
combined_df_Baltimore=combined_df_redline[which(combined_df_redline$city=="Baltimore"),]
combined_df_Baltimore$hpi_1980=0
combined_df_Baltimore$hpi_1990=0
combined_df_Baltimore$hpi_2000=0
combined_df_Baltimore$hpi_2010=0
combined_df_Baltimore$hpi_2020=0
for (i in 1:nrow(combined_df_Baltimore)){
  tmp=Baltimore_intersects[which(Baltimore_intersects$city_hex==combined_df_Baltimore$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Baltimore$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Baltimore$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Baltimore$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Baltimore$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Baltimore$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Baltimore$hpi_1980[i]=0
    combined_df_Baltimore$hpi_1990[i]=0
    combined_df_Baltimore$hpi_2000[i]=0
    combined_df_Baltimore$hpi_2010[i]=0
    combined_df_Baltimore$hpi_2020[i]=0
  }
}

#Boston
Boston_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="25025"),]
Boston_tracts=st_transform(Boston_tracts, st_crs(Boston_hexgrid))
Boston_tracts=st_make_valid(Boston_tracts)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
Boston_intersects=st_intersection(Boston_hexgrid, Boston_tracts)
Boston_intersects$area_intersected=as.numeric(st_area(Boston_intersects))
Boston_tracts$tract_area=as.numeric(st_area(Boston_tracts))
Boston_tracts=st_drop_geometry(Boston_tracts)
Boston_intersects$city_hex=paste0("Boston","_",Boston_intersects$hex_id)

#need to create weighted averages
combined_df_Boston=combined_df_redline[which(combined_df_redline$city=="Boston"),]
combined_df_Boston$hpi_1980=0
combined_df_Boston$hpi_1990=0
combined_df_Boston$hpi_2000=0
combined_df_Boston$hpi_2010=0
combined_df_Boston$hpi_2020=0
for (i in 1:nrow(combined_df_Boston)){
  tmp=Boston_intersects[which(Boston_intersects$city_hex==combined_df_Boston$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Boston$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Boston$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Boston$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Boston$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Boston$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Boston$hpi_1980[i]=0
    combined_df_Boston$hpi_1990[i]=0
    combined_df_Boston$hpi_2000[i]=0
    combined_df_Boston$hpi_2010[i]=0
    combined_df_Boston$hpi_2020[i]=0
  }
}

#Brooklyn
Brooklyn_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="36047"),]
Brooklyn_tracts=st_transform(Brooklyn_tracts, st_crs(Brooklyn_hexgrid))
Brooklyn_tracts=st_make_valid(Brooklyn_tracts)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
Brooklyn_intersects=st_intersection(Brooklyn_hexgrid, Brooklyn_tracts)
Brooklyn_intersects$area_intersected=as.numeric(st_area(Brooklyn_intersects))
Brooklyn_tracts$tract_area=as.numeric(st_area(Brooklyn_tracts))
Brooklyn_tracts=st_drop_geometry(Brooklyn_tracts)
Brooklyn_intersects$city_hex=paste0("Brooklyn","_",Brooklyn_intersects$hex_id)

#need to create weighted averages
combined_df_Brooklyn=combined_df_redline[which(combined_df_redline$city=="Brooklyn"),]
combined_df_Brooklyn$hpi_1980=0
combined_df_Brooklyn$hpi_1990=0
combined_df_Brooklyn$hpi_2000=0
combined_df_Brooklyn$hpi_2010=0
combined_df_Brooklyn$hpi_2020=0
for (i in 1:nrow(combined_df_Brooklyn)){
  tmp=Brooklyn_intersects[which(Brooklyn_intersects$city_hex==combined_df_Brooklyn$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Brooklyn$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Brooklyn$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Brooklyn$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Brooklyn$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Brooklyn$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Brooklyn$hpi_1980[i]=0
    combined_df_Brooklyn$hpi_1990[i]=0
    combined_df_Brooklyn$hpi_2000[i]=0
    combined_df_Brooklyn$hpi_2010[i]=0
    combined_df_Brooklyn$hpi_2020[i]=0
  }
}

#Cleveland
Cleveland_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="39035"),]
Cleveland_tracts=st_transform(Cleveland_tracts, st_crs(Cleveland_hexgrid))
Cleveland_tracts=st_make_valid(Cleveland_tracts)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
Cleveland_intersects=st_intersection(Cleveland_hexgrid, Cleveland_tracts)
Cleveland_intersects$area_intersected=as.numeric(st_area(Cleveland_intersects))
Cleveland_tracts$tract_area=as.numeric(st_area(Cleveland_tracts))
Cleveland_tracts=st_drop_geometry(Cleveland_tracts)
Cleveland_intersects$city_hex=paste0("Cleveland","_",Cleveland_intersects$hex_id)

#need to create weighted averages
combined_df_Cleveland=combined_df_redline[which(combined_df_redline$city=="Cleveland"),]
combined_df_Cleveland$hpi_1980=0
combined_df_Cleveland$hpi_1990=0
combined_df_Cleveland$hpi_2000=0
combined_df_Cleveland$hpi_2010=0
combined_df_Cleveland$hpi_2020=0
for (i in 1:nrow(combined_df_Cleveland)){
  tmp=Cleveland_intersects[which(Cleveland_intersects$city_hex==combined_df_Cleveland$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Cleveland$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Cleveland$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Cleveland$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Cleveland$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Cleveland$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Cleveland$hpi_1980[i]=0
    combined_df_Cleveland$hpi_1990[i]=0
    combined_df_Cleveland$hpi_2000[i]=0
    combined_df_Cleveland$hpi_2010[i]=0
    combined_df_Cleveland$hpi_2020[i]=0
  }
}

#Chicago
Chicago_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="17031"),]
Chicago_tracts=st_transform(Chicago_tracts, st_crs(Chicago_hexgrid))
Chicago_tracts=st_make_valid(Chicago_tracts)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
Chicago_intersects=st_intersection(Chicago_hexgrid, Chicago_tracts)
Chicago_intersects$area_intersected=as.numeric(st_area(Chicago_intersects))
Chicago_tracts$tract_area=as.numeric(st_area(Chicago_tracts))
Chicago_tracts=st_drop_geometry(Chicago_tracts)
Chicago_intersects$city_hex=paste0("Chicago","_",Chicago_intersects$hex_id)

#need to create weighted averages
combined_df_Chicago=combined_df_redline[which(combined_df_redline$city=="Chicago"),]
combined_df_Chicago$hpi_1980=0
combined_df_Chicago$hpi_1990=0
combined_df_Chicago$hpi_2000=0
combined_df_Chicago$hpi_2010=0
combined_df_Chicago$hpi_2020=0
for (i in 1:nrow(combined_df_Chicago)){
  tmp=Chicago_intersects[which(Chicago_intersects$city_hex==combined_df_Chicago$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Chicago$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Chicago$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Chicago$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Chicago$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Chicago$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Chicago$hpi_1980[i]=0
    combined_df_Chicago$hpi_1990[i]=0
    combined_df_Chicago$hpi_2000[i]=0
    combined_df_Chicago$hpi_2010[i]=0
    combined_df_Chicago$hpi_2020[i]=0
  }
}

#Detroit
Detroit_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="26163"),]
Detroit_tracts=st_transform(Detroit_tracts, st_crs(Detroit_hexgrid))
Detroit_tracts=st_make_valid(Detroit_tracts)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
Detroit_intersects=st_intersection(Detroit_hexgrid, Detroit_tracts)
Detroit_intersects$area_intersected=as.numeric(st_area(Detroit_intersects))
Detroit_tracts$tract_area=as.numeric(st_area(Detroit_tracts))
Detroit_tracts=st_drop_geometry(Detroit_tracts)
Detroit_intersects$city_hex=paste0("Detroit","_",Detroit_intersects$hex_id)

#need to create weighted averages
combined_df_Detroit=combined_df_redline[which(combined_df_redline$city=="Detroit"),]
combined_df_Detroit$hpi_1980=0
combined_df_Detroit$hpi_1990=0
combined_df_Detroit$hpi_2000=0
combined_df_Detroit$hpi_2010=0
combined_df_Detroit$hpi_2020=0
for (i in 1:nrow(combined_df_Detroit)){
  tmp=Detroit_intersects[which(Detroit_intersects$city_hex==combined_df_Detroit$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Detroit$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Detroit$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Detroit$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Detroit$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Detroit$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Detroit$hpi_1980[i]=0
    combined_df_Detroit$hpi_1990[i]=0
    combined_df_Detroit$hpi_2000[i]=0
    combined_df_Detroit$hpi_2010[i]=0
    combined_df_Detroit$hpi_2020[i]=0
  }
}

#Manhattan
Manhattan_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="36061"),]
Manhattan_tracts=st_transform(Manhattan_tracts, st_crs(Manhattan_hexgrid))
Manhattan_tracts=st_make_valid(Manhattan_tracts)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
Manhattan_intersects=st_intersection(Manhattan_hexgrid, Manhattan_tracts)
Manhattan_intersects$area_intersected=as.numeric(st_area(Manhattan_intersects))
Manhattan_tracts$tract_area=as.numeric(st_area(Manhattan_tracts))
Manhattan_tracts=st_drop_geometry(Manhattan_tracts)
Manhattan_intersects$city_hex=paste0("Manhattan","_",Manhattan_intersects$hex_id)

#need to create weighted averages
combined_df_Manhattan=combined_df_redline[which(combined_df_redline$city=="Manhattan"),]
combined_df_Manhattan$hpi_1980=0
combined_df_Manhattan$hpi_1990=0
combined_df_Manhattan$hpi_2000=0
combined_df_Manhattan$hpi_2010=0
combined_df_Manhattan$hpi_2020=0
for (i in 1:nrow(combined_df_Manhattan)){
  tmp=Manhattan_intersects[which(Manhattan_intersects$city_hex==combined_df_Manhattan$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Manhattan$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Manhattan$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Manhattan$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Manhattan$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Manhattan$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Manhattan$hpi_1980[i]=0
    combined_df_Manhattan$hpi_1990[i]=0
    combined_df_Manhattan$hpi_2000[i]=0
    combined_df_Manhattan$hpi_2010[i]=0
    combined_df_Manhattan$hpi_2020[i]=0
  }
}

#Philadelphia
Philadelphia_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="42101"),]
Philadelphia_tracts=st_transform(Philadelphia_tracts, st_crs(Philadelphia_hexgrid))
Philadelphia_tracts=st_make_valid(Philadelphia_tracts)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
Philadelphia_intersects=st_intersection(Philadelphia_hexgrid, Philadelphia_tracts)
Philadelphia_intersects$area_intersected=as.numeric(st_area(Philadelphia_intersects))
Philadelphia_tracts$tract_area=as.numeric(st_area(Philadelphia_tracts))
Philadelphia_tracts=st_drop_geometry(Philadelphia_tracts)
Philadelphia_intersects$city_hex=paste0("Philadelphia","_",Philadelphia_intersects$hex_id)

#need to create weighted averages
combined_df_Philadelphia=combined_df_redline[which(combined_df_redline$city=="Philadelphia"),]
combined_df_Philadelphia$hpi_1980=0
combined_df_Philadelphia$hpi_1990=0
combined_df_Philadelphia$hpi_2000=0
combined_df_Philadelphia$hpi_2010=0
combined_df_Philadelphia$hpi_2020=0
for (i in 1:nrow(combined_df_Philadelphia)){
  tmp=Philadelphia_intersects[which(Philadelphia_intersects$city_hex==combined_df_Philadelphia$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Philadelphia$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Philadelphia$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Philadelphia$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Philadelphia$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Philadelphia$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Philadelphia$hpi_1980[i]=0
    combined_df_Philadelphia$hpi_1990[i]=0
    combined_df_Philadelphia$hpi_2000[i]=0
    combined_df_Philadelphia$hpi_2010[i]=0
    combined_df_Philadelphia$hpi_2020[i]=0
  }
}

#Pittsburgh
Pittsburgh_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="42003"),]
Pittsburgh_tracts=st_transform(Pittsburgh_tracts, st_crs(Pittsburgh_hexgrid))
Pittsburgh_tracts=st_make_valid(Pittsburgh_tracts)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
Pittsburgh_intersects=st_intersection(Pittsburgh_hexgrid, Pittsburgh_tracts)
Pittsburgh_intersects$area_intersected=as.numeric(st_area(Pittsburgh_intersects))
Pittsburgh_tracts$tract_area=as.numeric(st_area(Pittsburgh_tracts))
Pittsburgh_tracts=st_drop_geometry(Pittsburgh_tracts)
Pittsburgh_intersects$city_hex=paste0("Pittsburgh","_",Pittsburgh_intersects$hex_id)

#need to create weighted averages
combined_df_Pittsburgh=combined_df_redline[which(combined_df_redline$city=="Pittsburgh"),]
combined_df_Pittsburgh$hpi_1980=0
combined_df_Pittsburgh$hpi_1990=0
combined_df_Pittsburgh$hpi_2000=0
combined_df_Pittsburgh$hpi_2010=0
combined_df_Pittsburgh$hpi_2020=0
for (i in 1:nrow(combined_df_Pittsburgh)){
  tmp=Pittsburgh_intersects[which(Pittsburgh_intersects$city_hex==combined_df_Pittsburgh$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Pittsburgh$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Pittsburgh$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Pittsburgh$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Pittsburgh$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Pittsburgh$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Pittsburgh$hpi_1980[i]=0
    combined_df_Pittsburgh$hpi_1990[i]=0
    combined_df_Pittsburgh$hpi_2000[i]=0
    combined_df_Pittsburgh$hpi_2010[i]=0
    combined_df_Pittsburgh$hpi_2020[i]=0
  }
}

#StLouis
StLouis_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="29510"),]
StLouis_tracts=st_transform(StLouis_tracts, st_crs(StLouis_hexgrid))
StLouis_tracts=st_make_valid(StLouis_tracts)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
StLouis_intersects=st_intersection(StLouis_hexgrid, StLouis_tracts)
StLouis_intersects$area_intersected=as.numeric(st_area(StLouis_intersects))
StLouis_tracts$tract_area=as.numeric(st_area(StLouis_tracts))
StLouis_tracts=st_drop_geometry(StLouis_tracts)
StLouis_intersects$city_hex=paste0("StLouis","_",StLouis_intersects$hex_id)

#need to create weighted averages
combined_df_StLouis=combined_df_redline[which(combined_df_redline$city=="StLouis"),]
combined_df_StLouis$hpi_1980=0
combined_df_StLouis$hpi_1990=0
combined_df_StLouis$hpi_2000=0
combined_df_StLouis$hpi_2010=0
combined_df_StLouis$hpi_2020=0
for (i in 1:nrow(combined_df_StLouis)){
  tmp=StLouis_intersects[which(StLouis_intersects$city_hex==combined_df_StLouis$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_StLouis$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_StLouis$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_StLouis$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_StLouis$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_StLouis$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_StLouis$hpi_1980[i]=0
    combined_df_StLouis$hpi_1990[i]=0
    combined_df_StLouis$hpi_2000[i]=0
    combined_df_StLouis$hpi_2010[i]=0
    combined_df_StLouis$hpi_2020[i]=0
  }
}


combined_df_redline=rbind.data.frame(combined_df_Baltimore, combined_df_Boston)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Brooklyn)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Cleveland)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Chicago)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Detroit)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Manhattan)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Philadelphia)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Pittsburgh)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_StLouis)

combined_df_redline_tract=combined_df_redline #for holding, before merge
#ALMOST TEHRE
hpi=readxl::read_xlsx( "input_data/step_3_input_data/redline_data/hpi_at_bdl_zip5.xlsx") ###### HOUSING PRICE INDEX DATA FROM FHFA (FEDERAL HOUSING FINANCE AGENCY)
colnames(hpi)=hpi[6,]
hpi=hpi[c(7:nrow(hpi)),]
hpi=as.data.frame(hpi)
zips=c("19019", "19092", "19093", "19099", "19101", "19102", "19103", "19104", "19105", "19106", "19107", "19108", "19109", "19110", "19111", "19112", "19114", "19115", "19116", "19118", "19119", "19120", "19121", "19122", "19123", "19124", "19125", "19126", "19127", "19128", "19129", "19130", "19131", "19132", "19133", "19134", "19135", "19136", "19137", "19138", "19139", "19140", "19141", "19142", "19143", "19144", "19145", "19146", "19147", "19148", "19149", "19150", "19151", "19152", "19153", "19154", "19155", "19160", "19161", "19162", "19170", "19171", "19172", "19173", "19175", "19176", "19177", "19178", "19179", "19181", "19182", "19183", "19184", "19185", "19187", "19188", "19190", "19191", "19192", "19193", "19194", "19195", "19196", "19197", "19244", "19255",
       "15106", "15120", "15136", "15201", "15202", "15203", "15204", "15205", "15206", "15207", "15208", "15209", "15210", "15211", "15212", "15213", "15214", "15215", "15216", "15217", "15218", "15219", "15220", "15221", "15222", "15223", "15224", "15225", "15226", "15227", "15228", "15229", "15230", "15231", "15232", "15233", "15234", "15235", "15236", "15237", "15238", "15239", "15240", "15241", "15242", "15243", "15244", "15250", "15251", "15252", "15253", "15254", "15255", "15257", "15258", "15259", "15260", "15261", "15262", "15264", "15265", "15267", "15268", "15270", "15272", "15274", "15275", "15276", "15277", "15278", "15279", "15281", "15282", "15283", "15286", "15289", "15290", "15295", "60007", "60018", "60106", "60131", "60601", "60602", "60603", "60604", "60605", "60606", "60607", "60608", "60609", "60610", "60611", "60612", "60613", "60614", "60615", "60616", "60617", "60618", "60619", "60620", "60621", "60622", "60623", "60624", "60625", "60626", "60628", "60629", "60630", "60631", "60632", "60633", "60634", "60636", "60637", "60638", "60639", "60640", "60641", "60642", "60643", "60644", "60645", "60646", "60647", "60649", "60651", "60652", "60653", "60654", "60655", "60656", "60657", "60659", "60660", "60661", "60664", "60666", "60668", "60669", "60670", "60673", "60674", "60675", "60677", "60678", "60680", "60681", "60682", "60684", "60685", "60686", "60687", "60688", "60689", "60690", "60691", "60693", "60694", "60695", "60696", "60697", "60699", "60701", "60707", "60712", "60714", "60803", "60827", "02108", "02109", "02110", "02111", "02112", "02113", "02114", "02115", "02116", "02117", "02118", "02119", "02120", "02121", "02122", "02123", "02124", "02125", "02126", "02127", "02128", "02129", "02130", "02131", "02132", "02133", "02134", "02135", "02136", "02151", "02152", "02163", "02186", "02196", "02199", "02201", "02203", "02204", "02205", "02206", "02210", "02211", "02212", "02215", "02217", "02222", "02241", "02283", "02284", "02293", "02297", "02298", "02458", "02467", "21201", "21202", "21203", "21205", "21206", "21207", "21208", "21209", "21210", "21211", "21212", "21213", "21214", "21215", "21216", "21217", "21218", "21222", "21223", "21224", "21225", "21226", "21227", "21228", "21229", "21230", "21231", "21233", "21234", "21235", "21236", "21237", "21239", "21240", "21241", "21250", "21251", "21263", "21264", "21270", "21273", "21275", "21278", "21279", "21281", "21287", "21289", "21290", "21297", "21298",
       "11201", "11202", "11203", "11204", "11205", "11206", "11207", "11208", "11209", "11210", "11211", "11212", "11213", "11214", "11215", "11216", "11217", "11218", "11219", "11220", "11221", "11222", "11223", "11224", "11225", "11226", "11228", "11229", "11230", "11231", "11232", "11233", "11234", "11235", "11236", "11237", "11238", "11239", "11241", "11242", "11243", "11245", "11247", "11249", "11251", "11252", "11256",
       "44101", "44102", "44103", "44104", "44105", "44106", "44108", "44109", "44110", "44111", "44112", "44113", "44114", "44115", "44117", "44118", "44119", "44120", "44121", "44122", "44124", "44125", "44126", "44127", "44128", "44129", "44130", "44131", "44134", "44135", "44142", "44143", "44144", "44181", "44188", "44190", "44191", "44192", "44193", "44194", "44195", "44197", "44198", "44199",
       "10001", "10002", "10003", "10004", "10005", "10006", "10007", "10008", "10009", "10010", "10011", "10012", "10013", "10014", "10016", "10017", "10018", "10019", "10020", "10021", "10022", "10023", "10024", "10025", "10026", "10027", "10028", "10029", "10030", "10031", "10032", "10033", "10034", "10035", "10036", "10037", "10038", "10039", "10040", "10041", "10043", "10044", "10045", "10055", "10060", "10065", "10069", "10075", "10080", "10081", "10087", "10090", "10101", "10102", "10103", "10104", "10105", "10106", "10107", "10108", "10109", "10110", "10111", "10112", "10113", "10114", "10115", "10116", "10117", "10118", "10119", "10120", "10121", "10122", "10123", "10124", "10125", "10126", "10128", "10129", "10130", "10131", "10132", "10133", "10138", "10150", "10151", "10152", "10153", "10154", "10155", "10156", "10157", "10158", "10159", "10160", "10162", "10163", "10164", "10165", "10166", "10167", "10168", "10169", "10170", "10171", "10172", "10173", "10174", "10175", "10176", "10177", "10178", "10179", "10185", "10199", "10203", "10211", "10212", "10213", "10242", "10249", "10256", "10258", "10259", "10260", "10261", "10265", "10268", "10269", "10270", "10271", "10272", "10273", "10274", "10275", "10276", "10277", "10278", "10279", "10280", "10281", "10282", "10285", "10286",
       "48120", "48127", "48201", "48202", "48203", "48204", "48205", "48206", "48207", "48208", "48209", "48210", "48211", "48212", "48213", "48214", "48215", "48216", "48217", "48219", "48221", "48222", "48223", "48224", "48226", "48227", "48228", "48231", "48232", "48233", "48234", "48235", "48236", "48238", "48239", "48242", "48243", "48244", "48255", "48260", "48264", "48265", "48266", "48267", "48268", "48269", "48272", "48275", "48277", "48278", "48279", "48288",
       "63101", "63102", "63103", "63104", "63105", "63106", "63107", "63108", "63109", "63110", "63111", "63112", "63113", "63114", "63115", "63116", "63117", "63118", "63119", "63120", "63121", "63122", "63123", "63124", "63125", "63126", "63127", "63128", "63129", "63130", "63131", "63132", "63133", "63134", "63135", "63136", "63137", "63138", "63139", "63140", "63141", "63143", "63144", "63145", "63146", "63147", "63155", "63156", "63157", "63158", "63160", "63163", "63166", "63167", "63169", "63171", "63177", "63178", "63179", "63180", "63182", "63188", "63195", "63197")
hpi=hpi[which(hpi$`Five-Digit ZIP Code` %in% zips),]
hpi$zip=hpi$`Five-Digit ZIP Code`
hpi$year=as.numeric(hpi$Year)
hpi$hpi=hpi$HPI
hpi_mat=matrix(nrow=length(unique(hpi$zip)), ncol=7)
hpi_mat=as.data.frame(hpi_mat)
hpi_mat[,1]=unique(hpi$zip)
for (i in 1:nrow(hpi_mat)){
    hpi_mat[i,2]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==1975),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
    hpi_mat[i,3]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==1980),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
    hpi_mat[i,4]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==1990),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
    hpi_mat[i,5]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==2000),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
    hpi_mat[i,6]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==2010),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
    hpi_mat[i,7]=tryCatch({
      result=hpi[which(hpi$zip==hpi_mat[i,1] & hpi$year==2020),"hpi"]
      if(length(result) == 0) NA else as.numeric(result)
      }, error=function(e) NA)
}
colnames(hpi_mat)=c("zip", "hpi_1975", "hpi_1980", "hpi_1990", "hpi_2000", "hpi_2010", "hpi_2020")
      #mapping
zip_shape=st_read("input_data/step_3_input_data/redline_data/nhgis0028_shape/nhgis0028_shapefile_tl2020_us_zcta_2020") ##### SHAPEFILES FOR MERGE
zip_shape=zip_shape[which(zip_shape$ZCTA5CE20 %in% zips),]
zip_shape$zip=zip_shape$ZCTA5CE20

zip_shape$hpi_1975=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_1975"]
zip_shape$hpi_1980=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_1980"]
zip_shape$hpi_1990=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_1990"]
zip_shape$hpi_2000=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_2000"]
zip_shape$hpi_2010=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_2010"]
zip_shape$hpi_2020=hpi_mat[match(zip_shape$zip, hpi_mat$zip), "hpi_2020"]

#Baltimore
Baltimore_zips=st_transform(zip_shape, st_crs(Baltimore_hexgrid))
Baltimore_zips=st_make_valid(Baltimore_zips)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
Baltimore_intersects=st_intersection(Baltimore_hexgrid, Baltimore_zips)
Baltimore_intersects$area_intersected=as.numeric(st_area(Baltimore_intersects))
Baltimore_zips$zip_area=as.numeric(st_area(Baltimore_zips))
Baltimore_zips=st_drop_geometry(Baltimore_zips)
Baltimore_intersects$city_hex=paste0("Baltimore","_",Baltimore_intersects$hex_id)


combined_df_Baltimore=combined_df_redline[which(combined_df_redline$city=="Baltimore"),]
combined_df_Baltimore$hpi_1975=0
combined_df_Baltimore$hpi_1980=0
combined_df_Baltimore$hpi_1990=0
combined_df_Baltimore$hpi_2000=0
combined_df_Baltimore$hpi_2010=0
combined_df_Baltimore$hpi_2020=0
for (i in 1:nrow(combined_df_Baltimore)){
  tmp=Baltimore_intersects[which(Baltimore_intersects$city_hex==combined_df_Baltimore$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Baltimore$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Baltimore$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Baltimore$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Baltimore$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Baltimore$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Baltimore$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Baltimore$hpi_1975[i]=0
    combined_df_Baltimore$hpi_1980[i]=0
    combined_df_Baltimore$hpi_1990[i]=0
    combined_df_Baltimore$hpi_2000[i]=0
    combined_df_Baltimore$hpi_2010[i]=0
    combined_df_Baltimore$hpi_2020[i]=0
  }
}

#Boston
Boston_zips=st_transform(zip_shape, st_crs(Boston_hexgrid))
Boston_zips=st_make_valid(Boston_zips)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
Boston_intersects=st_intersection(Boston_hexgrid, Boston_zips)
Boston_intersects$area_intersected=as.numeric(st_area(Boston_intersects))
Boston_zips$zip_area=as.numeric(st_area(Boston_zips))
Boston_zips=st_drop_geometry(Boston_zips)
Boston_intersects$city_hex=paste0("Boston","_",Boston_intersects$hex_id)


combined_df_Boston=combined_df_redline[which(combined_df_redline$city=="Boston"),]
combined_df_Boston$hpi_1975=0
combined_df_Boston$hpi_1980=0
combined_df_Boston$hpi_1990=0
combined_df_Boston$hpi_2000=0
combined_df_Boston$hpi_2010=0
combined_df_Boston$hpi_2020=0
for (i in 1:nrow(combined_df_Boston)){
  tmp=Boston_intersects[which(Boston_intersects$city_hex==combined_df_Boston$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Boston$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Boston$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Boston$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Boston$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Boston$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Boston$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Boston$hpi_1975[i]=0
    combined_df_Boston$hpi_1980[i]=0
    combined_df_Boston$hpi_1990[i]=0
    combined_df_Boston$hpi_2000[i]=0
    combined_df_Boston$hpi_2010[i]=0
    combined_df_Boston$hpi_2020[i]=0
  }
}

#Brooklyn
Brooklyn_zips=st_transform(zip_shape, st_crs(Brooklyn_hexgrid))
Brooklyn_zips=st_make_valid(Brooklyn_zips)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
Brooklyn_intersects=st_intersection(Brooklyn_hexgrid, Brooklyn_zips)
Brooklyn_intersects$area_intersected=as.numeric(st_area(Brooklyn_intersects))
Brooklyn_zips$zip_area=as.numeric(st_area(Brooklyn_zips))
Brooklyn_zips=st_drop_geometry(Brooklyn_zips)
Brooklyn_intersects$city_hex=paste0("Brooklyn","_",Brooklyn_intersects$hex_id)


combined_df_Brooklyn=combined_df_redline[which(combined_df_redline$city=="Brooklyn"),]
combined_df_Brooklyn$hpi_1975=0
combined_df_Brooklyn$hpi_1980=0
combined_df_Brooklyn$hpi_1990=0
combined_df_Brooklyn$hpi_2000=0
combined_df_Brooklyn$hpi_2010=0
combined_df_Brooklyn$hpi_2020=0
for (i in 1:nrow(combined_df_Brooklyn)){
  tmp=Brooklyn_intersects[which(Brooklyn_intersects$city_hex==combined_df_Brooklyn$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Brooklyn$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Brooklyn$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Brooklyn$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Brooklyn$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Brooklyn$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Brooklyn$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Brooklyn$hpi_1975[i]=0
    combined_df_Brooklyn$hpi_1980[i]=0
    combined_df_Brooklyn$hpi_1990[i]=0
    combined_df_Brooklyn$hpi_2000[i]=0
    combined_df_Brooklyn$hpi_2010[i]=0
    combined_df_Brooklyn$hpi_2020[i]=0
  }
}

#Cleveland
Cleveland_zips=st_transform(zip_shape, st_crs(Cleveland_hexgrid))
Cleveland_zips=st_make_valid(Cleveland_zips)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
Cleveland_intersects=st_intersection(Cleveland_hexgrid, Cleveland_zips)
Cleveland_intersects$area_intersected=as.numeric(st_area(Cleveland_intersects))
Cleveland_zips$zip_area=as.numeric(st_area(Cleveland_zips))
Cleveland_zips=st_drop_geometry(Cleveland_zips)
Cleveland_intersects$city_hex=paste0("Cleveland","_",Cleveland_intersects$hex_id)


combined_df_Cleveland=combined_df_redline[which(combined_df_redline$city=="Cleveland"),]
combined_df_Cleveland$hpi_1975=0
combined_df_Cleveland$hpi_1980=0
combined_df_Cleveland$hpi_1990=0
combined_df_Cleveland$hpi_2000=0
combined_df_Cleveland$hpi_2010=0
combined_df_Cleveland$hpi_2020=0
for (i in 1:nrow(combined_df_Cleveland)){
  tmp=Cleveland_intersects[which(Cleveland_intersects$city_hex==combined_df_Cleveland$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Cleveland$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Cleveland$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Cleveland$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Cleveland$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Cleveland$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Cleveland$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Cleveland$hpi_1975[i]=0
    combined_df_Cleveland$hpi_1980[i]=0
    combined_df_Cleveland$hpi_1990[i]=0
    combined_df_Cleveland$hpi_2000[i]=0
    combined_df_Cleveland$hpi_2010[i]=0
    combined_df_Cleveland$hpi_2020[i]=0
  }
}

#Chicago
Chicago_zips=st_transform(zip_shape, st_crs(Chicago_hexgrid))
Chicago_zips=st_make_valid(Chicago_zips)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
Chicago_intersects=st_intersection(Chicago_hexgrid, Chicago_zips)
Chicago_intersects$area_intersected=as.numeric(st_area(Chicago_intersects))
Chicago_zips$zip_area=as.numeric(st_area(Chicago_zips))
Chicago_zips=st_drop_geometry(Chicago_zips)
Chicago_intersects$city_hex=paste0("Chicago","_",Chicago_intersects$hex_id)


combined_df_Chicago=combined_df_redline[which(combined_df_redline$city=="Chicago"),]
combined_df_Chicago$hpi_1975=0
combined_df_Chicago$hpi_1980=0
combined_df_Chicago$hpi_1990=0
combined_df_Chicago$hpi_2000=0
combined_df_Chicago$hpi_2010=0
combined_df_Chicago$hpi_2020=0
for (i in 1:nrow(combined_df_Chicago)){
  tmp=Chicago_intersects[which(Chicago_intersects$city_hex==combined_df_Chicago$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Chicago$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Chicago$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Chicago$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Chicago$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Chicago$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Chicago$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Chicago$hpi_1975[i]=0
    combined_df_Chicago$hpi_1980[i]=0
    combined_df_Chicago$hpi_1990[i]=0
    combined_df_Chicago$hpi_2000[i]=0
    combined_df_Chicago$hpi_2010[i]=0
    combined_df_Chicago$hpi_2020[i]=0
  }
}

#Detroit
Detroit_zips=st_transform(zip_shape, st_crs(Detroit_hexgrid))
Detroit_zips=st_make_valid(Detroit_zips)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
Detroit_intersects=st_intersection(Detroit_hexgrid, Detroit_zips)
Detroit_intersects$area_intersected=as.numeric(st_area(Detroit_intersects))
Detroit_zips$zip_area=as.numeric(st_area(Detroit_zips))
Detroit_zips=st_drop_geometry(Detroit_zips)
Detroit_intersects$city_hex=paste0("Detroit","_",Detroit_intersects$hex_id)


combined_df_Detroit=combined_df_redline[which(combined_df_redline$city=="Detroit"),]
combined_df_Detroit$hpi_1975=0
combined_df_Detroit$hpi_1980=0
combined_df_Detroit$hpi_1990=0
combined_df_Detroit$hpi_2000=0
combined_df_Detroit$hpi_2010=0
combined_df_Detroit$hpi_2020=0
for (i in 1:nrow(combined_df_Detroit)){
  tmp=Detroit_intersects[which(Detroit_intersects$city_hex==combined_df_Detroit$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Detroit$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Detroit$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Detroit$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Detroit$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Detroit$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Detroit$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Detroit$hpi_1975[i]=0
    combined_df_Detroit$hpi_1980[i]=0
    combined_df_Detroit$hpi_1990[i]=0
    combined_df_Detroit$hpi_2000[i]=0
    combined_df_Detroit$hpi_2010[i]=0
    combined_df_Detroit$hpi_2020[i]=0
  }
}

#Manhattan
Manhattan_zips=st_transform(zip_shape, st_crs(Manhattan_hexgrid))
Manhattan_zips=st_make_valid(Manhattan_zips)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
Manhattan_intersects=st_intersection(Manhattan_hexgrid, Manhattan_zips)
Manhattan_intersects$area_intersected=as.numeric(st_area(Manhattan_intersects))
Manhattan_zips$zip_area=as.numeric(st_area(Manhattan_zips))
Manhattan_zips=st_drop_geometry(Manhattan_zips)
Manhattan_intersects$city_hex=paste0("Manhattan","_",Manhattan_intersects$hex_id)


combined_df_Manhattan=combined_df_redline[which(combined_df_redline$city=="Manhattan"),]
combined_df_Manhattan$hpi_1975=0
combined_df_Manhattan$hpi_1980=0
combined_df_Manhattan$hpi_1990=0
combined_df_Manhattan$hpi_2000=0
combined_df_Manhattan$hpi_2010=0
combined_df_Manhattan$hpi_2020=0
for (i in 1:nrow(combined_df_Manhattan)){
  tmp=Manhattan_intersects[which(Manhattan_intersects$city_hex==combined_df_Manhattan$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Manhattan$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Manhattan$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Manhattan$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Manhattan$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Manhattan$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Manhattan$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Manhattan$hpi_1975[i]=0
    combined_df_Manhattan$hpi_1980[i]=0
    combined_df_Manhattan$hpi_1990[i]=0
    combined_df_Manhattan$hpi_2000[i]=0
    combined_df_Manhattan$hpi_2010[i]=0
    combined_df_Manhattan$hpi_2020[i]=0
  }
}

#Philadelphia
Philadelphia_zips=st_transform(zip_shape, st_crs(Philadelphia_hexgrid))
Philadelphia_zips=st_make_valid(Philadelphia_zips)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
Philadelphia_intersects=st_intersection(Philadelphia_hexgrid, Philadelphia_zips)
Philadelphia_intersects$area_intersected=as.numeric(st_area(Philadelphia_intersects))
Philadelphia_zips$zip_area=as.numeric(st_area(Philadelphia_zips))
Philadelphia_zips=st_drop_geometry(Philadelphia_zips)
Philadelphia_intersects$city_hex=paste0("Philadelphia","_",Philadelphia_intersects$hex_id)


combined_df_Philadelphia=combined_df_redline[which(combined_df_redline$city=="Philadelphia"),]
combined_df_Philadelphia$hpi_1975=0
combined_df_Philadelphia$hpi_1980=0
combined_df_Philadelphia$hpi_1990=0
combined_df_Philadelphia$hpi_2000=0
combined_df_Philadelphia$hpi_2010=0
combined_df_Philadelphia$hpi_2020=0
for (i in 1:nrow(combined_df_Philadelphia)){
  tmp=Philadelphia_intersects[which(Philadelphia_intersects$city_hex==combined_df_Philadelphia$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Philadelphia$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Philadelphia$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Philadelphia$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Philadelphia$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Philadelphia$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Philadelphia$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Philadelphia$hpi_1975[i]=0
    combined_df_Philadelphia$hpi_1980[i]=0
    combined_df_Philadelphia$hpi_1990[i]=0
    combined_df_Philadelphia$hpi_2000[i]=0
    combined_df_Philadelphia$hpi_2010[i]=0
    combined_df_Philadelphia$hpi_2020[i]=0
  }
}

#Pittsburgh
Pittsburgh_zips=st_transform(zip_shape, st_crs(Pittsburgh_hexgrid))
Pittsburgh_zips=st_make_valid(Pittsburgh_zips)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
Pittsburgh_intersects=st_intersection(Pittsburgh_hexgrid, Pittsburgh_zips)
Pittsburgh_intersects$area_intersected=as.numeric(st_area(Pittsburgh_intersects))
Pittsburgh_zips$zip_area=as.numeric(st_area(Pittsburgh_zips))
Pittsburgh_zips=st_drop_geometry(Pittsburgh_zips)
Pittsburgh_intersects$city_hex=paste0("Pittsburgh","_",Pittsburgh_intersects$hex_id)


combined_df_Pittsburgh=combined_df_redline[which(combined_df_redline$city=="Pittsburgh"),]
combined_df_Pittsburgh$hpi_1975=0
combined_df_Pittsburgh$hpi_1980=0
combined_df_Pittsburgh$hpi_1990=0
combined_df_Pittsburgh$hpi_2000=0
combined_df_Pittsburgh$hpi_2010=0
combined_df_Pittsburgh$hpi_2020=0
for (i in 1:nrow(combined_df_Pittsburgh)){
  tmp=Pittsburgh_intersects[which(Pittsburgh_intersects$city_hex==combined_df_Pittsburgh$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_Pittsburgh$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_Pittsburgh$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_Pittsburgh$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_Pittsburgh$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_Pittsburgh$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_Pittsburgh$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_Pittsburgh$hpi_1975[i]=0
    combined_df_Pittsburgh$hpi_1980[i]=0
    combined_df_Pittsburgh$hpi_1990[i]=0
    combined_df_Pittsburgh$hpi_2000[i]=0
    combined_df_Pittsburgh$hpi_2010[i]=0
    combined_df_Pittsburgh$hpi_2020[i]=0
  }
}

#StLouis
StLouis_zips=st_transform(zip_shape, st_crs(StLouis_hexgrid))
StLouis_zips=st_make_valid(StLouis_zips)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
StLouis_intersects=st_intersection(StLouis_hexgrid, StLouis_zips)
StLouis_intersects$area_intersected=as.numeric(st_area(StLouis_intersects))
StLouis_zips$zip_area=as.numeric(st_area(StLouis_zips))
StLouis_zips=st_drop_geometry(StLouis_zips)
StLouis_intersects$city_hex=paste0("StLouis","_",StLouis_intersects$hex_id)


combined_df_StLouis=combined_df_redline[which(combined_df_redline$city=="StLouis"),]
combined_df_StLouis$hpi_1975=0
combined_df_StLouis$hpi_1980=0
combined_df_StLouis$hpi_1990=0
combined_df_StLouis$hpi_2000=0
combined_df_StLouis$hpi_2010=0
combined_df_StLouis$hpi_2020=0
for (i in 1:nrow(combined_df_StLouis)){
  tmp=StLouis_intersects[which(StLouis_intersects$city_hex==combined_df_StLouis$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$hpi_1975)==FALSE),]
    combined_df_StLouis$hpi_1975[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1975), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1980)==FALSE),]
    combined_df_StLouis$hpi_1980[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1980), 0)
    tmp2=tmp[which(is.na(tmp$hpi_1990)==FALSE),]
    combined_df_StLouis$hpi_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_1990), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2000)==FALSE),]
    combined_df_StLouis$hpi_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2000), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2010)==FALSE),]
    combined_df_StLouis$hpi_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2010), 0)
    tmp2=tmp[which(is.na(tmp$hpi_2020)==FALSE),]
    combined_df_StLouis$hpi_2020[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$hpi_2020), 0)
  } else{
    combined_df_StLouis$hpi_1975[i]=0
    combined_df_StLouis$hpi_1980[i]=0
    combined_df_StLouis$hpi_1990[i]=0
    combined_df_StLouis$hpi_2000[i]=0
    combined_df_StLouis$hpi_2010[i]=0
    combined_df_StLouis$hpi_2020[i]=0
  }
}

combined_df_redline=rbind.data.frame(combined_df_Baltimore, combined_df_Boston)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Brooklyn)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Cleveland)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Chicago)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Detroit)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Manhattan)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Philadelphia)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_Pittsburgh)
combined_df_redline=rbind.data.frame(combined_df_redline, combined_df_StLouis)

#now to merge zips and tracts files
colnames(combined_df_redline)[85]="hpi_zip_1980"
colnames(combined_df_redline)[86]="hpi_zip_1990"
colnames(combined_df_redline)[87]="hpi_zip_2000"
colnames(combined_df_redline)[88]="hpi_zip_2010"
colnames(combined_df_redline)[89]="hpi_zip_2020"
colnames(combined_df_redline)[90]="hpi_zip_1975"
colnames(combined_df_redline_tract)[85]="hpi_tract_1980"
colnames(combined_df_redline_tract)[86]="hpi_tract_1990"
colnames(combined_df_redline_tract)[87]="hpi_tract_2000"
colnames(combined_df_redline_tract)[88]="hpi_tract_2010"
colnames(combined_df_redline_tract)[89]="hpi_tract_2020"
combined_df_redline$hpi_tract_1980=combined_df_redline_tract[match(combined_df_redline$city_hex, combined_df_redline_tract$city_hex), "hpi_tract_1980"]
combined_df_redline$hpi_tract_1990=combined_df_redline_tract[match(combined_df_redline$city_hex, combined_df_redline_tract$city_hex), "hpi_tract_1990"]
combined_df_redline$hpi_tract_2000=combined_df_redline_tract[match(combined_df_redline$city_hex, combined_df_redline_tract$city_hex), "hpi_tract_2000"]
combined_df_redline$hpi_tract_2010=combined_df_redline_tract[match(combined_df_redline$city_hex, combined_df_redline_tract$city_hex), "hpi_tract_2010"]
combined_df_redline$hpi_tract_2020=combined_df_redline_tract[match(combined_df_redline$city_hex, combined_df_redline_tract$city_hex), "hpi_tract_2020"]

save(combined_df_redline, file="analysis_data/combined_df_redline.rda")
