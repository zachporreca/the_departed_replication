########################################################################################################
################# STEP 3I: CREATING OPPORTUNITY INSIGHTS DATA FOR HEXAGONS  ############################
########################################################################################################

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
load("intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")

oi_tract_covariates=read.csv(file="input_data/step_3_input_data/opportunity_insights/tract_covariates.csv") ##### OPPORTUNITY INSIGHTS DATA
oi_tract_covariates$state=ifelse(nchar(oi_tract_covariates$state)<2, paste0("0",oi_tract_covariates$state), oi_tract_covariates$state)
oi_tract_covariates$county=ifelse(nchar(oi_tract_covariates$county)==2, paste0("0",oi_tract_covariates$county), 
                                  ifelse(nchar(oi_tract_covariates$county)==1, paste0("00",oi_tract_covariates$county), oi_tract_covariates$county))
oi_tract_covariates$tract=ifelse(nchar(oi_tract_covariates$tract)==3, paste0("000",oi_tract_covariates$tract), 
                                 ifelse(nchar(oi_tract_covariates$tract)==4, paste0("00",oi_tract_covariates$tract), 
                                        ifelse(nchar(oi_tract_covariates$tract)==5, paste0("0",oi_tract_covariates$tract),oi_tract_covariates$tract)))
oi_tract_covariates$GEOID=paste0(oi_tract_covariates$state,oi_tract_covariates$county,oi_tract_covariates$tract)

oi_tract_early=read.csv(file="input_data/step_3_input_data/opportunity_insights/tract_outcomes_early.csv") ##### OPPORTUNITY INSIGHTS DATA
oi_tract_early=oi_tract_early[,which(grepl("jail", colnames(oi_tract_early))==TRUE | grepl("two_par", colnames(oi_tract_early))==TRUE |
                                       grepl("work", colnames(oi_tract_early))==TRUE | grepl("staytract", colnames(oi_tract_early))==TRUE|
                                       colnames(oi_tract_early) %in% c("state", "county", "tract"))]
oi_tract_early=oi_tract_early[,which(grepl("_n", colnames(oi_tract_early))==FALSE & grepl("p1", colnames(oi_tract_early))==FALSE &
                                       grepl("p25", colnames(oi_tract_early))==FALSE & grepl("p50", colnames(oi_tract_early))==FALSE &
                                       grepl("p75", colnames(oi_tract_early))==FALSE & grepl("p100", colnames(oi_tract_early))==FALSE &
                                       grepl("_se", colnames(oi_tract_early))==FALSE)]
oi_tract_early$state=ifelse(nchar(oi_tract_early$state)<2, paste0("0",oi_tract_early$state), oi_tract_early$state)
oi_tract_early$county=ifelse(nchar(oi_tract_early$county)==2, paste0("0",oi_tract_early$county), 
                             ifelse(nchar(oi_tract_early$county)==1, paste0("00",oi_tract_early$county), oi_tract_early$county))
oi_tract_early$tract=ifelse(nchar(oi_tract_early$tract)==3, paste0("000",oi_tract_early$tract), 
                            ifelse(nchar(oi_tract_early$tract)==4, paste0("00",oi_tract_early$tract), 
                                   ifelse(nchar(oi_tract_early$tract)==5, paste0("0",oi_tract_early$tract),oi_tract_early$tract)))
oi_tract_early$GEOID=paste0(oi_tract_early$state,oi_tract_early$county,oi_tract_early$tract)
crosswalk=read.csv(file="input_data/step_3_input_data/opportunity_insights/us_tract_2010_2020_crosswalk.csv")
oi_tract_early$GEOID=crosswalk[match(oi_tract_early$GEOID, crosswalk$tract_2010_1), "tract_2020"]
oi_tract_covariates$GEOID=crosswalk[match(oi_tract_covariates$GEOID, crosswalk$tract_2010_1), "tract_2020"]

load("analysis_data/combined_df.rda")
#tracts first
tracts=read_sf("input_data/step_3_input_data/redline_data/nhgis0027_shape/nhgis0027_shapefile_tl2018_us_tract_2018")

#Baltimore
Baltimore_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="24510"),]
Baltimore_tracts=merge(Baltimore_tracts, oi_tract_covariates, by="GEOID")
Baltimore_tracts=merge(Baltimore_tracts, oi_tract_early, by="GEOID")
Baltimore_tracts=st_transform(Baltimore_tracts, st_crs(Baltimore_hexgrid))
Baltimore_tracts=st_make_valid(Baltimore_tracts)
Baltimore_tracts$tract_area=as.numeric(st_area(Baltimore_tracts))
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
Baltimore_hexgrid$hex_area=as.numeric(st_area(Baltimore_hexgrid))
Baltimore_intersects=st_intersection(Baltimore_hexgrid, Baltimore_tracts)
Baltimore_intersects$area_intersected=as.numeric(st_area(Baltimore_intersects))
Baltimore_tracts=st_drop_geometry(Baltimore_tracts)
Baltimore_intersects$city_hex=paste0("Baltimore","_",Baltimore_intersects$hex_id)
Baltimore_intersects$prop_hex_intersected=Baltimore_intersects$area_intersected/Baltimore_intersects$hex_area
Baltimore_intersects$prop_tract_intersected=Baltimore_intersects$area_intersected/Baltimore_intersects$tract_area
#need to create weighted averages
combined_df_Baltimore=combined_df[which(combined_df$city=="Baltimore"),]
combined_df_Baltimore$stay_tract_white=0
combined_df_Baltimore$stay_tract_pooled=0
combined_df_Baltimore$stay_tract_white_male=0
combined_df_Baltimore$stay_tract_pooled_male=0
combined_df_Baltimore$stay_tract_white_female=0
combined_df_Baltimore$stay_tract_pooled_female=0
combined_df_Baltimore$two_parent_white=0
combined_df_Baltimore$two_parent_pooled=0
combined_df_Baltimore$work_32_white=0
combined_df_Baltimore$work_24_white=0
combined_df_Baltimore$work_32_pooled=0
combined_df_Baltimore$work_24_pooled=0
combined_df_Baltimore$work_32_white_male=0
combined_df_Baltimore$work_24_white_male=0
combined_df_Baltimore$work_32_pooled_male=0
combined_df_Baltimore$work_24_pooled_male=0
combined_df_Baltimore$jail_white=0
combined_df_Baltimore$jail_pooled=0
combined_df_Baltimore$jail_white_male=0
combined_df_Baltimore$jail_pooled_male=0
combined_df_Baltimore$foreign_born_share_2010=0
combined_df_Baltimore$employment_2000=0
combined_df_Baltimore$college_rate_2000=0
combined_df_Baltimore$college_rate_2010=0
combined_df_Baltimore$black_2000=0
combined_df_Baltimore$black_2010=0
combined_df_Baltimore$white_2000=0
combined_df_Baltimore$white_2010=0
combined_df_Baltimore$median_income_1990=0
combined_df_Baltimore$median_income_2016=0
combined_df_Baltimore$poor_1990=0
combined_df_Baltimore$poor_2000=0
combined_df_Baltimore$poor_2010=0
combined_df_Baltimore$rent_2015=0
combined_df_Baltimore$single_parent_share_1990=0
combined_df_Baltimore$single_parent_share_2000=0
combined_df_Baltimore$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Baltimore)){
  tmp=Baltimore_intersects[which(Baltimore_intersects$city_hex==combined_df_Baltimore$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Baltimore$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Baltimore$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Baltimore$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Baltimore$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Baltimore$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Baltimore$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Baltimore$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Baltimore$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Baltimore$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Baltimore$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Baltimore$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Baltimore$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Baltimore$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Baltimore$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Baltimore$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Baltimore$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Baltimore$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Baltimore$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Baltimore$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Baltimore$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Baltimore$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Baltimore$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Baltimore$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Baltimore$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Baltimore$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Baltimore$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Baltimore$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Baltimore$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Baltimore$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Baltimore$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Baltimore$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Baltimore$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Baltimore$stay_tract_white[i]=0
    combined_df_Baltimore$stay_tract_pooled[i]=0
    combined_df_Baltimore$stay_tract_white_male[i]=0
    combined_df_Baltimore$stay_tract_pooled_male[i]=0
    combined_df_Baltimore$stay_tract_white_female[i]=0
    combined_df_Baltimore$stay_tract_pooled_female[i]=0
    combined_df_Baltimore$two_parent_white[i]=0
    combined_df_Baltimore$two_parent_pooled[i]=0
    combined_df_Baltimore$work_32_white[i]=0
    combined_df_Baltimore$work_24_white[i]=0
    combined_df_Baltimore$work_32_pooled[i]=0
    combined_df_Baltimore$work_24_pooled[i]=0
    combined_df_Baltimore$work_32_white_male[i]=0
    combined_df_Baltimore$work_24_white_male[i]=0
    combined_df_Baltimore$work_32_pooled_male[i]=0
    combined_df_Baltimore$work_24_pooled_male[i]=0
    combined_df_Baltimore$jail_white[i]=0
    combined_df_Baltimore$jail_pooled[i]=0
    combined_df_Baltimore$jail_white_male[i]=0
    combined_df_Baltimore$jail_pooled_male[i]=0
    combined_df_Baltimore$foreign_born_share_2010[i]=0
    combined_df_Baltimore$employment_2000[i]=0
    combined_df_Baltimore$college_rate_2000[i]=0
    combined_df_Baltimore$college_rate_2010[i]=0
    combined_df_Baltimore$black_2000[i]=0
    combined_df_Baltimore$black_2010[i]=0
    combined_df_Baltimore$white_2000[i]=0
    combined_df_Baltimore$white_2010[i]=0
    combined_df_Baltimore$median_income_1990[i]=0
    combined_df_Baltimore$median_income_2016[i]=0
    combined_df_Baltimore$poor_1990[i]=0
    combined_df_Baltimore$poor_2000[i]=0
    combined_df_Baltimore$poor_2010[i]=0
    combined_df_Baltimore$rent_2015[i]=0
    combined_df_Baltimore$single_parent_share_1990[i]=0
    combined_df_Baltimore$single_parent_share_2000[i]=0
    combined_df_Baltimore$single_parent_share_2010[i]=0
  }
}

#Brooklyn
Brooklyn_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="36047"),]
Brooklyn_tracts=merge(Brooklyn_tracts, oi_tract_covariates, by="GEOID")
Brooklyn_tracts=merge(Brooklyn_tracts, oi_tract_early, by="GEOID")
Brooklyn_tracts=st_transform(Brooklyn_tracts, st_crs(Brooklyn_hexgrid))
Brooklyn_tracts=st_make_valid(Brooklyn_tracts)
Brooklyn_tracts$tract_area=as.numeric(st_area(Brooklyn_tracts))
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
Brooklyn_hexgrid$hex_area=as.numeric(st_area(Brooklyn_hexgrid))
Brooklyn_intersects=st_intersection(Brooklyn_hexgrid, Brooklyn_tracts)
Brooklyn_intersects$area_intersected=as.numeric(st_area(Brooklyn_intersects))
Brooklyn_tracts=st_drop_geometry(Brooklyn_tracts)
Brooklyn_intersects$city_hex=paste0("Brooklyn","_",Brooklyn_intersects$hex_id)
Brooklyn_intersects$prop_hex_intersected=Brooklyn_intersects$area_intersected/Brooklyn_intersects$hex_area
Brooklyn_intersects$prop_tract_intersected=Brooklyn_intersects$area_intersected/Brooklyn_intersects$tract_area
#need to create weighted averages
combined_df_Brooklyn=combined_df[which(combined_df$city=="Brooklyn"),]
combined_df_Brooklyn$stay_tract_white=0
combined_df_Brooklyn$stay_tract_pooled=0
combined_df_Brooklyn$stay_tract_white_male=0
combined_df_Brooklyn$stay_tract_pooled_male=0
combined_df_Brooklyn$stay_tract_white_female=0
combined_df_Brooklyn$stay_tract_pooled_female=0
combined_df_Brooklyn$two_parent_white=0
combined_df_Brooklyn$two_parent_pooled=0
combined_df_Brooklyn$work_32_white=0
combined_df_Brooklyn$work_24_white=0
combined_df_Brooklyn$work_32_pooled=0
combined_df_Brooklyn$work_24_pooled=0
combined_df_Brooklyn$work_32_white_male=0
combined_df_Brooklyn$work_24_white_male=0
combined_df_Brooklyn$work_32_pooled_male=0
combined_df_Brooklyn$work_24_pooled_male=0
combined_df_Brooklyn$jail_white=0
combined_df_Brooklyn$jail_pooled=0
combined_df_Brooklyn$jail_white_male=0
combined_df_Brooklyn$jail_pooled_male=0
combined_df_Brooklyn$foreign_born_share_2010=0
combined_df_Brooklyn$employment_2000=0
combined_df_Brooklyn$college_rate_2000=0
combined_df_Brooklyn$college_rate_2010=0
combined_df_Brooklyn$black_2000=0
combined_df_Brooklyn$black_2010=0
combined_df_Brooklyn$white_2000=0
combined_df_Brooklyn$white_2010=0
combined_df_Brooklyn$median_income_1990=0
combined_df_Brooklyn$median_income_2016=0
combined_df_Brooklyn$poor_1990=0
combined_df_Brooklyn$poor_2000=0
combined_df_Brooklyn$poor_2010=0
combined_df_Brooklyn$rent_2015=0
combined_df_Brooklyn$single_parent_share_1990=0
combined_df_Brooklyn$single_parent_share_2000=0
combined_df_Brooklyn$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Brooklyn)){
  tmp=Brooklyn_intersects[which(Brooklyn_intersects$city_hex==combined_df_Brooklyn$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Brooklyn$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Brooklyn$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Brooklyn$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Brooklyn$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Brooklyn$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Brooklyn$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Brooklyn$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Brooklyn$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Brooklyn$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Brooklyn$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Brooklyn$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Brooklyn$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Brooklyn$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Brooklyn$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Brooklyn$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Brooklyn$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Brooklyn$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Brooklyn$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Brooklyn$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Brooklyn$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Brooklyn$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Brooklyn$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Brooklyn$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Brooklyn$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Brooklyn$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Brooklyn$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Brooklyn$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Brooklyn$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Brooklyn$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Brooklyn$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Brooklyn$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Brooklyn$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Brooklyn$stay_tract_white[i]=0
    combined_df_Brooklyn$stay_tract_pooled[i]=0
    combined_df_Brooklyn$stay_tract_white_male[i]=0
    combined_df_Brooklyn$stay_tract_pooled_male[i]=0
    combined_df_Brooklyn$stay_tract_white_female[i]=0
    combined_df_Brooklyn$stay_tract_pooled_female[i]=0
    combined_df_Brooklyn$two_parent_white[i]=0
    combined_df_Brooklyn$two_parent_pooled[i]=0
    combined_df_Brooklyn$work_32_white[i]=0
    combined_df_Brooklyn$work_24_white[i]=0
    combined_df_Brooklyn$work_32_pooled[i]=0
    combined_df_Brooklyn$work_24_pooled[i]=0
    combined_df_Brooklyn$work_32_white_male[i]=0
    combined_df_Brooklyn$work_24_white_male[i]=0
    combined_df_Brooklyn$work_32_pooled_male[i]=0
    combined_df_Brooklyn$work_24_pooled_male[i]=0
    combined_df_Brooklyn$jail_white[i]=0
    combined_df_Brooklyn$jail_pooled[i]=0
    combined_df_Brooklyn$jail_white_male[i]=0
    combined_df_Brooklyn$jail_pooled_male[i]=0
    combined_df_Brooklyn$foreign_born_share_2010[i]=0
    combined_df_Brooklyn$employment_2000[i]=0
    combined_df_Brooklyn$college_rate_2000[i]=0
    combined_df_Brooklyn$college_rate_2010[i]=0
    combined_df_Brooklyn$black_2000[i]=0
    combined_df_Brooklyn$black_2010[i]=0
    combined_df_Brooklyn$white_2000[i]=0
    combined_df_Brooklyn$white_2010[i]=0
    combined_df_Brooklyn$median_income_1990[i]=0
    combined_df_Brooklyn$median_income_2016[i]=0
    combined_df_Brooklyn$poor_1990[i]=0
    combined_df_Brooklyn$poor_2000[i]=0
    combined_df_Brooklyn$poor_2010[i]=0
    combined_df_Brooklyn$rent_2015[i]=0
    combined_df_Brooklyn$single_parent_share_1990[i]=0
    combined_df_Brooklyn$single_parent_share_2000[i]=0
    combined_df_Brooklyn$single_parent_share_2010[i]=0
  }
}

#Boston
Boston_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="25025"),]
Boston_tracts=merge(Boston_tracts, oi_tract_covariates, by="GEOID")
Boston_tracts=merge(Boston_tracts, oi_tract_early, by="GEOID")
Boston_tracts=st_transform(Boston_tracts, st_crs(Boston_hexgrid))
Boston_tracts=st_make_valid(Boston_tracts)
Boston_tracts$tract_area=as.numeric(st_area(Boston_tracts))
Boston_hexgrid=st_make_valid(Boston_hexgrid)
Boston_hexgrid$hex_area=as.numeric(st_area(Boston_hexgrid))
Boston_intersects=st_intersection(Boston_hexgrid, Boston_tracts)
Boston_intersects$area_intersected=as.numeric(st_area(Boston_intersects))
Boston_tracts=st_drop_geometry(Boston_tracts)
Boston_intersects$city_hex=paste0("Boston","_",Boston_intersects$hex_id)
Boston_intersects$prop_hex_intersected=Boston_intersects$area_intersected/Boston_intersects$hex_area
Boston_intersects$prop_tract_intersected=Boston_intersects$area_intersected/Boston_intersects$tract_area
#need to create weighted averages
combined_df_Boston=combined_df[which(combined_df$city=="Boston"),]
combined_df_Boston$stay_tract_white=0
combined_df_Boston$stay_tract_pooled=0
combined_df_Boston$stay_tract_white_male=0
combined_df_Boston$stay_tract_pooled_male=0
combined_df_Boston$stay_tract_white_female=0
combined_df_Boston$stay_tract_pooled_female=0
combined_df_Boston$two_parent_white=0
combined_df_Boston$two_parent_pooled=0
combined_df_Boston$work_32_white=0
combined_df_Boston$work_24_white=0
combined_df_Boston$work_32_pooled=0
combined_df_Boston$work_24_pooled=0
combined_df_Boston$work_32_white_male=0
combined_df_Boston$work_24_white_male=0
combined_df_Boston$work_32_pooled_male=0
combined_df_Boston$work_24_pooled_male=0
combined_df_Boston$jail_white=0
combined_df_Boston$jail_pooled=0
combined_df_Boston$jail_white_male=0
combined_df_Boston$jail_pooled_male=0
combined_df_Boston$foreign_born_share_2010=0
combined_df_Boston$employment_2000=0
combined_df_Boston$college_rate_2000=0
combined_df_Boston$college_rate_2010=0
combined_df_Boston$black_2000=0
combined_df_Boston$black_2010=0
combined_df_Boston$white_2000=0
combined_df_Boston$white_2010=0
combined_df_Boston$median_income_1990=0
combined_df_Boston$median_income_2016=0
combined_df_Boston$poor_1990=0
combined_df_Boston$poor_2000=0
combined_df_Boston$poor_2010=0
combined_df_Boston$rent_2015=0
combined_df_Boston$single_parent_share_1990=0
combined_df_Boston$single_parent_share_2000=0
combined_df_Boston$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Boston)){
  tmp=Boston_intersects[which(Boston_intersects$city_hex==combined_df_Boston$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Boston$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Boston$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Boston$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Boston$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Boston$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Boston$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Boston$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Boston$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Boston$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Boston$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Boston$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Boston$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Boston$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Boston$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Boston$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Boston$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Boston$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Boston$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Boston$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Boston$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Boston$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Boston$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Boston$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Boston$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Boston$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Boston$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Boston$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Boston$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Boston$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Boston$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Boston$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Boston$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Boston$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Boston$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Boston$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Boston$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Boston$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Boston$stay_tract_white[i]=0
    combined_df_Boston$stay_tract_pooled[i]=0
    combined_df_Boston$stay_tract_white_male[i]=0
    combined_df_Boston$stay_tract_pooled_male[i]=0
    combined_df_Boston$stay_tract_white_female[i]=0
    combined_df_Boston$stay_tract_pooled_female[i]=0
    combined_df_Boston$two_parent_white[i]=0
    combined_df_Boston$two_parent_pooled[i]=0
    combined_df_Boston$work_32_white[i]=0
    combined_df_Boston$work_24_white[i]=0
    combined_df_Boston$work_32_pooled[i]=0
    combined_df_Boston$work_24_pooled[i]=0
    combined_df_Boston$work_32_white_male[i]=0
    combined_df_Boston$work_24_white_male[i]=0
    combined_df_Boston$work_32_pooled_male[i]=0
    combined_df_Boston$work_24_pooled_male[i]=0
    combined_df_Boston$jail_white[i]=0
    combined_df_Boston$jail_pooled[i]=0
    combined_df_Boston$jail_white_male[i]=0
    combined_df_Boston$jail_pooled_male[i]=0
    combined_df_Boston$foreign_born_share_2010[i]=0
    combined_df_Boston$employment_2000[i]=0
    combined_df_Boston$college_rate_2000[i]=0
    combined_df_Boston$college_rate_2010[i]=0
    combined_df_Boston$black_2000[i]=0
    combined_df_Boston$black_2010[i]=0
    combined_df_Boston$white_2000[i]=0
    combined_df_Boston$white_2010[i]=0
    combined_df_Boston$median_income_1990[i]=0
    combined_df_Boston$median_income_2016[i]=0
    combined_df_Boston$poor_1990[i]=0
    combined_df_Boston$poor_2000[i]=0
    combined_df_Boston$poor_2010[i]=0
    combined_df_Boston$rent_2015[i]=0
    combined_df_Boston$single_parent_share_1990[i]=0
    combined_df_Boston$single_parent_share_2000[i]=0
    combined_df_Boston$single_parent_share_2010[i]=0
  }
}

#Cleveland
Cleveland_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="39035"),]
Cleveland_tracts=merge(Cleveland_tracts, oi_tract_covariates, by="GEOID")
Cleveland_tracts=merge(Cleveland_tracts, oi_tract_early, by="GEOID")
Cleveland_tracts=st_transform(Cleveland_tracts, st_crs(Cleveland_hexgrid))
Cleveland_tracts=st_make_valid(Cleveland_tracts)
Cleveland_tracts$tract_area=as.numeric(st_area(Cleveland_tracts))
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
Cleveland_hexgrid$hex_area=as.numeric(st_area(Cleveland_hexgrid))
Cleveland_intersects=st_intersection(Cleveland_hexgrid, Cleveland_tracts)
Cleveland_intersects$area_intersected=as.numeric(st_area(Cleveland_intersects))
Cleveland_tracts=st_drop_geometry(Cleveland_tracts)
Cleveland_intersects$city_hex=paste0("Cleveland","_",Cleveland_intersects$hex_id)
Cleveland_intersects$prop_hex_intersected=Cleveland_intersects$area_intersected/Cleveland_intersects$hex_area
Cleveland_intersects$prop_tract_intersected=Cleveland_intersects$area_intersected/Cleveland_intersects$tract_area
#need to create weighted averages
combined_df_Cleveland=combined_df[which(combined_df$city=="Cleveland"),]
combined_df_Cleveland$stay_tract_white=0
combined_df_Cleveland$stay_tract_pooled=0
combined_df_Cleveland$stay_tract_white_male=0
combined_df_Cleveland$stay_tract_pooled_male=0
combined_df_Cleveland$stay_tract_white_female=0
combined_df_Cleveland$stay_tract_pooled_female=0
combined_df_Cleveland$two_parent_white=0
combined_df_Cleveland$two_parent_pooled=0
combined_df_Cleveland$work_32_white=0
combined_df_Cleveland$work_24_white=0
combined_df_Cleveland$work_32_pooled=0
combined_df_Cleveland$work_24_pooled=0
combined_df_Cleveland$work_32_white_male=0
combined_df_Cleveland$work_24_white_male=0
combined_df_Cleveland$work_32_pooled_male=0
combined_df_Cleveland$work_24_pooled_male=0
combined_df_Cleveland$jail_white=0
combined_df_Cleveland$jail_pooled=0
combined_df_Cleveland$jail_white_male=0
combined_df_Cleveland$jail_pooled_male=0
combined_df_Cleveland$foreign_born_share_2010=0
combined_df_Cleveland$employment_2000=0
combined_df_Cleveland$college_rate_2000=0
combined_df_Cleveland$college_rate_2010=0
combined_df_Cleveland$black_2000=0
combined_df_Cleveland$black_2010=0
combined_df_Cleveland$white_2000=0
combined_df_Cleveland$white_2010=0
combined_df_Cleveland$median_income_1990=0
combined_df_Cleveland$median_income_2016=0
combined_df_Cleveland$poor_1990=0
combined_df_Cleveland$poor_2000=0
combined_df_Cleveland$poor_2010=0
combined_df_Cleveland$rent_2015=0
combined_df_Cleveland$single_parent_share_1990=0
combined_df_Cleveland$single_parent_share_2000=0
combined_df_Cleveland$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Cleveland)){
  tmp=Cleveland_intersects[which(Cleveland_intersects$city_hex==combined_df_Cleveland$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Cleveland$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Cleveland$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Cleveland$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Cleveland$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Cleveland$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Cleveland$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Cleveland$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Cleveland$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Cleveland$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Cleveland$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Cleveland$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Cleveland$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Cleveland$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Cleveland$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Cleveland$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Cleveland$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Cleveland$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Cleveland$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Cleveland$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Cleveland$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Cleveland$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Cleveland$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Cleveland$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Cleveland$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Cleveland$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Cleveland$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Cleveland$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Cleveland$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Cleveland$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Cleveland$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Cleveland$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Cleveland$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Cleveland$stay_tract_white[i]=0
    combined_df_Cleveland$stay_tract_pooled[i]=0
    combined_df_Cleveland$stay_tract_white_male[i]=0
    combined_df_Cleveland$stay_tract_pooled_male[i]=0
    combined_df_Cleveland$stay_tract_white_female[i]=0
    combined_df_Cleveland$stay_tract_pooled_female[i]=0
    combined_df_Cleveland$two_parent_white[i]=0
    combined_df_Cleveland$two_parent_pooled[i]=0
    combined_df_Cleveland$work_32_white[i]=0
    combined_df_Cleveland$work_24_white[i]=0
    combined_df_Cleveland$work_32_pooled[i]=0
    combined_df_Cleveland$work_24_pooled[i]=0
    combined_df_Cleveland$work_32_white_male[i]=0
    combined_df_Cleveland$work_24_white_male[i]=0
    combined_df_Cleveland$work_32_pooled_male[i]=0
    combined_df_Cleveland$work_24_pooled_male[i]=0
    combined_df_Cleveland$jail_white[i]=0
    combined_df_Cleveland$jail_pooled[i]=0
    combined_df_Cleveland$jail_white_male[i]=0
    combined_df_Cleveland$jail_pooled_male[i]=0
    combined_df_Cleveland$foreign_born_share_2010[i]=0
    combined_df_Cleveland$employment_2000[i]=0
    combined_df_Cleveland$college_rate_2000[i]=0
    combined_df_Cleveland$college_rate_2010[i]=0
    combined_df_Cleveland$black_2000[i]=0
    combined_df_Cleveland$black_2010[i]=0
    combined_df_Cleveland$white_2000[i]=0
    combined_df_Cleveland$white_2010[i]=0
    combined_df_Cleveland$median_income_1990[i]=0
    combined_df_Cleveland$median_income_2016[i]=0
    combined_df_Cleveland$poor_1990[i]=0
    combined_df_Cleveland$poor_2000[i]=0
    combined_df_Cleveland$poor_2010[i]=0
    combined_df_Cleveland$rent_2015[i]=0
    combined_df_Cleveland$single_parent_share_1990[i]=0
    combined_df_Cleveland$single_parent_share_2000[i]=0
    combined_df_Cleveland$single_parent_share_2010[i]=0
  }
}

#Chicago
Chicago_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="17031"),]
Chicago_tracts=merge(Chicago_tracts, oi_tract_covariates, by="GEOID")
Chicago_tracts=merge(Chicago_tracts, oi_tract_early, by="GEOID")
Chicago_tracts=st_transform(Chicago_tracts, st_crs(Chicago_hexgrid))
Chicago_tracts=st_make_valid(Chicago_tracts)
Chicago_tracts$tract_area=as.numeric(st_area(Chicago_tracts))
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
Chicago_hexgrid$hex_area=as.numeric(st_area(Chicago_hexgrid))
Chicago_intersects=st_intersection(Chicago_hexgrid, Chicago_tracts)
Chicago_intersects$area_intersected=as.numeric(st_area(Chicago_intersects))
Chicago_tracts=st_drop_geometry(Chicago_tracts)
Chicago_intersects$city_hex=paste0("Chicago","_",Chicago_intersects$hex_id)
Chicago_intersects$prop_hex_intersected=Chicago_intersects$area_intersected/Chicago_intersects$hex_area
Chicago_intersects$prop_tract_intersected=Chicago_intersects$area_intersected/Chicago_intersects$tract_area
#need to create weighted averages
combined_df_Chicago=combined_df[which(combined_df$city=="Chicago"),]
combined_df_Chicago$stay_tract_white=0
combined_df_Chicago$stay_tract_pooled=0
combined_df_Chicago$stay_tract_white_male=0
combined_df_Chicago$stay_tract_pooled_male=0
combined_df_Chicago$stay_tract_white_female=0
combined_df_Chicago$stay_tract_pooled_female=0
combined_df_Chicago$two_parent_white=0
combined_df_Chicago$two_parent_pooled=0
combined_df_Chicago$work_32_white=0
combined_df_Chicago$work_24_white=0
combined_df_Chicago$work_32_pooled=0
combined_df_Chicago$work_24_pooled=0
combined_df_Chicago$work_32_white_male=0
combined_df_Chicago$work_24_white_male=0
combined_df_Chicago$work_32_pooled_male=0
combined_df_Chicago$work_24_pooled_male=0
combined_df_Chicago$jail_white=0
combined_df_Chicago$jail_pooled=0
combined_df_Chicago$jail_white_male=0
combined_df_Chicago$jail_pooled_male=0
combined_df_Chicago$foreign_born_share_2010=0
combined_df_Chicago$employment_2000=0
combined_df_Chicago$college_rate_2000=0
combined_df_Chicago$college_rate_2010=0
combined_df_Chicago$black_2000=0
combined_df_Chicago$black_2010=0
combined_df_Chicago$white_2000=0
combined_df_Chicago$white_2010=0
combined_df_Chicago$median_income_1990=0
combined_df_Chicago$median_income_2016=0
combined_df_Chicago$poor_1990=0
combined_df_Chicago$poor_2000=0
combined_df_Chicago$poor_2010=0
combined_df_Chicago$rent_2015=0
combined_df_Chicago$single_parent_share_1990=0
combined_df_Chicago$single_parent_share_2000=0
combined_df_Chicago$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Chicago)){
  tmp=Chicago_intersects[which(Chicago_intersects$city_hex==combined_df_Chicago$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Chicago$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Chicago$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Chicago$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Chicago$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Chicago$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Chicago$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Chicago$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Chicago$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Chicago$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Chicago$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Chicago$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Chicago$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Chicago$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Chicago$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Chicago$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Chicago$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Chicago$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Chicago$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Chicago$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Chicago$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Chicago$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Chicago$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Chicago$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Chicago$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Chicago$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Chicago$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Chicago$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Chicago$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Chicago$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Chicago$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Chicago$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Chicago$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Chicago$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Chicago$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Chicago$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Chicago$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Chicago$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Chicago$stay_tract_white[i]=0
    combined_df_Chicago$stay_tract_pooled[i]=0
    combined_df_Chicago$stay_tract_white_male[i]=0
    combined_df_Chicago$stay_tract_pooled_male[i]=0
    combined_df_Chicago$stay_tract_white_female[i]=0
    combined_df_Chicago$stay_tract_pooled_female[i]=0
    combined_df_Chicago$two_parent_white[i]=0
    combined_df_Chicago$two_parent_pooled[i]=0
    combined_df_Chicago$work_32_white[i]=0
    combined_df_Chicago$work_24_white[i]=0
    combined_df_Chicago$work_32_pooled[i]=0
    combined_df_Chicago$work_24_pooled[i]=0
    combined_df_Chicago$work_32_white_male[i]=0
    combined_df_Chicago$work_24_white_male[i]=0
    combined_df_Chicago$work_32_pooled_male[i]=0
    combined_df_Chicago$work_24_pooled_male[i]=0
    combined_df_Chicago$jail_white[i]=0
    combined_df_Chicago$jail_pooled[i]=0
    combined_df_Chicago$jail_white_male[i]=0
    combined_df_Chicago$jail_pooled_male[i]=0
    combined_df_Chicago$foreign_born_share_2010[i]=0
    combined_df_Chicago$employment_2000[i]=0
    combined_df_Chicago$college_rate_2000[i]=0
    combined_df_Chicago$college_rate_2010[i]=0
    combined_df_Chicago$black_2000[i]=0
    combined_df_Chicago$black_2010[i]=0
    combined_df_Chicago$white_2000[i]=0
    combined_df_Chicago$white_2010[i]=0
    combined_df_Chicago$median_income_1990[i]=0
    combined_df_Chicago$median_income_2016[i]=0
    combined_df_Chicago$poor_1990[i]=0
    combined_df_Chicago$poor_2000[i]=0
    combined_df_Chicago$poor_2010[i]=0
    combined_df_Chicago$rent_2015[i]=0
    combined_df_Chicago$single_parent_share_1990[i]=0
    combined_df_Chicago$single_parent_share_2000[i]=0
    combined_df_Chicago$single_parent_share_2010[i]=0
  }
}

#Detroit
Detroit_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="26163"),]
Detroit_tracts=merge(Detroit_tracts, oi_tract_covariates, by="GEOID")
Detroit_tracts=merge(Detroit_tracts, oi_tract_early, by="GEOID")
Detroit_tracts=st_transform(Detroit_tracts, st_crs(Detroit_hexgrid))
Detroit_tracts=st_make_valid(Detroit_tracts)
Detroit_tracts$tract_area=as.numeric(st_area(Detroit_tracts))
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
Detroit_hexgrid$hex_area=as.numeric(st_area(Detroit_hexgrid))
Detroit_intersects=st_intersection(Detroit_hexgrid, Detroit_tracts)
Detroit_intersects$area_intersected=as.numeric(st_area(Detroit_intersects))
Detroit_tracts=st_drop_geometry(Detroit_tracts)
Detroit_intersects$city_hex=paste0("Detroit","_",Detroit_intersects$hex_id)
Detroit_intersects$prop_hex_intersected=Detroit_intersects$area_intersected/Detroit_intersects$hex_area
Detroit_intersects$prop_tract_intersected=Detroit_intersects$area_intersected/Detroit_intersects$tract_area
#need to create weighted averages
combined_df_Detroit=combined_df[which(combined_df$city=="Detroit"),]
combined_df_Detroit$stay_tract_white=0
combined_df_Detroit$stay_tract_pooled=0
combined_df_Detroit$stay_tract_white_male=0
combined_df_Detroit$stay_tract_pooled_male=0
combined_df_Detroit$stay_tract_white_female=0
combined_df_Detroit$stay_tract_pooled_female=0
combined_df_Detroit$two_parent_white=0
combined_df_Detroit$two_parent_pooled=0
combined_df_Detroit$work_32_white=0
combined_df_Detroit$work_24_white=0
combined_df_Detroit$work_32_pooled=0
combined_df_Detroit$work_24_pooled=0
combined_df_Detroit$work_32_white_male=0
combined_df_Detroit$work_24_white_male=0
combined_df_Detroit$work_32_pooled_male=0
combined_df_Detroit$work_24_pooled_male=0
combined_df_Detroit$jail_white=0
combined_df_Detroit$jail_pooled=0
combined_df_Detroit$jail_white_male=0
combined_df_Detroit$jail_pooled_male=0
combined_df_Detroit$foreign_born_share_2010=0
combined_df_Detroit$employment_2000=0
combined_df_Detroit$college_rate_2000=0
combined_df_Detroit$college_rate_2010=0
combined_df_Detroit$black_2000=0
combined_df_Detroit$black_2010=0
combined_df_Detroit$white_2000=0
combined_df_Detroit$white_2010=0
combined_df_Detroit$median_income_1990=0
combined_df_Detroit$median_income_2016=0
combined_df_Detroit$poor_1990=0
combined_df_Detroit$poor_2000=0
combined_df_Detroit$poor_2010=0
combined_df_Detroit$rent_2015=0
combined_df_Detroit$single_parent_share_1990=0
combined_df_Detroit$single_parent_share_2000=0
combined_df_Detroit$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Detroit)){
  tmp=Detroit_intersects[which(Detroit_intersects$city_hex==combined_df_Detroit$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Detroit$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Detroit$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Detroit$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Detroit$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Detroit$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Detroit$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Detroit$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Detroit$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Detroit$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Detroit$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Detroit$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Detroit$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Detroit$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Detroit$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Detroit$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Detroit$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Detroit$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Detroit$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Detroit$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Detroit$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Detroit$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Detroit$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Detroit$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Detroit$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Detroit$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Detroit$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Detroit$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Detroit$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Detroit$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Detroit$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Detroit$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Detroit$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Detroit$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Detroit$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Detroit$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Detroit$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Detroit$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Detroit$stay_tract_white[i]=0
    combined_df_Detroit$stay_tract_pooled[i]=0
    combined_df_Detroit$stay_tract_white_male[i]=0
    combined_df_Detroit$stay_tract_pooled_male[i]=0
    combined_df_Detroit$stay_tract_white_female[i]=0
    combined_df_Detroit$stay_tract_pooled_female[i]=0
    combined_df_Detroit$two_parent_white[i]=0
    combined_df_Detroit$two_parent_pooled[i]=0
    combined_df_Detroit$work_32_white[i]=0
    combined_df_Detroit$work_24_white[i]=0
    combined_df_Detroit$work_32_pooled[i]=0
    combined_df_Detroit$work_24_pooled[i]=0
    combined_df_Detroit$work_32_white_male[i]=0
    combined_df_Detroit$work_24_white_male[i]=0
    combined_df_Detroit$work_32_pooled_male[i]=0
    combined_df_Detroit$work_24_pooled_male[i]=0
    combined_df_Detroit$jail_white[i]=0
    combined_df_Detroit$jail_pooled[i]=0
    combined_df_Detroit$jail_white_male[i]=0
    combined_df_Detroit$jail_pooled_male[i]=0
    combined_df_Detroit$foreign_born_share_2010[i]=0
    combined_df_Detroit$employment_2000[i]=0
    combined_df_Detroit$college_rate_2000[i]=0
    combined_df_Detroit$college_rate_2010[i]=0
    combined_df_Detroit$black_2000[i]=0
    combined_df_Detroit$black_2010[i]=0
    combined_df_Detroit$white_2000[i]=0
    combined_df_Detroit$white_2010[i]=0
    combined_df_Detroit$median_income_1990[i]=0
    combined_df_Detroit$median_income_2016[i]=0
    combined_df_Detroit$poor_1990[i]=0
    combined_df_Detroit$poor_2000[i]=0
    combined_df_Detroit$poor_2010[i]=0
    combined_df_Detroit$rent_2015[i]=0
    combined_df_Detroit$single_parent_share_1990[i]=0
    combined_df_Detroit$single_parent_share_2000[i]=0
    combined_df_Detroit$single_parent_share_2010[i]=0
  }
}

#Manhattan
Manhattan_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="36061"),]
Manhattan_tracts=merge(Manhattan_tracts, oi_tract_covariates, by="GEOID")
Manhattan_tracts=merge(Manhattan_tracts, oi_tract_early, by="GEOID")
Manhattan_tracts=st_transform(Manhattan_tracts, st_crs(Manhattan_hexgrid))
Manhattan_tracts=st_make_valid(Manhattan_tracts)
Manhattan_tracts$tract_area=as.numeric(st_area(Manhattan_tracts))
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
Manhattan_hexgrid$hex_area=as.numeric(st_area(Manhattan_hexgrid))
Manhattan_intersects=st_intersection(Manhattan_hexgrid, Manhattan_tracts)
Manhattan_intersects$area_intersected=as.numeric(st_area(Manhattan_intersects))
Manhattan_tracts=st_drop_geometry(Manhattan_tracts)
Manhattan_intersects$city_hex=paste0("Manhattan","_",Manhattan_intersects$hex_id)
Manhattan_intersects$prop_hex_intersected=Manhattan_intersects$area_intersected/Manhattan_intersects$hex_area
Manhattan_intersects$prop_tract_intersected=Manhattan_intersects$area_intersected/Manhattan_intersects$tract_area
#need to create weighted averages
combined_df_Manhattan=combined_df[which(combined_df$city=="Manhattan"),]
combined_df_Manhattan$stay_tract_white=0
combined_df_Manhattan$stay_tract_pooled=0
combined_df_Manhattan$stay_tract_white_male=0
combined_df_Manhattan$stay_tract_pooled_male=0
combined_df_Manhattan$stay_tract_white_female=0
combined_df_Manhattan$stay_tract_pooled_female=0
combined_df_Manhattan$two_parent_white=0
combined_df_Manhattan$two_parent_pooled=0
combined_df_Manhattan$work_32_white=0
combined_df_Manhattan$work_24_white=0
combined_df_Manhattan$work_32_pooled=0
combined_df_Manhattan$work_24_pooled=0
combined_df_Manhattan$work_32_white_male=0
combined_df_Manhattan$work_24_white_male=0
combined_df_Manhattan$work_32_pooled_male=0
combined_df_Manhattan$work_24_pooled_male=0
combined_df_Manhattan$jail_white=0
combined_df_Manhattan$jail_pooled=0
combined_df_Manhattan$jail_white_male=0
combined_df_Manhattan$jail_pooled_male=0
combined_df_Manhattan$foreign_born_share_2010=0
combined_df_Manhattan$employment_2000=0
combined_df_Manhattan$college_rate_2000=0
combined_df_Manhattan$college_rate_2010=0
combined_df_Manhattan$black_2000=0
combined_df_Manhattan$black_2010=0
combined_df_Manhattan$white_2000=0
combined_df_Manhattan$white_2010=0
combined_df_Manhattan$median_income_1990=0
combined_df_Manhattan$median_income_2016=0
combined_df_Manhattan$poor_1990=0
combined_df_Manhattan$poor_2000=0
combined_df_Manhattan$poor_2010=0
combined_df_Manhattan$rent_2015=0
combined_df_Manhattan$single_parent_share_1990=0
combined_df_Manhattan$single_parent_share_2000=0
combined_df_Manhattan$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Manhattan)){
  tmp=Manhattan_intersects[which(Manhattan_intersects$city_hex==combined_df_Manhattan$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Manhattan$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Manhattan$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Manhattan$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Manhattan$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Manhattan$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Manhattan$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Manhattan$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Manhattan$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Manhattan$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Manhattan$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Manhattan$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Manhattan$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Manhattan$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Manhattan$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Manhattan$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Manhattan$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Manhattan$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Manhattan$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Manhattan$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Manhattan$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Manhattan$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Manhattan$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Manhattan$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Manhattan$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Manhattan$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Manhattan$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Manhattan$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Manhattan$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Manhattan$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Manhattan$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Manhattan$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Manhattan$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Manhattan$stay_tract_white[i]=0
    combined_df_Manhattan$stay_tract_pooled[i]=0
    combined_df_Manhattan$stay_tract_white_male[i]=0
    combined_df_Manhattan$stay_tract_pooled_male[i]=0
    combined_df_Manhattan$stay_tract_white_female[i]=0
    combined_df_Manhattan$stay_tract_pooled_female[i]=0
    combined_df_Manhattan$two_parent_white[i]=0
    combined_df_Manhattan$two_parent_pooled[i]=0
    combined_df_Manhattan$work_32_white[i]=0
    combined_df_Manhattan$work_24_white[i]=0
    combined_df_Manhattan$work_32_pooled[i]=0
    combined_df_Manhattan$work_24_pooled[i]=0
    combined_df_Manhattan$work_32_white_male[i]=0
    combined_df_Manhattan$work_24_white_male[i]=0
    combined_df_Manhattan$work_32_pooled_male[i]=0
    combined_df_Manhattan$work_24_pooled_male[i]=0
    combined_df_Manhattan$jail_white[i]=0
    combined_df_Manhattan$jail_pooled[i]=0
    combined_df_Manhattan$jail_white_male[i]=0
    combined_df_Manhattan$jail_pooled_male[i]=0
    combined_df_Manhattan$foreign_born_share_2010[i]=0
    combined_df_Manhattan$employment_2000[i]=0
    combined_df_Manhattan$college_rate_2000[i]=0
    combined_df_Manhattan$college_rate_2010[i]=0
    combined_df_Manhattan$black_2000[i]=0
    combined_df_Manhattan$black_2010[i]=0
    combined_df_Manhattan$white_2000[i]=0
    combined_df_Manhattan$white_2010[i]=0
    combined_df_Manhattan$median_income_1990[i]=0
    combined_df_Manhattan$median_income_2016[i]=0
    combined_df_Manhattan$poor_1990[i]=0
    combined_df_Manhattan$poor_2000[i]=0
    combined_df_Manhattan$poor_2010[i]=0
    combined_df_Manhattan$rent_2015[i]=0
    combined_df_Manhattan$single_parent_share_1990[i]=0
    combined_df_Manhattan$single_parent_share_2000[i]=0
    combined_df_Manhattan$single_parent_share_2010[i]=0
  }
}

#Philadelphia
Philadelphia_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="42101"),]
Philadelphia_tracts=merge(Philadelphia_tracts, oi_tract_covariates, by="GEOID")
Philadelphia_tracts=merge(Philadelphia_tracts, oi_tract_early, by="GEOID")
Philadelphia_tracts=st_transform(Philadelphia_tracts, st_crs(Philadelphia_hexgrid))
Philadelphia_tracts=st_make_valid(Philadelphia_tracts)
Philadelphia_tracts$tract_area=as.numeric(st_area(Philadelphia_tracts))
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
Philadelphia_hexgrid$hex_area=as.numeric(st_area(Philadelphia_hexgrid))
Philadelphia_intersects=st_intersection(Philadelphia_hexgrid, Philadelphia_tracts)
Philadelphia_intersects$area_intersected=as.numeric(st_area(Philadelphia_intersects))
Philadelphia_tracts=st_drop_geometry(Philadelphia_tracts)
Philadelphia_intersects$city_hex=paste0("Philadelphia","_",Philadelphia_intersects$hex_id)
Philadelphia_intersects$prop_hex_intersected=Philadelphia_intersects$area_intersected/Philadelphia_intersects$hex_area
Philadelphia_intersects$prop_tract_intersected=Philadelphia_intersects$area_intersected/Philadelphia_intersects$tract_area
#need to create weighted averages
combined_df_Philadelphia=combined_df[which(combined_df$city=="Philadelphia"),]
combined_df_Philadelphia$stay_tract_white=0
combined_df_Philadelphia$stay_tract_pooled=0
combined_df_Philadelphia$stay_tract_white_male=0
combined_df_Philadelphia$stay_tract_pooled_male=0
combined_df_Philadelphia$stay_tract_white_female=0
combined_df_Philadelphia$stay_tract_pooled_female=0
combined_df_Philadelphia$two_parent_white=0
combined_df_Philadelphia$two_parent_pooled=0
combined_df_Philadelphia$work_32_white=0
combined_df_Philadelphia$work_24_white=0
combined_df_Philadelphia$work_32_pooled=0
combined_df_Philadelphia$work_24_pooled=0
combined_df_Philadelphia$work_32_white_male=0
combined_df_Philadelphia$work_24_white_male=0
combined_df_Philadelphia$work_32_pooled_male=0
combined_df_Philadelphia$work_24_pooled_male=0
combined_df_Philadelphia$jail_white=0
combined_df_Philadelphia$jail_pooled=0
combined_df_Philadelphia$jail_white_male=0
combined_df_Philadelphia$jail_pooled_male=0
combined_df_Philadelphia$foreign_born_share_2010=0
combined_df_Philadelphia$employment_2000=0
combined_df_Philadelphia$college_rate_2000=0
combined_df_Philadelphia$college_rate_2010=0
combined_df_Philadelphia$black_2000=0
combined_df_Philadelphia$black_2010=0
combined_df_Philadelphia$white_2000=0
combined_df_Philadelphia$white_2010=0
combined_df_Philadelphia$median_income_1990=0
combined_df_Philadelphia$median_income_2016=0
combined_df_Philadelphia$poor_1990=0
combined_df_Philadelphia$poor_2000=0
combined_df_Philadelphia$poor_2010=0
combined_df_Philadelphia$rent_2015=0
combined_df_Philadelphia$single_parent_share_1990=0
combined_df_Philadelphia$single_parent_share_2000=0
combined_df_Philadelphia$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Philadelphia)){
  tmp=Philadelphia_intersects[which(Philadelphia_intersects$city_hex==combined_df_Philadelphia$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Philadelphia$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Philadelphia$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Philadelphia$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Philadelphia$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Philadelphia$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Philadelphia$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Philadelphia$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Philadelphia$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Philadelphia$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Philadelphia$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Philadelphia$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Philadelphia$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Philadelphia$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Philadelphia$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Philadelphia$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Philadelphia$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Philadelphia$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Philadelphia$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Philadelphia$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Philadelphia$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Philadelphia$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Philadelphia$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Philadelphia$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Philadelphia$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Philadelphia$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Philadelphia$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Philadelphia$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Philadelphia$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Philadelphia$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Philadelphia$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Philadelphia$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Philadelphia$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Philadelphia$stay_tract_white[i]=0
    combined_df_Philadelphia$stay_tract_pooled[i]=0
    combined_df_Philadelphia$stay_tract_white_male[i]=0
    combined_df_Philadelphia$stay_tract_pooled_male[i]=0
    combined_df_Philadelphia$stay_tract_white_female[i]=0
    combined_df_Philadelphia$stay_tract_pooled_female[i]=0
    combined_df_Philadelphia$two_parent_white[i]=0
    combined_df_Philadelphia$two_parent_pooled[i]=0
    combined_df_Philadelphia$work_32_white[i]=0
    combined_df_Philadelphia$work_24_white[i]=0
    combined_df_Philadelphia$work_32_pooled[i]=0
    combined_df_Philadelphia$work_24_pooled[i]=0
    combined_df_Philadelphia$work_32_white_male[i]=0
    combined_df_Philadelphia$work_24_white_male[i]=0
    combined_df_Philadelphia$work_32_pooled_male[i]=0
    combined_df_Philadelphia$work_24_pooled_male[i]=0
    combined_df_Philadelphia$jail_white[i]=0
    combined_df_Philadelphia$jail_pooled[i]=0
    combined_df_Philadelphia$jail_white_male[i]=0
    combined_df_Philadelphia$jail_pooled_male[i]=0
    combined_df_Philadelphia$foreign_born_share_2010[i]=0
    combined_df_Philadelphia$employment_2000[i]=0
    combined_df_Philadelphia$college_rate_2000[i]=0
    combined_df_Philadelphia$college_rate_2010[i]=0
    combined_df_Philadelphia$black_2000[i]=0
    combined_df_Philadelphia$black_2010[i]=0
    combined_df_Philadelphia$white_2000[i]=0
    combined_df_Philadelphia$white_2010[i]=0
    combined_df_Philadelphia$median_income_1990[i]=0
    combined_df_Philadelphia$median_income_2016[i]=0
    combined_df_Philadelphia$poor_1990[i]=0
    combined_df_Philadelphia$poor_2000[i]=0
    combined_df_Philadelphia$poor_2010[i]=0
    combined_df_Philadelphia$rent_2015[i]=0
    combined_df_Philadelphia$single_parent_share_1990[i]=0
    combined_df_Philadelphia$single_parent_share_2000[i]=0
    combined_df_Philadelphia$single_parent_share_2010[i]=0
  }
}

#Pittsburgh
Pittsburgh_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="42003"),]
Pittsburgh_tracts=merge(Pittsburgh_tracts, oi_tract_covariates, by="GEOID")
Pittsburgh_tracts=merge(Pittsburgh_tracts, oi_tract_early, by="GEOID")
Pittsburgh_tracts=st_transform(Pittsburgh_tracts, st_crs(Pittsburgh_hexgrid))
Pittsburgh_tracts=st_make_valid(Pittsburgh_tracts)
Pittsburgh_tracts$tract_area=as.numeric(st_area(Pittsburgh_tracts))
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
Pittsburgh_hexgrid$hex_area=as.numeric(st_area(Pittsburgh_hexgrid))
Pittsburgh_intersects=st_intersection(Pittsburgh_hexgrid, Pittsburgh_tracts)
Pittsburgh_intersects$area_intersected=as.numeric(st_area(Pittsburgh_intersects))
Pittsburgh_tracts=st_drop_geometry(Pittsburgh_tracts)
Pittsburgh_intersects$city_hex=paste0("Pittsburgh","_",Pittsburgh_intersects$hex_id)
Pittsburgh_intersects$prop_hex_intersected=Pittsburgh_intersects$area_intersected/Pittsburgh_intersects$hex_area
Pittsburgh_intersects$prop_tract_intersected=Pittsburgh_intersects$area_intersected/Pittsburgh_intersects$tract_area
#need to create weighted averages
combined_df_Pittsburgh=combined_df[which(combined_df$city=="Pittsburgh"),]
combined_df_Pittsburgh$stay_tract_white=0
combined_df_Pittsburgh$stay_tract_pooled=0
combined_df_Pittsburgh$stay_tract_white_male=0
combined_df_Pittsburgh$stay_tract_pooled_male=0
combined_df_Pittsburgh$stay_tract_white_female=0
combined_df_Pittsburgh$stay_tract_pooled_female=0
combined_df_Pittsburgh$two_parent_white=0
combined_df_Pittsburgh$two_parent_pooled=0
combined_df_Pittsburgh$work_32_white=0
combined_df_Pittsburgh$work_24_white=0
combined_df_Pittsburgh$work_32_pooled=0
combined_df_Pittsburgh$work_24_pooled=0
combined_df_Pittsburgh$work_32_white_male=0
combined_df_Pittsburgh$work_24_white_male=0
combined_df_Pittsburgh$work_32_pooled_male=0
combined_df_Pittsburgh$work_24_pooled_male=0
combined_df_Pittsburgh$jail_white=0
combined_df_Pittsburgh$jail_pooled=0
combined_df_Pittsburgh$jail_white_male=0
combined_df_Pittsburgh$jail_pooled_male=0
combined_df_Pittsburgh$foreign_born_share_2010=0
combined_df_Pittsburgh$employment_2000=0
combined_df_Pittsburgh$college_rate_2000=0
combined_df_Pittsburgh$college_rate_2010=0
combined_df_Pittsburgh$black_2000=0
combined_df_Pittsburgh$black_2010=0
combined_df_Pittsburgh$white_2000=0
combined_df_Pittsburgh$white_2010=0
combined_df_Pittsburgh$median_income_1990=0
combined_df_Pittsburgh$median_income_2016=0
combined_df_Pittsburgh$poor_1990=0
combined_df_Pittsburgh$poor_2000=0
combined_df_Pittsburgh$poor_2010=0
combined_df_Pittsburgh$rent_2015=0
combined_df_Pittsburgh$single_parent_share_1990=0
combined_df_Pittsburgh$single_parent_share_2000=0
combined_df_Pittsburgh$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Pittsburgh)){
  tmp=Pittsburgh_intersects[which(Pittsburgh_intersects$city_hex==combined_df_Pittsburgh$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Pittsburgh$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Pittsburgh$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Pittsburgh$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Pittsburgh$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Pittsburgh$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Pittsburgh$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Pittsburgh$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Pittsburgh$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Pittsburgh$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Pittsburgh$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Pittsburgh$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Pittsburgh$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Pittsburgh$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Pittsburgh$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Pittsburgh$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Pittsburgh$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Pittsburgh$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Pittsburgh$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Pittsburgh$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Pittsburgh$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Pittsburgh$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Pittsburgh$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Pittsburgh$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Pittsburgh$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Pittsburgh$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Pittsburgh$stay_tract_white[i]=0
    combined_df_Pittsburgh$stay_tract_pooled[i]=0
    combined_df_Pittsburgh$stay_tract_white_male[i]=0
    combined_df_Pittsburgh$stay_tract_pooled_male[i]=0
    combined_df_Pittsburgh$stay_tract_white_female[i]=0
    combined_df_Pittsburgh$stay_tract_pooled_female[i]=0
    combined_df_Pittsburgh$two_parent_white[i]=0
    combined_df_Pittsburgh$two_parent_pooled[i]=0
    combined_df_Pittsburgh$work_32_white[i]=0
    combined_df_Pittsburgh$work_24_white[i]=0
    combined_df_Pittsburgh$work_32_pooled[i]=0
    combined_df_Pittsburgh$work_24_pooled[i]=0
    combined_df_Pittsburgh$work_32_white_male[i]=0
    combined_df_Pittsburgh$work_24_white_male[i]=0
    combined_df_Pittsburgh$work_32_pooled_male[i]=0
    combined_df_Pittsburgh$work_24_pooled_male[i]=0
    combined_df_Pittsburgh$jail_white[i]=0
    combined_df_Pittsburgh$jail_pooled[i]=0
    combined_df_Pittsburgh$jail_white_male[i]=0
    combined_df_Pittsburgh$jail_pooled_male[i]=0
    combined_df_Pittsburgh$foreign_born_share_2010[i]=0
    combined_df_Pittsburgh$employment_2000[i]=0
    combined_df_Pittsburgh$college_rate_2000[i]=0
    combined_df_Pittsburgh$college_rate_2010[i]=0
    combined_df_Pittsburgh$black_2000[i]=0
    combined_df_Pittsburgh$black_2010[i]=0
    combined_df_Pittsburgh$white_2000[i]=0
    combined_df_Pittsburgh$white_2010[i]=0
    combined_df_Pittsburgh$median_income_1990[i]=0
    combined_df_Pittsburgh$median_income_2016[i]=0
    combined_df_Pittsburgh$poor_1990[i]=0
    combined_df_Pittsburgh$poor_2000[i]=0
    combined_df_Pittsburgh$poor_2010[i]=0
    combined_df_Pittsburgh$rent_2015[i]=0
    combined_df_Pittsburgh$single_parent_share_1990[i]=0
    combined_df_Pittsburgh$single_parent_share_2000[i]=0
    combined_df_Pittsburgh$single_parent_share_2010[i]=0
  }
}

#StLouis
StLouis_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="29510"),]
StLouis_tracts=merge(StLouis_tracts, oi_tract_covariates, by="GEOID")
StLouis_tracts=merge(StLouis_tracts, oi_tract_early, by="GEOID")
StLouis_tracts=st_transform(StLouis_tracts, st_crs(StLouis_hexgrid))
StLouis_tracts=st_make_valid(StLouis_tracts)
StLouis_tracts$tract_area=as.numeric(st_area(StLouis_tracts))
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
StLouis_hexgrid$hex_area=as.numeric(st_area(StLouis_hexgrid))
StLouis_intersects=st_intersection(StLouis_hexgrid, StLouis_tracts)
StLouis_intersects$area_intersected=as.numeric(st_area(StLouis_intersects))
StLouis_tracts=st_drop_geometry(StLouis_tracts)
StLouis_intersects$city_hex=paste0("StLouis","_",StLouis_intersects$hex_id)
StLouis_intersects$prop_hex_intersected=StLouis_intersects$area_intersected/StLouis_intersects$hex_area
StLouis_intersects$prop_tract_intersected=StLouis_intersects$area_intersected/StLouis_intersects$tract_area
#need to create weighted averages
combined_df_StLouis=combined_df[which(combined_df$city=="StLouis"),]
combined_df_StLouis$stay_tract_white=0
combined_df_StLouis$stay_tract_pooled=0
combined_df_StLouis$stay_tract_white_male=0
combined_df_StLouis$stay_tract_pooled_male=0
combined_df_StLouis$stay_tract_white_female=0
combined_df_StLouis$stay_tract_pooled_female=0
combined_df_StLouis$two_parent_white=0
combined_df_StLouis$two_parent_pooled=0
combined_df_StLouis$work_32_white=0
combined_df_StLouis$work_24_white=0
combined_df_StLouis$work_32_pooled=0
combined_df_StLouis$work_24_pooled=0
combined_df_StLouis$work_32_white_male=0
combined_df_StLouis$work_24_white_male=0
combined_df_StLouis$work_32_pooled_male=0
combined_df_StLouis$work_24_pooled_male=0
combined_df_StLouis$jail_white=0
combined_df_StLouis$jail_pooled=0
combined_df_StLouis$jail_white_male=0
combined_df_StLouis$jail_pooled_male=0
combined_df_StLouis$foreign_born_share_2010=0
combined_df_StLouis$employment_2000=0
combined_df_StLouis$college_rate_2000=0
combined_df_StLouis$college_rate_2010=0
combined_df_StLouis$black_2000=0
combined_df_StLouis$black_2010=0
combined_df_StLouis$white_2000=0
combined_df_StLouis$white_2010=0
combined_df_StLouis$median_income_1990=0
combined_df_StLouis$median_income_2016=0
combined_df_StLouis$poor_1990=0
combined_df_StLouis$poor_2000=0
combined_df_StLouis$poor_2010=0
combined_df_StLouis$rent_2015=0
combined_df_StLouis$single_parent_share_1990=0
combined_df_StLouis$single_parent_share_2000=0
combined_df_StLouis$single_parent_share_2010=0
for (i in 1:nrow(combined_df_StLouis)){
  tmp=StLouis_intersects[which(StLouis_intersects$city_hex==combined_df_StLouis$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_StLouis$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_StLouis$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_StLouis$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_StLouis$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_StLouis$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_StLouis$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_StLouis$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_StLouis$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_StLouis$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_StLouis$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_StLouis$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_StLouis$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_StLouis$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_StLouis$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_StLouis$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_StLouis$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_StLouis$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_StLouis$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_StLouis$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_StLouis$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_StLouis$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_StLouis$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_StLouis$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_StLouis$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_StLouis$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_StLouis$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_StLouis$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_StLouis$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_StLouis$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_StLouis$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_StLouis$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_StLouis$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_StLouis$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_StLouis$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_StLouis$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_StLouis$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_StLouis$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_StLouis$stay_tract_white[i]=0
    combined_df_StLouis$stay_tract_pooled[i]=0
    combined_df_StLouis$stay_tract_white_male[i]=0
    combined_df_StLouis$stay_tract_pooled_male[i]=0
    combined_df_StLouis$stay_tract_white_female[i]=0
    combined_df_StLouis$stay_tract_pooled_female[i]=0
    combined_df_StLouis$two_parent_white[i]=0
    combined_df_StLouis$two_parent_pooled[i]=0
    combined_df_StLouis$work_32_white[i]=0
    combined_df_StLouis$work_24_white[i]=0
    combined_df_StLouis$work_32_pooled[i]=0
    combined_df_StLouis$work_24_pooled[i]=0
    combined_df_StLouis$work_32_white_male[i]=0
    combined_df_StLouis$work_24_white_male[i]=0
    combined_df_StLouis$work_32_pooled_male[i]=0
    combined_df_StLouis$work_24_pooled_male[i]=0
    combined_df_StLouis$jail_white[i]=0
    combined_df_StLouis$jail_pooled[i]=0
    combined_df_StLouis$jail_white_male[i]=0
    combined_df_StLouis$jail_pooled_male[i]=0
    combined_df_StLouis$foreign_born_share_2010[i]=0
    combined_df_StLouis$employment_2000[i]=0
    combined_df_StLouis$college_rate_2000[i]=0
    combined_df_StLouis$college_rate_2010[i]=0
    combined_df_StLouis$black_2000[i]=0
    combined_df_StLouis$black_2010[i]=0
    combined_df_StLouis$white_2000[i]=0
    combined_df_StLouis$white_2010[i]=0
    combined_df_StLouis$median_income_1990[i]=0
    combined_df_StLouis$median_income_2016[i]=0
    combined_df_StLouis$poor_1990[i]=0
    combined_df_StLouis$poor_2000[i]=0
    combined_df_StLouis$poor_2010[i]=0
    combined_df_StLouis$rent_2015[i]=0
    combined_df_StLouis$single_parent_share_1990[i]=0
    combined_df_StLouis$single_parent_share_2000[i]=0
    combined_df_StLouis$single_parent_share_2010[i]=0
  }
}

#Cincinnati
Cincinnati_tracts=tracts[which(substr(tracts$GEOID, 1, 5)=="39061"),]
Cincinnati_tracts=merge(Cincinnati_tracts, oi_tract_covariates, by="GEOID")
Cincinnati_tracts=merge(Cincinnati_tracts, oi_tract_early, by="GEOID")
Cincinnati_tracts=st_transform(Cincinnati_tracts, st_crs(Cincinnati_hexgrid))
Cincinnati_tracts=st_make_valid(Cincinnati_tracts)
Cincinnati_tracts$tract_area=as.numeric(st_area(Cincinnati_tracts))
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
Cincinnati_hexgrid$hex_area=as.numeric(st_area(Cincinnati_hexgrid))
Cincinnati_intersects=st_intersection(Cincinnati_hexgrid, Cincinnati_tracts)
Cincinnati_intersects$area_intersected=as.numeric(st_area(Cincinnati_intersects))
Cincinnati_tracts=st_drop_geometry(Cincinnati_tracts)
Cincinnati_intersects$city_hex=paste0("Cincinnati","_",Cincinnati_intersects$hex_id)
Cincinnati_intersects$prop_hex_intersected=Cincinnati_intersects$area_intersected/Cincinnati_intersects$hex_area
Cincinnati_intersects$prop_tract_intersected=Cincinnati_intersects$area_intersected/Cincinnati_intersects$tract_area
#need to create weighted averages
combined_df_Cincinnati=combined_df[which(combined_df$city=="Cincinnati"),]
combined_df_Cincinnati$stay_tract_white=0
combined_df_Cincinnati$stay_tract_pooled=0
combined_df_Cincinnati$stay_tract_white_male=0
combined_df_Cincinnati$stay_tract_pooled_male=0
combined_df_Cincinnati$stay_tract_white_female=0
combined_df_Cincinnati$stay_tract_pooled_female=0
combined_df_Cincinnati$two_parent_white=0
combined_df_Cincinnati$two_parent_pooled=0
combined_df_Cincinnati$work_32_white=0
combined_df_Cincinnati$work_24_white=0
combined_df_Cincinnati$work_32_pooled=0
combined_df_Cincinnati$work_24_pooled=0
combined_df_Cincinnati$work_32_white_male=0
combined_df_Cincinnati$work_24_white_male=0
combined_df_Cincinnati$work_32_pooled_male=0
combined_df_Cincinnati$work_24_pooled_male=0
combined_df_Cincinnati$jail_white=0
combined_df_Cincinnati$jail_pooled=0
combined_df_Cincinnati$jail_white_male=0
combined_df_Cincinnati$jail_pooled_male=0
combined_df_Cincinnati$foreign_born_share_2010=0
combined_df_Cincinnati$employment_2000=0
combined_df_Cincinnati$college_rate_2000=0
combined_df_Cincinnati$college_rate_2010=0
combined_df_Cincinnati$black_2000=0
combined_df_Cincinnati$black_2010=0
combined_df_Cincinnati$white_2000=0
combined_df_Cincinnati$white_2010=0
combined_df_Cincinnati$median_income_1990=0
combined_df_Cincinnati$median_income_2016=0
combined_df_Cincinnati$poor_1990=0
combined_df_Cincinnati$poor_2000=0
combined_df_Cincinnati$poor_2010=0
combined_df_Cincinnati$rent_2015=0
combined_df_Cincinnati$single_parent_share_1990=0
combined_df_Cincinnati$single_parent_share_2000=0
combined_df_Cincinnati$single_parent_share_2010=0
for (i in 1:nrow(combined_df_Cincinnati)){
  tmp=Cincinnati_intersects[which(Cincinnati_intersects$city_hex==combined_df_Cincinnati$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$staytract_white_pooled_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_pooled_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_male_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_male_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_white_female_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_white_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_white_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$staytract_pooled_female_mean)==FALSE),]
    combined_df_Cincinnati$stay_tract_pooled_female[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$staytract_pooled_female_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_white_pooled_mean)==FALSE),]
    combined_df_Cincinnati$two_parent_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$two_par_pooled_pooled_mean)==FALSE),]
    combined_df_Cincinnati$two_parent_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$two_par_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_pooled_mean)==FALSE),]
    combined_df_Cincinnati$work_32_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_pooled_mean)==FALSE),]
    combined_df_Cincinnati$work_24_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_pooled_mean)==FALSE),]
    combined_df_Cincinnati$work_32_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_pooled_mean)==FALSE),]
    combined_df_Cincinnati$work_24_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_white_male_mean)==FALSE),]
    combined_df_Cincinnati$work_32_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_24_white_male_mean)==FALSE),]
    combined_df_Cincinnati$work_24_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$work_32_pooled_male_mean)==FALSE),]
    combined_df_Cincinnati$work_32_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_32_pooled_male), 0)
    tmp2=tmp[which(is.na(tmp$work_24_pooled_male_mean)==FALSE),]
    combined_df_Cincinnati$work_24_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$work_24_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_pooled_mean)==FALSE),]
    combined_df_Cincinnati$jail_white[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_pooled_mean)==FALSE),]
    combined_df_Cincinnati$jail_pooled[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_pooled_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_white_male_mean)==FALSE),]
    combined_df_Cincinnati$jail_white_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_white_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$jail_pooled_male_mean)==FALSE),]
    combined_df_Cincinnati$jail_pooled_male[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$jail_pooled_male_mean), 0)
    tmp2=tmp[which(is.na(tmp$foreign_share2010)==FALSE),]
    combined_df_Cincinnati$foreign_born_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$foreign_share2010), 0)
    tmp2=tmp[which(is.na(tmp$emp2000)==FALSE),]
    combined_df_Cincinnati$employment_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$emp2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2000)==FALSE),]
    combined_df_Cincinnati$college_rate_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2000), 0)
    tmp2=tmp[which(is.na(tmp$frac_coll_plus2010)==FALSE),]
    combined_df_Cincinnati$college_rate_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$frac_coll_plus2010), 0)
    tmp2=tmp[which(is.na(tmp$share_black2000)==FALSE),]
    combined_df_Cincinnati$black_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2000), 0)
    tmp2=tmp[which(is.na(tmp$share_black2010)==FALSE),]
    combined_df_Cincinnati$black_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_black2010), 0)
    tmp2=tmp[which(is.na(tmp$share_white2000)==FALSE),]
    combined_df_Cincinnati$white_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2000), 0)
    tmp2=tmp[which(is.na(tmp$share_white2010)==FALSE),]
    combined_df_Cincinnati$white_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$share_white2010), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc1990)==FALSE),]
    combined_df_Cincinnati$median_income_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc1990), 0)
    tmp2=tmp[which(is.na(tmp$med_hhinc2016)==FALSE),]
    combined_df_Cincinnati$median_income_2016[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$med_hhinc2016), 0)
    tmp2=tmp[which(is.na(tmp$poor_share1990)==FALSE),]
    combined_df_Cincinnati$poor_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share1990), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2000)==FALSE),]
    combined_df_Cincinnati$poor_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2000), 0)
    tmp2=tmp[which(is.na(tmp$poor_share2010)==FALSE),]
    combined_df_Cincinnati$poor_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$poor_share2010), 0)
    tmp2=tmp[which(is.na(tmp$rent_twobed2015)==FALSE),]
    combined_df_Cincinnati$rent_2015[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$rent_twobed2015), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share1990)==FALSE),]
    combined_df_Cincinnati$single_parent_share_1990[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share1990), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2000)==FALSE),]
    combined_df_Cincinnati$single_parent_share_2000[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2000), 0)
    tmp2=tmp[which(is.na(tmp$singleparent_share2010)==FALSE),]
    combined_df_Cincinnati$single_parent_share_2010[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$singleparent_share2010), 0)
  } else{
    combined_df_Cincinnati$stay_tract_white[i]=0
    combined_df_Cincinnati$stay_tract_pooled[i]=0
    combined_df_Cincinnati$stay_tract_white_male[i]=0
    combined_df_Cincinnati$stay_tract_pooled_male[i]=0
    combined_df_Cincinnati$stay_tract_white_female[i]=0
    combined_df_Cincinnati$stay_tract_pooled_female[i]=0
    combined_df_Cincinnati$two_parent_white[i]=0
    combined_df_Cincinnati$two_parent_pooled[i]=0
    combined_df_Cincinnati$work_32_white[i]=0
    combined_df_Cincinnati$work_24_white[i]=0
    combined_df_Cincinnati$work_32_pooled[i]=0
    combined_df_Cincinnati$work_24_pooled[i]=0
    combined_df_Cincinnati$work_32_white_male[i]=0
    combined_df_Cincinnati$work_24_white_male[i]=0
    combined_df_Cincinnati$work_32_pooled_male[i]=0
    combined_df_Cincinnati$work_24_pooled_male[i]=0
    combined_df_Cincinnati$jail_white[i]=0
    combined_df_Cincinnati$jail_pooled[i]=0
    combined_df_Cincinnati$jail_white_male[i]=0
    combined_df_Cincinnati$jail_pooled_male[i]=0
    combined_df_Cincinnati$foreign_born_share_2010[i]=0
    combined_df_Cincinnati$employment_2000[i]=0
    combined_df_Cincinnati$college_rate_2000[i]=0
    combined_df_Cincinnati$college_rate_2010[i]=0
    combined_df_Cincinnati$black_2000[i]=0
    combined_df_Cincinnati$black_2010[i]=0
    combined_df_Cincinnati$white_2000[i]=0
    combined_df_Cincinnati$white_2010[i]=0
    combined_df_Cincinnati$median_income_1990[i]=0
    combined_df_Cincinnati$median_income_2016[i]=0
    combined_df_Cincinnati$poor_1990[i]=0
    combined_df_Cincinnati$poor_2000[i]=0
    combined_df_Cincinnati$poor_2010[i]=0
    combined_df_Cincinnati$rent_2015[i]=0
    combined_df_Cincinnati$single_parent_share_1990[i]=0
    combined_df_Cincinnati$single_parent_share_2000[i]=0
    combined_df_Cincinnati$single_parent_share_2010[i]=0
  }
}

combined_df_oi=rbind.data.frame(combined_df_Baltimore, combined_df_Boston)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Brooklyn)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Cleveland)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Chicago)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Detroit)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Manhattan)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Philadelphia)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Pittsburgh)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_StLouis)
combined_df_oi=rbind.data.frame(combined_df_oi, combined_df_Cincinnati)

rm(oi_tract_covariates)
rm(oi_tract_early)
gc()

combined_df_oi=combined_df_oi[which(combined_df_oi$year==1920),]
#now on to zip code stuff
opportunity_insights_zip=read.csv(file="input_data/step_3_input_data/opportunity_insights/social_capital_zip.csv")
opportunity_insights_zip$zip=ifelse(nchar(opportunity_insights_zip$zip)==4, paste0("0", opportunity_insights_zip$zip), opportunity_insights_zip$zip)

load("analysis_data/combined_df.rda")
#zips
zip_shape=st_read("input_data/step_3_input_data/redline_data/nhgis0028_shape/nhgis0028_shapefile_tl2020_us_zcta_2020")
zip_shape$zip=zip_shape$ZCTA5CE20
zips=merge(zip_shape, opportunity_insights_zip, by="zip")

#Baltimore
Baltimore_zips=st_transform(zips, st_crs(Baltimore_hexgrid))
Baltimore_zips=st_make_valid(Baltimore_zips)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
Baltimore_intersects=st_intersection(Baltimore_hexgrid, Baltimore_zips)
Baltimore_intersects$area_intersected=as.numeric(st_area(Baltimore_intersects))
Baltimore_zips$zip_area=as.numeric(st_area(Baltimore_zips))
Baltimore_zips=st_drop_geometry(Baltimore_zips)
Baltimore_intersects$city_hex=paste0("Baltimore","_",Baltimore_intersects$hex_id)
combined_df_Baltimore=combined_df[which(combined_df$city=="Baltimore"),]
combined_df_Baltimore$clustering=0
combined_df_Baltimore$economic_connectedness=0
combined_df_Baltimore$neighborhood_economic_connectness=0
combined_df_Baltimore$economic_connectedness_group=0
combined_df_Baltimore$exposure=0
combined_df_Baltimore$neighborhood_exposure=0
combined_df_Baltimore$rich_bias_group=0
combined_df_Baltimore$rich_bias_neighborhood=0
combined_df_Baltimore$support_ratio=0
combined_df_Baltimore$volunteering=0
combined_df_Baltimore$civic_organizations=0
for (i in 1:nrow(combined_df_Baltimore)){
  tmp=Baltimore_intersects[which(Baltimore_intersects$city_hex==combined_df_Baltimore$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Baltimore$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Baltimore$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Baltimore$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Baltimore$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Baltimore$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Baltimore$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Baltimore$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Baltimore$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Baltimore$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Baltimore$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Baltimore$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Baltimore$clustering[i]=0
    combined_df_Baltimore$economic_connectedness[i]=0
    combined_df_Baltimore$neighborhood_economic_connectness[i]=0
    combined_df_Baltimore$economic_connectedness_group[i]=0
    combined_df_Baltimore$exposure[i]=0
    combined_df_Baltimore$neighborhood_exposure[i]=0
    combined_df_Baltimore$rich_bias_group[i]=0
    combined_df_Baltimore$rich_bias_neighborhood[i]=0
    combined_df_Baltimore$support_ratio[i]=0
    combined_df_Baltimore$volunteering[i]=0
    combined_df_Baltimore$civic_organizations[i]=0
  }
}

#Boston
Boston_zips=st_transform(zips, st_crs(Boston_hexgrid))
Boston_zips=st_make_valid(Boston_zips)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
Boston_intersects=st_intersection(Boston_hexgrid, Boston_zips)
Boston_intersects$area_intersected=as.numeric(st_area(Boston_intersects))
Boston_zips$zip_area=as.numeric(st_area(Boston_zips))
Boston_zips=st_drop_geometry(Boston_zips)
Boston_intersects$city_hex=paste0("Boston","_",Boston_intersects$hex_id)
combined_df_Boston=combined_df[which(combined_df$city=="Boston"),]
combined_df_Boston$clustering=0
combined_df_Boston$economic_connectedness=0
combined_df_Boston$neighborhood_economic_connectness=0
combined_df_Boston$economic_connectedness_group=0
combined_df_Boston$exposure=0
combined_df_Boston$neighborhood_exposure=0
combined_df_Boston$rich_bias_group=0
combined_df_Boston$rich_bias_neighborhood=0
combined_df_Boston$support_ratio=0
combined_df_Boston$volunteering=0
combined_df_Boston$civic_organizations=0
for (i in 1:nrow(combined_df_Boston)){
  tmp=Boston_intersects[which(Boston_intersects$city_hex==combined_df_Boston$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Boston$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Boston$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Boston$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Boston$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Boston$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Boston$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Boston$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Boston$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Boston$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Boston$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Boston$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Boston$clustering[i]=0
    combined_df_Boston$economic_connectedness[i]=0
    combined_df_Boston$neighborhood_economic_connectness[i]=0
    combined_df_Boston$economic_connectedness_group[i]=0
    combined_df_Boston$exposure[i]=0
    combined_df_Boston$neighborhood_exposure[i]=0
    combined_df_Boston$rich_bias_group[i]=0
    combined_df_Boston$rich_bias_neighborhood[i]=0
    combined_df_Boston$support_ratio[i]=0
    combined_df_Boston$volunteering[i]=0
    combined_df_Boston$civic_organizations[i]=0
  }
}

#Brooklyn
Brooklyn_zips=st_transform(zips, st_crs(Brooklyn_hexgrid))
Brooklyn_zips=st_make_valid(Brooklyn_zips)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
Brooklyn_intersects=st_intersection(Brooklyn_hexgrid, Brooklyn_zips)
Brooklyn_intersects$area_intersected=as.numeric(st_area(Brooklyn_intersects))
Brooklyn_zips$zip_area=as.numeric(st_area(Brooklyn_zips))
Brooklyn_zips=st_drop_geometry(Brooklyn_zips)
Brooklyn_intersects$city_hex=paste0("Brooklyn","_",Brooklyn_intersects$hex_id)
combined_df_Brooklyn=combined_df[which(combined_df$city=="Brooklyn"),]
combined_df_Brooklyn$clustering=0
combined_df_Brooklyn$economic_connectedness=0
combined_df_Brooklyn$neighborhood_economic_connectness=0
combined_df_Brooklyn$economic_connectedness_group=0
combined_df_Brooklyn$exposure=0
combined_df_Brooklyn$neighborhood_exposure=0
combined_df_Brooklyn$rich_bias_group=0
combined_df_Brooklyn$rich_bias_neighborhood=0
combined_df_Brooklyn$support_ratio=0
combined_df_Brooklyn$volunteering=0
combined_df_Brooklyn$civic_organizations=0
for (i in 1:nrow(combined_df_Brooklyn)){
  tmp=Brooklyn_intersects[which(Brooklyn_intersects$city_hex==combined_df_Brooklyn$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Brooklyn$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Brooklyn$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Brooklyn$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Brooklyn$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Brooklyn$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Brooklyn$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Brooklyn$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Brooklyn$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Brooklyn$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Brooklyn$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Brooklyn$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Brooklyn$clustering[i]=0
    combined_df_Brooklyn$economic_connectedness[i]=0
    combined_df_Brooklyn$neighborhood_economic_connectness[i]=0
    combined_df_Brooklyn$economic_connectedness_group[i]=0
    combined_df_Brooklyn$exposure[i]=0
    combined_df_Brooklyn$neighborhood_exposure[i]=0
    combined_df_Brooklyn$rich_bias_group[i]=0
    combined_df_Brooklyn$rich_bias_neighborhood[i]=0
    combined_df_Brooklyn$support_ratio[i]=0
    combined_df_Brooklyn$volunteering[i]=0
    combined_df_Brooklyn$civic_organizations[i]=0
  }
}

#Cleveland
Cleveland_zips=st_transform(zips, st_crs(Cleveland_hexgrid))
Cleveland_zips=st_make_valid(Cleveland_zips)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
Cleveland_intersects=st_intersection(Cleveland_hexgrid, Cleveland_zips)
Cleveland_intersects$area_intersected=as.numeric(st_area(Cleveland_intersects))
Cleveland_zips$zip_area=as.numeric(st_area(Cleveland_zips))
Cleveland_zips=st_drop_geometry(Cleveland_zips)
Cleveland_intersects$city_hex=paste0("Cleveland","_",Cleveland_intersects$hex_id)
combined_df_Cleveland=combined_df[which(combined_df$city=="Cleveland"),]
combined_df_Cleveland$clustering=0
combined_df_Cleveland$economic_connectedness=0
combined_df_Cleveland$neighborhood_economic_connectness=0
combined_df_Cleveland$economic_connectedness_group=0
combined_df_Cleveland$exposure=0
combined_df_Cleveland$neighborhood_exposure=0
combined_df_Cleveland$rich_bias_group=0
combined_df_Cleveland$rich_bias_neighborhood=0
combined_df_Cleveland$support_ratio=0
combined_df_Cleveland$volunteering=0
combined_df_Cleveland$civic_organizations=0
for (i in 1:nrow(combined_df_Cleveland)){
  tmp=Cleveland_intersects[which(Cleveland_intersects$city_hex==combined_df_Cleveland$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Cleveland$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Cleveland$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Cleveland$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Cleveland$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Cleveland$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Cleveland$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Cleveland$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Cleveland$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Cleveland$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Cleveland$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Cleveland$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Cleveland$clustering[i]=0
    combined_df_Cleveland$economic_connectedness[i]=0
    combined_df_Cleveland$neighborhood_economic_connectness[i]=0
    combined_df_Cleveland$economic_connectedness_group[i]=0
    combined_df_Cleveland$exposure[i]=0
    combined_df_Cleveland$neighborhood_exposure[i]=0
    combined_df_Cleveland$rich_bias_group[i]=0
    combined_df_Cleveland$rich_bias_neighborhood[i]=0
    combined_df_Cleveland$support_ratio[i]=0
    combined_df_Cleveland$volunteering[i]=0
    combined_df_Cleveland$civic_organizations[i]=0
  }
}

#Chicago
Chicago_zips=st_transform(zips, st_crs(Chicago_hexgrid))
Chicago_zips=st_make_valid(Chicago_zips)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
Chicago_intersects=st_intersection(Chicago_hexgrid, Chicago_zips)
Chicago_intersects$area_intersected=as.numeric(st_area(Chicago_intersects))
Chicago_zips$zip_area=as.numeric(st_area(Chicago_zips))
Chicago_zips=st_drop_geometry(Chicago_zips)
Chicago_intersects$city_hex=paste0("Chicago","_",Chicago_intersects$hex_id)
combined_df_Chicago=combined_df[which(combined_df$city=="Chicago"),]
combined_df_Chicago$clustering=0
combined_df_Chicago$economic_connectedness=0
combined_df_Chicago$neighborhood_economic_connectness=0
combined_df_Chicago$economic_connectedness_group=0
combined_df_Chicago$exposure=0
combined_df_Chicago$neighborhood_exposure=0
combined_df_Chicago$rich_bias_group=0
combined_df_Chicago$rich_bias_neighborhood=0
combined_df_Chicago$support_ratio=0
combined_df_Chicago$volunteering=0
combined_df_Chicago$civic_organizations=0
for (i in 1:nrow(combined_df_Chicago)){
  tmp=Chicago_intersects[which(Chicago_intersects$city_hex==combined_df_Chicago$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Chicago$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Chicago$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Chicago$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Chicago$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Chicago$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Chicago$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Chicago$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Chicago$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Chicago$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Chicago$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Chicago$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Chicago$clustering[i]=0
    combined_df_Chicago$economic_connectedness[i]=0
    combined_df_Chicago$neighborhood_economic_connectness[i]=0
    combined_df_Chicago$economic_connectedness_group[i]=0
    combined_df_Chicago$exposure[i]=0
    combined_df_Chicago$neighborhood_exposure[i]=0
    combined_df_Chicago$rich_bias_group[i]=0
    combined_df_Chicago$rich_bias_neighborhood[i]=0
    combined_df_Chicago$support_ratio[i]=0
    combined_df_Chicago$volunteering[i]=0
    combined_df_Chicago$civic_organizations[i]=0
  }
}

#Cincinnati
Cincinnati_zips=st_transform(zips, st_crs(Cincinnati_hexgrid))
Cincinnati_zips=st_make_valid(Cincinnati_zips)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
Cincinnati_intersects=st_intersection(Cincinnati_hexgrid, Cincinnati_zips)
Cincinnati_intersects$area_intersected=as.numeric(st_area(Cincinnati_intersects))
Cincinnati_zips$zip_area=as.numeric(st_area(Cincinnati_zips))
Cincinnati_zips=st_drop_geometry(Cincinnati_zips)
Cincinnati_intersects$city_hex=paste0("Cincinnati","_",Cincinnati_intersects$hex_id)
combined_df_Cincinnati=combined_df[which(combined_df$city=="Cincinnati"),]
combined_df_Cincinnati$clustering=0
combined_df_Cincinnati$economic_connectedness=0
combined_df_Cincinnati$neighborhood_economic_connectness=0
combined_df_Cincinnati$economic_connectedness_group=0
combined_df_Cincinnati$exposure=0
combined_df_Cincinnati$neighborhood_exposure=0
combined_df_Cincinnati$rich_bias_group=0
combined_df_Cincinnati$rich_bias_neighborhood=0
combined_df_Cincinnati$support_ratio=0
combined_df_Cincinnati$volunteering=0
combined_df_Cincinnati$civic_organizations=0
for (i in 1:nrow(combined_df_Cincinnati)){
  tmp=Cincinnati_intersects[which(Cincinnati_intersects$city_hex==combined_df_Cincinnati$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Cincinnati$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Cincinnati$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Cincinnati$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Cincinnati$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Cincinnati$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Cincinnati$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Cincinnati$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Cincinnati$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Cincinnati$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Cincinnati$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Cincinnati$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Cincinnati$clustering[i]=0
    combined_df_Cincinnati$economic_connectedness[i]=0
    combined_df_Cincinnati$neighborhood_economic_connectness[i]=0
    combined_df_Cincinnati$economic_connectedness_group[i]=0
    combined_df_Cincinnati$exposure[i]=0
    combined_df_Cincinnati$neighborhood_exposure[i]=0
    combined_df_Cincinnati$rich_bias_group[i]=0
    combined_df_Cincinnati$rich_bias_neighborhood[i]=0
    combined_df_Cincinnati$support_ratio[i]=0
    combined_df_Cincinnati$volunteering[i]=0
    combined_df_Cincinnati$civic_organizations[i]=0
  }
}

#Detroit
Detroit_zips=st_transform(zips, st_crs(Detroit_hexgrid))
Detroit_zips=st_make_valid(Detroit_zips)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
Detroit_intersects=st_intersection(Detroit_hexgrid, Detroit_zips)
Detroit_intersects$area_intersected=as.numeric(st_area(Detroit_intersects))
Detroit_zips$zip_area=as.numeric(st_area(Detroit_zips))
Detroit_zips=st_drop_geometry(Detroit_zips)
Detroit_intersects$city_hex=paste0("Detroit","_",Detroit_intersects$hex_id)
combined_df_Detroit=combined_df[which(combined_df$city=="Detroit"),]
combined_df_Detroit$clustering=0
combined_df_Detroit$economic_connectedness=0
combined_df_Detroit$neighborhood_economic_connectness=0
combined_df_Detroit$economic_connectedness_group=0
combined_df_Detroit$exposure=0
combined_df_Detroit$neighborhood_exposure=0
combined_df_Detroit$rich_bias_group=0
combined_df_Detroit$rich_bias_neighborhood=0
combined_df_Detroit$support_ratio=0
combined_df_Detroit$volunteering=0
combined_df_Detroit$civic_organizations=0
for (i in 1:nrow(combined_df_Detroit)){
  tmp=Detroit_intersects[which(Detroit_intersects$city_hex==combined_df_Detroit$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Detroit$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Detroit$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Detroit$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Detroit$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Detroit$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Detroit$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Detroit$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Detroit$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Detroit$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Detroit$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Detroit$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Detroit$clustering[i]=0
    combined_df_Detroit$economic_connectedness[i]=0
    combined_df_Detroit$neighborhood_economic_connectness[i]=0
    combined_df_Detroit$economic_connectedness_group[i]=0
    combined_df_Detroit$exposure[i]=0
    combined_df_Detroit$neighborhood_exposure[i]=0
    combined_df_Detroit$rich_bias_group[i]=0
    combined_df_Detroit$rich_bias_neighborhood[i]=0
    combined_df_Detroit$support_ratio[i]=0
    combined_df_Detroit$volunteering[i]=0
    combined_df_Detroit$civic_organizations[i]=0
  }
}

#Manhattan
Manhattan_zips=st_transform(zips, st_crs(Manhattan_hexgrid))
Manhattan_zips=st_make_valid(Manhattan_zips)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
Manhattan_intersects=st_intersection(Manhattan_hexgrid, Manhattan_zips)
Manhattan_intersects$area_intersected=as.numeric(st_area(Manhattan_intersects))
Manhattan_zips$zip_area=as.numeric(st_area(Manhattan_zips))
Manhattan_zips=st_drop_geometry(Manhattan_zips)
Manhattan_intersects$city_hex=paste0("Manhattan","_",Manhattan_intersects$hex_id)
combined_df_Manhattan=combined_df[which(combined_df$city=="Manhattan"),]
combined_df_Manhattan$clustering=0
combined_df_Manhattan$economic_connectedness=0
combined_df_Manhattan$neighborhood_economic_connectness=0
combined_df_Manhattan$economic_connectedness_group=0
combined_df_Manhattan$exposure=0
combined_df_Manhattan$neighborhood_exposure=0
combined_df_Manhattan$rich_bias_group=0
combined_df_Manhattan$rich_bias_neighborhood=0
combined_df_Manhattan$support_ratio=0
combined_df_Manhattan$volunteering=0
combined_df_Manhattan$civic_organizations=0
for (i in 1:nrow(combined_df_Manhattan)){
  tmp=Manhattan_intersects[which(Manhattan_intersects$city_hex==combined_df_Manhattan$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Manhattan$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Manhattan$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Manhattan$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Manhattan$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Manhattan$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Manhattan$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Manhattan$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Manhattan$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Manhattan$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Manhattan$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Manhattan$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Manhattan$clustering[i]=0
    combined_df_Manhattan$economic_connectedness[i]=0
    combined_df_Manhattan$neighborhood_economic_connectness[i]=0
    combined_df_Manhattan$economic_connectedness_group[i]=0
    combined_df_Manhattan$exposure[i]=0
    combined_df_Manhattan$neighborhood_exposure[i]=0
    combined_df_Manhattan$rich_bias_group[i]=0
    combined_df_Manhattan$rich_bias_neighborhood[i]=0
    combined_df_Manhattan$support_ratio[i]=0
    combined_df_Manhattan$volunteering[i]=0
    combined_df_Manhattan$civic_organizations[i]=0
  }
}

#Philadelphia
Philadelphia_zips=st_transform(zips, st_crs(Philadelphia_hexgrid))
Philadelphia_zips=st_make_valid(Philadelphia_zips)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
Philadelphia_intersects=st_intersection(Philadelphia_hexgrid, Philadelphia_zips)
Philadelphia_intersects$area_intersected=as.numeric(st_area(Philadelphia_intersects))
Philadelphia_zips$zip_area=as.numeric(st_area(Philadelphia_zips))
Philadelphia_zips=st_drop_geometry(Philadelphia_zips)
Philadelphia_intersects$city_hex=paste0("Philadelphia","_",Philadelphia_intersects$hex_id)
combined_df_Philadelphia=combined_df[which(combined_df$city=="Philadelphia"),]
combined_df_Philadelphia$clustering=0
combined_df_Philadelphia$economic_connectedness=0
combined_df_Philadelphia$neighborhood_economic_connectness=0
combined_df_Philadelphia$economic_connectedness_group=0
combined_df_Philadelphia$exposure=0
combined_df_Philadelphia$neighborhood_exposure=0
combined_df_Philadelphia$rich_bias_group=0
combined_df_Philadelphia$rich_bias_neighborhood=0
combined_df_Philadelphia$support_ratio=0
combined_df_Philadelphia$volunteering=0
combined_df_Philadelphia$civic_organizations=0
for (i in 1:nrow(combined_df_Philadelphia)){
  tmp=Philadelphia_intersects[which(Philadelphia_intersects$city_hex==combined_df_Philadelphia$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Philadelphia$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Philadelphia$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Philadelphia$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Philadelphia$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Philadelphia$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Philadelphia$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Philadelphia$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Philadelphia$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Philadelphia$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Philadelphia$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Philadelphia$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Philadelphia$clustering[i]=0
    combined_df_Philadelphia$economic_connectedness[i]=0
    combined_df_Philadelphia$neighborhood_economic_connectness[i]=0
    combined_df_Philadelphia$economic_connectedness_group[i]=0
    combined_df_Philadelphia$exposure[i]=0
    combined_df_Philadelphia$neighborhood_exposure[i]=0
    combined_df_Philadelphia$rich_bias_group[i]=0
    combined_df_Philadelphia$rich_bias_neighborhood[i]=0
    combined_df_Philadelphia$support_ratio[i]=0
    combined_df_Philadelphia$volunteering[i]=0
    combined_df_Philadelphia$civic_organizations[i]=0
  }
}

#Pittsburgh
Pittsburgh_zips=st_transform(zips, st_crs(Pittsburgh_hexgrid))
Pittsburgh_zips=st_make_valid(Pittsburgh_zips)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
Pittsburgh_intersects=st_intersection(Pittsburgh_hexgrid, Pittsburgh_zips)
Pittsburgh_intersects$area_intersected=as.numeric(st_area(Pittsburgh_intersects))
Pittsburgh_zips$zip_area=as.numeric(st_area(Pittsburgh_zips))
Pittsburgh_zips=st_drop_geometry(Pittsburgh_zips)
Pittsburgh_intersects$city_hex=paste0("Pittsburgh","_",Pittsburgh_intersects$hex_id)
combined_df_Pittsburgh=combined_df[which(combined_df$city=="Pittsburgh"),]
combined_df_Pittsburgh$clustering=0
combined_df_Pittsburgh$economic_connectedness=0
combined_df_Pittsburgh$neighborhood_economic_connectness=0
combined_df_Pittsburgh$economic_connectedness_group=0
combined_df_Pittsburgh$exposure=0
combined_df_Pittsburgh$neighborhood_exposure=0
combined_df_Pittsburgh$rich_bias_group=0
combined_df_Pittsburgh$rich_bias_neighborhood=0
combined_df_Pittsburgh$support_ratio=0
combined_df_Pittsburgh$volunteering=0
combined_df_Pittsburgh$civic_organizations=0
for (i in 1:nrow(combined_df_Pittsburgh)){
  tmp=Pittsburgh_intersects[which(Pittsburgh_intersects$city_hex==combined_df_Pittsburgh$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_Pittsburgh$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_Pittsburgh$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_Pittsburgh$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_Pittsburgh$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_Pittsburgh$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_Pittsburgh$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_Pittsburgh$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_Pittsburgh$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_Pittsburgh$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_Pittsburgh$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_Pittsburgh$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_Pittsburgh$clustering[i]=0
    combined_df_Pittsburgh$economic_connectedness[i]=0
    combined_df_Pittsburgh$neighborhood_economic_connectness[i]=0
    combined_df_Pittsburgh$economic_connectedness_group[i]=0
    combined_df_Pittsburgh$exposure[i]=0
    combined_df_Pittsburgh$neighborhood_exposure[i]=0
    combined_df_Pittsburgh$rich_bias_group[i]=0
    combined_df_Pittsburgh$rich_bias_neighborhood[i]=0
    combined_df_Pittsburgh$support_ratio[i]=0
    combined_df_Pittsburgh$volunteering[i]=0
    combined_df_Pittsburgh$civic_organizations[i]=0
  }
}

#StLouis
StLouis_zips=st_transform(zips, st_crs(StLouis_hexgrid))
StLouis_zips=st_make_valid(StLouis_zips)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
StLouis_intersects=st_intersection(StLouis_hexgrid, StLouis_zips)
StLouis_intersects$area_intersected=as.numeric(st_area(StLouis_intersects))
StLouis_zips$zip_area=as.numeric(st_area(StLouis_zips))
StLouis_zips=st_drop_geometry(StLouis_zips)
StLouis_intersects$city_hex=paste0("StLouis","_",StLouis_intersects$hex_id)
combined_df_StLouis=combined_df[which(combined_df$city=="StLouis"),]
combined_df_StLouis$clustering=0
combined_df_StLouis$economic_connectedness=0
combined_df_StLouis$neighborhood_economic_connectness=0
combined_df_StLouis$economic_connectedness_group=0
combined_df_StLouis$exposure=0
combined_df_StLouis$neighborhood_exposure=0
combined_df_StLouis$rich_bias_group=0
combined_df_StLouis$rich_bias_neighborhood=0
combined_df_StLouis$support_ratio=0
combined_df_StLouis$volunteering=0
combined_df_StLouis$civic_organizations=0
for (i in 1:nrow(combined_df_StLouis)){
  tmp=StLouis_intersects[which(StLouis_intersects$city_hex==combined_df_StLouis$city_hex[i]),]
  if (nrow(tmp)>0){
    tmp=st_drop_geometry(tmp)
    tmp$weight=tmp$area_intersected/sum(tmp$area_intersected)
    tmp2=tmp[which(is.na(tmp$clustering_zip)==FALSE),]
    combined_df_StLouis$clustering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$clustering_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_zip)==FALSE),]
    combined_df_StLouis$economic_connectedness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_ec_zip)==FALSE),]
    combined_df_StLouis$neighborhood_economic_connectness[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_ec_zip), 0)
    tmp2=tmp[which(is.na(tmp$ec_grp_mem_zip)==FALSE),]
    combined_df_StLouis$economic_connectedness_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$ec_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$exposure_grp_mem_zip)==FALSE),]
    combined_df_StLouis$exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$exposure_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_exposure_zip)==FALSE),]
    combined_df_StLouis$neighborhood_exposure[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_exposure_zip), 0)
    tmp2=tmp[which(is.na(tmp$bias_grp_mem_zip)==FALSE),]
    combined_df_StLouis$rich_bias_group[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$bias_grp_mem_zip), 0)
    tmp2=tmp[which(is.na(tmp$nbhd_bias_zip)==FALSE),]
    combined_df_StLouis$rich_bias_neighborhood[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$nbhd_bias_zip), 0)
    tmp2=tmp[which(is.na(tmp$support_ratio_zip)==FALSE),]
    combined_df_StLouis$support_ratio[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$support_ratio_zip), 0)
    tmp2=tmp[which(is.na(tmp$volunteering_rate_zip)==FALSE),]
    combined_df_StLouis$volunteering[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$volunteering_rate_zip), 0)
    tmp2=tmp[which(is.na(tmp$civic_organizations_zip)==FALSE),]
    combined_df_StLouis$civic_organizations[i]=ifelse(nrow(tmp2)>0, sum(tmp2$weight*tmp2$civic_organizations_zip), 0)
  } else{
    combined_df_StLouis$clustering[i]=0
    combined_df_StLouis$economic_connectedness[i]=0
    combined_df_StLouis$neighborhood_economic_connectness[i]=0
    combined_df_StLouis$economic_connectedness_group[i]=0
    combined_df_StLouis$exposure[i]=0
    combined_df_StLouis$neighborhood_exposure[i]=0
    combined_df_StLouis$rich_bias_group[i]=0
    combined_df_StLouis$rich_bias_neighborhood[i]=0
    combined_df_StLouis$support_ratio[i]=0
    combined_df_StLouis$volunteering[i]=0
    combined_df_StLouis$civic_organizations[i]=0
  }
}

combined_df_oi_zip=rbind.data.frame(combined_df_Baltimore, combined_df_Boston)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Brooklyn)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Cleveland)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Chicago)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Detroit)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Manhattan)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Philadelphia)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Pittsburgh)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_StLouis)
combined_df_oi_zip=rbind.data.frame(combined_df_oi_zip, combined_df_Cincinnati)

combined_df_oi_zip=combined_df_oi_zip[which(combined_df_oi_zip$year==1920),]

combined_df_oi$clustering=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"clustering" ]
combined_df_oi$economic_connectedness=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"economic_connectedness"]
combined_df_oi$neighborhood_economic_connectness=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"neighborhood_economic_connectness"]
combined_df_oi$economic_connectedness_group=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"economic_connectedness_group"]
combined_df_oi$exposure=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"exposure"]
combined_df_oi$neighborhood_exposure=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"neighborhood_exposure"]
combined_df_oi$rich_bias_group=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"rich_bias_group"]
combined_df_oi$rich_bias_neighborhood=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"rich_bias_neighborhood"]
combined_df_oi$support_ratio=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"support_ratio"]
combined_df_oi$volunteering=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"volunteering"]
combined_df_oi$civic_organizations=combined_df_oi_zip[match(combined_df_oi$city_hex, combined_df_oi_zip$city_hex),"civic_organizations"]

save(combined_df_oi, file="analysis_data/combined_df_opportunity_insights.rda")
