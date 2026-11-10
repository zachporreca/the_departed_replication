############################################################################################################
###################### ANALYSIS SCRIPT 2: THIS SCRIPT REPLICATES SECONDARY RESULTS #########################
######################                    FIGURES AND TABLES FOR OUTCOMES/FEATURE  #########################
######################                    RELYING ON OPPORTUNITY INSIGHTS AND      #########################
######################                    REDLINING DATA                           #########################
############################################################################################################
load("analysis_data/combined_df_opportunity_insights.rda")
load("intermediate_outputs/neighbors.rda")
load("analysis_data/combined_df_redline.rda")

library(fixest)
############################################################
############## TABLE REPLICATIONS #########################
###########################################################

#######################
##### TABLE 5 #########
#######################
#prelim table about redlining
combined_df_test=combined_df_oi
combined_df_test$redline_grade_D=combined_df_redline[match(combined_df_test$city_hex, combined_df_test$city_hex),"redline_grade_D"]
summary(feols(median_income_1990~redline_grade_D|city, data=combined_df_test))
mean(combined_df_test$median_income_1990)
summary(feols(hpi_zip_2020~redline_grade_D|city, data=combined_df_redline[which(combined_df_redline$hpi_zip_2020!=0),]))
mean(combined_df_redline$hpi_zip_2020)
summary(feols(college_rate_2000~redline_grade_D|city, data=combined_df_test))
mean(combined_df_test$college_rate_2000)
summary(feols(poor_1990~redline_grade_D|city, data=combined_df_test))
mean(combined_df_test$poor_1990)

#######################
##### TABLE 6 #########
#######################
#table of mori readline prob
summary(feols(redline_grade_D~fixed_mori_set|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop|city, data=combined_df_redline))
combined_df_redline$any_sicilians=ifelse(combined_df_redline$any_sicilians_1910+combined_df_redline$any_sicilians_1920>0, 1, 0)
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop|city, data=combined_df_redline))
mean(combined_df_redline$redline_grade_D)
### ADDITIONS TO TABLE- CUTRERA AND DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo|city, data=combined_df_redline))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE AND CUTRERA/DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline))


#######################
##### TABLE 7 #########
#######################
#Median Income 2016
feols(median_income_2016~ fixed_mori_set|city, data = combined_df_oi)
feols(median_income_2016~ ital_prop+fixed_mori_set|city, data = combined_df_oi)
feols(median_income_2016~ any_sicilians+ital_prop+fixed_mori_set|city, data = combined_df_oi)

#College Educated 2010
feols(college_rate_2010~ fixed_mori_set|city, data = combined_df_oi)
feols(college_rate_2010~ ital_prop+fixed_mori_set|city, data = combined_df_oi)
feols(college_rate_2010~ any_sicilians+ital_prop+fixed_mori_set|city, data = combined_df_oi)

#College Educated 2000
feols(college_rate_2000~ fixed_mori_set|city, data = combined_df_oi)
feols(college_rate_2000~ ital_prop+fixed_mori_set|city, data = combined_df_oi)
feols(college_rate_2000~ any_sicilians+ital_prop+fixed_mori_set|city, data = combined_df_oi)

#Children employed Age 32
feols(work_32_pooled~ fixed_mori_set|city, data = combined_df_oi)
feols(work_32_pooled~ ital_prop+fixed_mori_set|city, data = combined_df_oi)
feols(work_32_pooled~ any_sicilians+ital_prop+fixed_mori_set|city, data = combined_df_oi)

#Children employed Age 24
feols(work_24_pooled~ fixed_mori_set|city, data = combined_df_oi)
feols(work_24_pooled~ ital_prop+fixed_mori_set|city, data = combined_df_oi)
feols(work_24_pooled~ any_sicilians+ital_prop+fixed_mori_set|city, data = combined_df_oi)

#Same Tract from Childhood to Adulthood
feols(stay_tract_pooled~ fixed_mori_set|city, data = combined_df_oi)
feols(stay_tract_pooled~ fixed_mori_set+ital_prop|city, data = combined_df_oi)
feols(stay_tract_pooled~ fixed_mori_set+ital_prop+any_sicilians|city, data = combined_df_oi)

#######################
##### TABLE 8 #########
#######################
combined_df_oi$mori_neighbor=ifelse(combined_df_oi$city_hex %in% neighbors, 1, 0)
combined_df_oi$post_mori_neighbor=combined_df_oi$mori_neighbor*combined_df_oi$post

#Median Income 2016
feols(median_income_2016~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(median_income_2016~ ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(median_income_2016~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])

#College Educated 2010
feols(college_rate_2010~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(college_rate_2010~ ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(college_rate_2010~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])

#College Educated 2000
feols(college_rate_2000~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(college_rate_2000~ ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(college_rate_2000~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])

#Children employed Age 32
feols(work_32_pooled~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(work_32_pooled~ ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(work_32_pooled~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])

#Children employed Age 24
feols(work_24_pooled~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(work_24_pooled~ ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(work_24_pooled~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])

#Same Tract from Childhood to Adulthood
feols(stay_tract_pooled~ mori_neighbor|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(stay_tract_pooled~ mori_neighbor+ital_prop|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
feols(stay_tract_pooled~ mori_neighbor+ital_prop+any_sicilians|city, data = combined_df_oi[which(combined_df_oi$fixed_mori_set==0),])
