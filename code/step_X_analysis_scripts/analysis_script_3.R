###################################################################################################################
###################### ANALYSIS SCRIPT 3: THIS SCRIPT REPLICATES RESULTS ACROSS MULTIPLE  #########################
######################                    TABLES WHICH CONTAIN CONLEY STANDARD ERRORS FOR #########################
######################                    SPATIAL PERSISTENCE                             #########################
######################                    REDLINING DATA                                  #########################
###################################################################################################################

load("analysis_data/combined_df.rda")
load("intermediate_outputs/hexgrid_conley.rda")
load("analysis_data/combined_df_opportunity_insights.rda")
load("analysis_data/combined_df_redline.rda")
load("intermediate_outputs/neighbors.rda")


library(fixest)
library(conleyreg)
#######################
##### TABLE 3 #########
#######################
combined_df$lat=hexgrid[match(combined_df$city_hex, hexgrid$city_hex),"lat"]
combined_df$lon=hexgrid[match(combined_df$city_hex, hexgrid$city_hex),"lon"]
                        
##### MAFIA LEADER IN 1959
feols(mafia_book_hex~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_hex~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

feols(mafia_book_hex~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set+any_sicilians|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_hex~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set+any_sicilians|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

feols(mafia_book_hex~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_hex~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_hex~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

##### MAFIA GEOCODED LOCATIONS IN 1959
feols(mafia_book_geo~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_geo~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

feols(mafia_book_geo~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set+any_sicilians|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_geo~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set+any_sicilians|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

feols(mafia_book_geo~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(5) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, combined_df[which(combined_df$year=="1920"),], 5, lat="lat", lon="lon")
feols(mafia_book_geo~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df[which(combined_df$year==1920),], vcov = conley(10) ~ lat + lon)
conleyreg(mafia_book_geo~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, combined_df[which(combined_df$year=="1920"),], 10, lat="lat", lon="lon")

#######################
##### TABLE 5 #########
#######################
combined_df_test=combined_df_oi
combined_df_test$redline_grade_D=combined_df_redline[match(combined_df_test$city_hex, combined_df_test$city_hex),"redline_grade_D"]
combined_df_test$lat=hexgrid[match(combined_df_test$city_hex, hexgrid$city_hex),"lat"]
combined_df_test$lon=hexgrid[match(combined_df_test$city_hex, hexgrid$city_hex),"lon"]
combined_df_redline$lat=hexgrid[match(combined_df_redline$city_hex, hexgrid$city_hex),"lat"]
combined_df_redline$lon=hexgrid[match(combined_df_redline$city_hex, hexgrid$city_hex),"lon"]

#prelim table about redlining- 5km Conley
summary(feols(median_income_1990~redline_grade_D|city, data=combined_df_test, vcov = conley(5) ~ lat + lon))
mean(combined_df_test$median_income_1990)
summary(feols(hpi_zip_2020~redline_grade_D|city, data=combined_df_redline[which(combined_df_redline$hpi_zip_2020!=0),], vcov = conley(5) ~ lat + lon))
mean(combined_df_redline$hpi_zip_2020)
summary(feols(college_rate_2000~redline_grade_D|city, data=combined_df_test, vcov = conley(5) ~ lat + lon))
mean(combined_df_test$college_rate_2000)
summary(feols(poor_1990~redline_grade_D|city, data=combined_df_test, vcov = conley(5) ~ lat + lon))
mean(combined_df_test$poor_1990)

#prelim table about redlining- 10km Conley
summary(feols(median_income_1990~redline_grade_D|city, data=combined_df_test, vcov = conley(10) ~ lat + lon))
mean(combined_df_test$median_income_1990)
summary(feols(hpi_zip_2020~redline_grade_D|city, data=combined_df_redline[which(combined_df_redline$hpi_zip_2020!=0),], vcov = conley(10) ~ lat + lon))
mean(combined_df_redline$hpi_zip_2020)
summary(feols(college_rate_2000~redline_grade_D|city, data=combined_df_test, vcov = conley(10) ~ lat + lon))
mean(combined_df_test$college_rate_2000)
summary(feols(poor_1990~redline_grade_D|city, data=combined_df_test, vcov = conley(10) ~ lat + lon))
mean(combined_df_test$poor_1990)

#######################
##### TABLE 6 #########
#######################
##### FIRST SET IS 5 KM CONLEY
#table of mori readline prob
summary(feols(redline_grade_D~fixed_mori_set|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
combined_df_redline$any_sicilians=ifelse(combined_df_redline$any_sicilians_1910+combined_df_redline$any_sicilians_1920>0, 1, 0)
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
mean(combined_df_redline$redline_grade_D)
### ADDITIONS TO TABLE- CUTRERA AND DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE AND CUTRERA/DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(5) ~ lat + lon))
##### SECOND SET IS 10 KM CONLEY
#table of mori readline prob
summary(feols(redline_grade_D~fixed_mori_set|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
combined_df_redline$any_sicilians=ifelse(combined_df_redline$any_sicilians_1910+combined_df_redline$any_sicilians_1920>0, 1, 0)
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
mean(combined_df_redline$redline_grade_D)
### ADDITIONS TO TABLE- CUTRERA AND DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
### ADDITIONS TO TABLE- PRE-1926 PRESENCE AND CUTRERA/DAMIANI
summary(feols(redline_grade_D~fixed_mori_set+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))
summary(feols(redline_grade_D~fixed_mori_set+any_sicilians+ital_prop+mafia_pre_geo+any_cutrera_or_damiani_maps_sicilians|city, data=combined_df_redline, vcov = conley(10) ~ lat + lon))



#######################
##### TABLE 8 #########
#######################
combined_df_oi$mori_neighbor=ifelse(combined_df_oi$city_hex %in% neighbors, 1, 0)
###### FIRST IS 5KM CONLEY
#Median Income 2016
feols(median_income_2016~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(median_income_2016~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(median_income_2016~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

#College Educated 2010
feols(college_rate_2010~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(college_rate_2010~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(college_rate_2010~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

#College Educated 2000
feols(college_rate_2000~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(college_rate_2000~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(college_rate_2000~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

#Children employed Age 32
feols(work_32_pooled~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(work_32_pooled~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(work_32_pooled~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

#Children employed Age 24
feols(work_24_pooled~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(work_24_pooled~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(work_24_pooled~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

#Same Tract from Childhood to Adulthood
feols(stay_tract_pooled~ mori_neighbor|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(stay_tract_pooled~ mori_neighbor+ital_prop|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)
feols(stay_tract_pooled~ mori_neighbor+ital_prop+any_sicilians|city, data = combined_df_oi, vcov = conley(5) ~ lat + lon)

###### FIRST IS 10KM CONLEY
#Median Income 2016
feols(median_income_2016~ mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(median_income_2016~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(median_income_2016~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)

#College Educated 2010
feols(college_rate_2010~ mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(college_rate_2010~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(college_rate_2010~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)

#College Educated 2000
#Children employed Age 24
feols(work_24_pooled~ mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(work_24_pooled~ ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(work_24_pooled~ any_sicilians+ital_prop+mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)

#Same Tract from Childhood to Adulthood
feols(stay_tract_pooled~ mori_neighbor|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(stay_tract_pooled~ mori_neighbor+ital_prop|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
feols(stay_tract_pooled~ mori_neighbor+ital_prop+any_sicilians|city, data = combined_df_oi, vcov = conley(10) ~ lat + lon)
