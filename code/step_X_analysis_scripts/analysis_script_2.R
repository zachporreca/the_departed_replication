############################################################################################################
###################### ANALYSIS SCRIPT 2: THIS SCRIPT REPLICATES SECONDARY RESULTS #########################
######################                    FIGURES AND TABLES FOR OUTCOMES/FEATURE  #########################
######################                    RELYING ON OPPORTUNITY INSIGHTS AND      #########################
######################                    REDLINING DATA                           #########################
############################################################################################################


############################################################
############## TABLE REPLICATIONS #########################
###########################################################

#######################
##### TABLE 6 #########
#######################
library(fixest)
load("analysis_data/combined_df_redline.rda")

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