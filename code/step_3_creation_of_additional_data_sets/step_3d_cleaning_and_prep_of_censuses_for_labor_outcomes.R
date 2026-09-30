##################################################################################################
#################### STEP 3D: CLEANING OF CENSUS FILES FOR LABOR OUTCOMES        #################
####################          EXTENSION AND CLEANING OF RA GENERATED CODE       ##################
##################################################################################################


library(data.table)
library(dplyr)

###############
#### 1900 #####
###############

data=fread("input_data/step_3_input_data/census_with_labor/1900-raw.csv")
# Create unique identifier - CHANGE "DATA"
data$unique_id <- paste0(data$STATEICP, "_", data$COUNTYICP, "_", data$ENUMDIST)

# Prep data for processing
data$unique_id <- as.character(data$unique_id)
data$OCC1950 <- as.character(data$OCC1950)
data$IND1950 <- as.character(data$IND1950)
data$OCCSCORE <- as.numeric(data$OCCSCORE)
data$SEI <- as.numeric(data$SEI)


#HOUSEHOLDS
#Break down responses to home type by household
hh1900 <- data %>%
  group_by(SERIAL, unique_id) %>%
  summarise(owned = sum(OWNERSHP == 1, na.rm = TRUE),
            owned_free = sum(MORTGAGE == 1, na.rm = TRUE), 
            rent = sum(OWNERSHP == 2, na.rm = TRUE))

#Make ownership/rent variables binary
hh_serial1900 <- hh1900 %>%
  mutate(owned = ifelse(owned >= 1, 1, owned),
         owned_free = ifelse(owned_free >= 1, 1, owned_free),
         rent = ifelse(rent >= 1, 1, rent)
  )

#Group categories by ED and get number of households for each ED
hh_serial1900 <- hh_serial1900 %>%
  group_by(unique_id) %>%
  summarise(owned = sum(owned == 1, na.rm = TRUE),
            owned_free = sum(owned_free == 1, na.rm = TRUE),
            hh = n_distinct(SERIAL),
            rent = sum(rent == 1, na.rm = TRUE)
            
  )

#Combine household data
#Combine household data
hh_id1900 <- data.frame(
  unique_id = hh_serial1900$unique_id,
  hh_1900 = hh_serial1900$hh,
  owned_1900 = hh_serial1900$owned / hh_serial1900$hh,
  owned_free_1900 = hh_serial1900$owned_free / hh_serial1900$hh,
  rent_1900 = hh_serial1900$rent / hh_serial1900$hh
)

#EMPLOYMENT 
#Working age population
data1 <- data[data$AGE >= 14, ]

#Industries
#Recode industries
data1$IND1950[data1$IND1950 %in% c("105", "116", "126")] <- "Agriculture"
data1$IND1950[data1$IND1950 %in% c( "206", "216", "226", "236", "239" )] <- "Mining"
data1$IND1950[data1$IND1950 == "246"] <- "Construction"
data1$IND1950[data1$IND1950 %in% c( "306", "307", "308", "309", "316", "317", "318", "319", 
                                    "326", "336", "337", "338", "346", "347", "348", "356", 
                                    "357","358", "367", "376", "377", "378", "379", "386", 
                                    "387", "388", "399" )] <- "Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "406", "407", "408", "409", "416", "417", "418", "419", "426", "429", 
                                    "436", "437", "438", "439", "446", "448", "449","456", "457", "458", 
                                    "459", "466", "467", "468", "469", "476", "477", "478", "487", "488", 
                                    "489", "499" )] <- "Non-Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "506", "516", "526", "527", "536", "546", "556", "567", "568" )] <- "Transportation"
data1$IND1950[data1$IND1950 %in% c("578", "579")] <- "Telecommunications"
data1$IND1950[data1$IND1950 %in% c( "586", "587", "588", "596", "597", "598" )] <- "Utilities"
data1$IND1950[data1$IND1950 %in% c( "606", "607", "608", "609", "616", "617", "618", "619", "626", "627" )] <- "Wholesale Trade"
data1$IND1950[data1$IND1950 %in% c( "636", "637", "646", "647", "656", "657", "658", "659", "667", "668", "669", 
                                    "679", "686", "687", "688", "689", "696","697", "698", "699" )] <- "Retail Trade"
data1$IND1950[data1$IND1950 %in% c( "716", "726", "736", "746", "756" )] <- "Finance"
data1$IND1950[data1$IND1950 %in% c("806", "807", "808", "816", "817")] <- "Business Services"
data1$IND1950[data1$IND1950 %in% c( "826", "836", "846", "847", "848", "849" )] <- "Personal Services"
data1$IND1950[data1$IND1950 %in% c("856", "857", "858", "859")] <- "Entertainment"
data1$IND1950[data1$IND1950 %in% c( "868", "869", "879", "888", "896", "897", "898", "899" )] <- "Professional and Related"
data1$IND1950[data1$IND1950 %in% c( "906", "916", "926", "936", "946", "976", "979", "980", "982", "983", 
                                    "984", "986", "987" )] <- "Public Administration"
data1$IND1950[data1$IND1950 %in% c("991", "995", "997", "998", "999")] <- "Other"


#Get means and wap and allathat
person1900 <- data1 %>%
  group_by(unique_id) %>%
  summarise(wap_1900 = n(),
            ind_ag_1900 = mean(IND1950 == "Agriculture", na.rm = TRUE),
            ind_min_1900 = mean(IND1950 == "Mining", na.rm = TRUE),
            ind_cons_1900 = mean(IND1950 == "Construction", na.rm = TRUE),
            ind_dman_1900 = mean(IND1950 == "Durable Manufacturing", na.rm = TRUE),
            ind_ndman_1900 = mean(IND1950 == "Non-Durable Manufacturing", na.rm = TRUE),
            ind_trans_1900 = mean(IND1950 == "Transportation", na.rm = TRUE),
            ind_tele_1900 = mean(IND1950 == "Telecommunications", na.rm = TRUE),
            ind_util_1900 = mean(IND1950 == "Utilities", na.rm = TRUE),
            ind_wt_1900 = mean(IND1950 == "Wholesale Trade", na.rm = TRUE),
            ind_rt_1900 = mean(IND1950 == "Retail Trade", na.rm = TRUE),
            ind_fin_1900 = mean(IND1950 == "Finance", na.rm = TRUE),
            ind_bs_1900 = mean(IND1950 == "Business Services", na.rm = TRUE),
            ind_ps_1900 = mean(IND1950 == "Personal Services", na.rm = TRUE),
            ind_ent_1900 = mean(IND1950 == "Entertainment", na.rm = TRUE),
            ind_prof_1900 = mean(IND1950 == "Professional and Related", na.rm = TRUE),
            ind_pa_1900 = mean(IND1950 == "Public Administration", na.rm = TRUE),
            ind_other_1900 = mean(IND1950 == "Other", na.rm = TRUE))



#ECONOMIC
ec1900 <- data1 %>%
  group_by(unique_id) %>%
  summarise(occ_score_1900 = mean(OCCSCORE, na.rm = TRUE),
            duncan1900 = mean(SEI, na.rm = TRUE))


#COMBINE DATA
final <- Reduce(function(x, y) merge(x, y, by = "unique_id"), list(hh_id1900, ec1900, person1900))



#CLEAN
#Make NaN values NA for ease
final$owned_free_1900[is.nan(final$owned_free_1900)] <- NA 
#Make values where homeownership not reported NA
final$owned_1900[final$owned_1900 == 0 & final$rent_1900 == 0] <- NA
final$rent_1900[is.na(final$owned_1900) & final$rent_1900 == 0] <- NA

#SAVE
write.csv(final, "intermediate_outputs/census_with_labor/fullclean_1900.csv")
rm(list = ls())
gc()


###############
#### 1910 #####
###############


data=fread("input_data/step_3_input_data/census_with_labor/1910-raw.csv")
# Create unique identifier - CHANGE "DATA"
data$unique_id <- paste0(data$STATEICP, "_", data$COUNTYICP, "_", data$ENUMDIST)

# Prep data for processing
data$unique_id <- as.character(data$unique_id)
data$OCC1950 <- as.character(data$OCC1950)
data$IND1950 <- as.character(data$IND1950)
data$OCCSCORE <- as.numeric(data$OCCSCORE)
data$SEI <- as.numeric(data$SEI)


#HOUSEHOLDS
#Break down responses to home type by household
hh1910 <- data %>%
  group_by(SERIAL, unique_id) %>%
  summarise(owned = sum(OWNERSHP == 1, na.rm = TRUE),
            owned_free = sum(MORTGAGE == 1, na.rm = TRUE), 
            rent = sum(OWNERSHP == 2, na.rm = TRUE))

#Make ownership/rent variables binary
hh_serial1910 <- hh1910 %>%
  mutate(owned = ifelse(owned >= 1, 1, owned),
         owned_free = ifelse(owned_free >= 1, 1, owned_free),
         rent = ifelse(rent >= 1, 1, rent)
  )

#Group categories by ED and get number of households for each ED
hh_serial1910 <- hh_serial1910 %>%
  group_by(unique_id) %>%
  summarise(owned = sum(owned == 1, na.rm = TRUE),
            owned_free = sum(owned_free == 1, na.rm = TRUE),
            hh = n_distinct(SERIAL),
            rent = sum(rent == 1, na.rm = TRUE)
            
  )

#Combine household data
#Combine household data
hh_id1910 <- data.frame(
  unique_id = hh_serial1910$unique_id,
  hh_1910 = hh_serial1910$hh,
  owned_1910 = hh_serial1910$owned / hh_serial1910$hh,
  owned_free_1910 = hh_serial1910$owned_free / hh_serial1910$hh,
  rent_1910 = hh_serial1910$rent / hh_serial1910$hh
)

#EMPLOYMENT 
#Working age population
data1 <- data[data$AGE >= 14, ]

#General employment measures
emp1910 <- data1 %>%
  group_by(unique_id) %>%
  summarise(lforce_1910 = mean(LABFORCE == 2, na.rm = TRUE),
            emp_1910 = mean(EMPSTAT == 1, na.rm = TRUE),
            unemp_1910 = mean(EMPSTAT == 2, na.rm = TRUE),
            wap_1910 = n()
  )

#Industries
#Recode industries
data1$IND1950[data1$IND1950 %in% c("105", "116", "126")] <- "Agriculture"
data1$IND1950[data1$IND1950 %in% c( "206", "216", "226", "236", "239" )] <- "Mining"
data1$IND1950[data1$IND1950 == "246"] <- "Construction"
data1$IND1950[data1$IND1950 %in% c( "306", "307", "308", "309", "316", "317", "318", "319", 
                                    "326", "336", "337", "338", "346", "347", "348", "356", 
                                    "357","358", "367", "376", "377", "378", "379", "386", 
                                    "387", "388", "399" )] <- "Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "406", "407", "408", "409", "416", "417", "418", "419", "426", "429", 
                                    "436", "437", "438", "439", "446", "448", "449","456", "457", "458", 
                                    "459", "466", "467", "468", "469", "476", "477", "478", "487", "488", 
                                    "489", "499" )] <- "Non-Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "506", "516", "526", "527", "536", "546", "556", "567", "568" )] <- "Transportation"
data1$IND1950[data1$IND1950 %in% c("578", "579")] <- "Telecommunications"
data1$IND1950[data1$IND1950 %in% c( "586", "587", "588", "596", "597", "598" )] <- "Utilities"
data1$IND1950[data1$IND1950 %in% c( "606", "607", "608", "609", "616", "617", "618", "619", "626", "627" )] <- "Wholesale Trade"
data1$IND1950[data1$IND1950 %in% c( "636", "637", "646", "647", "656", "657", "658", "659", "667", "668", "669", 
                                    "679", "686", "687", "688", "689", "696","697", "698", "699" )] <- "Retail Trade"
data1$IND1950[data1$IND1950 %in% c( "716", "726", "736", "746", "756" )] <- "Finance"
data1$IND1950[data1$IND1950 %in% c("806", "807", "808", "816", "817")] <- "Business Services"
data1$IND1950[data1$IND1950 %in% c( "826", "836", "846", "847", "848", "849" )] <- "Personal Services"
data1$IND1950[data1$IND1950 %in% c("856", "857", "858", "859")] <- "Entertainment"
data1$IND1950[data1$IND1950 %in% c( "868", "869", "879", "888", "896", "897", "898", "899" )] <- "Professional and Related"
data1$IND1950[data1$IND1950 %in% c( "906", "916", "926", "936", "946", "976", "979", "980", "982", "983", 
                                    "984", "986", "987" )] <- "Public Administration"
data1$IND1950[data1$IND1950 %in% c("991", "995", "997", "998", "999")] <- "Other"


#Get means and wap and allathat
person1910 <- data1 %>%
  group_by(unique_id) %>%
  summarise(ind_ag_1910 = mean(IND1950 == "Agriculture", na.rm = TRUE),
            ind_min_1910 = mean(IND1950 == "Mining", na.rm = TRUE),
            ind_cons_1910 = mean(IND1950 == "Construction", na.rm = TRUE),
            ind_dman_1910 = mean(IND1950 == "Durable Manufacturing", na.rm = TRUE),
            ind_ndman_1910 = mean(IND1950 == "Non-Durable Manufacturing", na.rm = TRUE),
            ind_trans_1910 = mean(IND1950 == "Transportation", na.rm = TRUE),
            ind_tele_1910 = mean(IND1950 == "Telecommunications", na.rm = TRUE),
            ind_util_1910 = mean(IND1950 == "Utilities", na.rm = TRUE),
            ind_wt_1910 = mean(IND1950 == "Wholesale Trade", na.rm = TRUE),
            ind_rt_1910 = mean(IND1950 == "Retail Trade", na.rm = TRUE),
            ind_fin_1910 = mean(IND1950 == "Finance", na.rm = TRUE),
            ind_bs_1910 = mean(IND1950 == "Business Services", na.rm = TRUE),
            ind_ps_1910 = mean(IND1950 == "Personal Services", na.rm = TRUE),
            ind_ent_1910 = mean(IND1950 == "Entertainment", na.rm = TRUE),
            ind_prof_1910 = mean(IND1950 == "Professional and Related", na.rm = TRUE),
            ind_pa_1910 = mean(IND1950 == "Public Administration", na.rm = TRUE),
            ind_other_1910 = mean(IND1950 == "Other", na.rm = TRUE))



#ECONOMIC
ec1910 <- data1 %>%
  group_by(unique_id) %>%
  summarise(occ_score_1910 = mean(OCCSCORE, na.rm = TRUE),
            duncan1910 = mean(SEI, na.rm = TRUE))


#COMBINE DATA
final <- Reduce(function(x, y) merge(x, y, by = "unique_id"), list(hh_id1910, ec1910, person1910, emp1910))



#CLEAN
#Make NaN values NA for ease
final$owned_free_1910[is.nan(final$owned_free_1910)] <- NA 
#Make values where homeownership not reported NA
final$owned_1910[final$owned_1910 == 0 & final$rent_1910 == 0] <- NA
final$rent_1910[is.na(final$owned_1910) & final$rent_1910 == 0] <- NA

#SAVE
write.csv(final, "intermediate_outputs/census_with_labor/fullclean_1910.csv")
rm(list = ls())
gc()

###############
#### 1920 #####
###############


data=fread("input_data/step_3_input_data/census_with_labor/1920-raw.csv")
# Create unique identifier - CHANGE "DATA"
data$unique_id <- paste0(data$STATEICP, "_", data$COUNTYICP, "_", data$ENUMDIST)

# Prep data for processing
data$unique_id <- as.character(data$unique_id)
data$OCC1950 <- as.character(data$OCC1950)
data$IND1950 <- as.character(data$IND1950)
data$OCCSCORE <- as.numeric(data$OCCSCORE)
data$SEI <- as.numeric(data$SEI)


#HOUSEHOLDS
#Break down responses to home type by household
hh1920 <- data %>%
  group_by(SERIAL, unique_id) %>%
  summarise(owned = sum(OWNERSHP == 1, na.rm = TRUE),
            owned_free = sum(MORTGAGE == 1, na.rm = TRUE), 
            rent = sum(OWNERSHP == 2, na.rm = TRUE))

#Make ownership/rent variables binary
hh_serial1920 <- hh1920 %>%
  mutate(owned = ifelse(owned >= 1, 1, owned),
         owned_free = ifelse(owned_free >= 1, 1, owned_free),
         rent = ifelse(rent >= 1, 1, rent)
  )

#Group categories by ED and get number of households for each ED
hh_serial1920 <- hh_serial1920 %>%
  group_by(unique_id) %>%
  summarise(owned = sum(owned == 1, na.rm = TRUE),
            owned_free = sum(owned_free == 1, na.rm = TRUE),
            hh = n_distinct(SERIAL),
            rent = sum(rent == 1, na.rm = TRUE)
            
  )

#Combine household data
#Combine household data
hh_id1920 <- data.frame(
  unique_id = hh_serial1920$unique_id,
  hh_1920 = hh_serial1920$hh,
  owned_1920 = hh_serial1920$owned / hh_serial1920$hh,
  owned_free_1920 = hh_serial1920$owned_free / hh_serial1920$hh,
  rent_1920 = hh_serial1920$rent / hh_serial1920$hh
)

#EMPLOYMENT 
#Working age population
data1 <- data[data$AGE >= 14, ]

#General employment measures
emp1920 <- data1 %>%
  group_by(unique_id) %>%
  summarise(lforce_1920 = sum(LABFORCE == 2, na.rm = TRUE),
            wap_1920 = n()
  )

#Industries
#Recode industries
data1$IND1950[data1$IND1950 %in% c("105", "116", "126")] <- "Agriculture"
data1$IND1950[data1$IND1950 %in% c( "206", "216", "226", "236", "239" )] <- "Mining"
data1$IND1950[data1$IND1950 == "246"] <- "Construction"
data1$IND1950[data1$IND1950 %in% c( "306", "307", "308", "309", "316", "317", "318", "319", 
                                    "326", "336", "337", "338", "346", "347", "348", "356", 
                                    "357","358", "367", "376", "377", "378", "379", "386", 
                                    "387", "388", "399" )] <- "Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "406", "407", "408", "409", "416", "417", "418", "419", "426", "429", 
                                    "436", "437", "438", "439", "446", "448", "449","456", "457", "458", 
                                    "459", "466", "467", "468", "469", "476", "477", "478", "487", "488", 
                                    "489", "499" )] <- "Non-Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "506", "516", "526", "527", "536", "546", "556", "567", "568" )] <- "Transportation"
data1$IND1950[data1$IND1950 %in% c("578", "579")] <- "Telecommunications"
data1$IND1950[data1$IND1950 %in% c( "586", "587", "588", "596", "597", "598" )] <- "Utilities"
data1$IND1950[data1$IND1950 %in% c( "606", "607", "608", "609", "616", "617", "618", "619", "626", "627" )] <- "Wholesale Trade"
data1$IND1950[data1$IND1950 %in% c( "636", "637", "646", "647", "656", "657", "658", "659", "667", "668", "669", 
                                    "679", "686", "687", "688", "689", "696","697", "698", "699" )] <- "Retail Trade"
data1$IND1950[data1$IND1950 %in% c( "716", "726", "736", "746", "756" )] <- "Finance"
data1$IND1950[data1$IND1950 %in% c("806", "807", "808", "816", "817")] <- "Business Services"
data1$IND1950[data1$IND1950 %in% c( "826", "836", "846", "847", "848", "849" )] <- "Personal Services"
data1$IND1950[data1$IND1950 %in% c("856", "857", "858", "859")] <- "Entertainment"
data1$IND1950[data1$IND1950 %in% c( "868", "869", "879", "888", "896", "897", "898", "899" )] <- "Professional and Related"
data1$IND1950[data1$IND1950 %in% c( "906", "916", "926", "936", "946", "976", "979", "980", "982", "983", 
                                    "984", "986", "987" )] <- "Public Administration"
data1$IND1950[data1$IND1950 %in% c("991", "995", "997", "998", "999")] <- "Other"


#Get means and wap and allathat
person1920 <- data1 %>%
  group_by(unique_id) %>%
  summarise(ind_ag_1920 = mean(IND1950 == "Agriculture", na.rm = TRUE),
            ind_min_1920 = mean(IND1950 == "Mining", na.rm = TRUE),
            ind_cons_1920 = mean(IND1950 == "Construction", na.rm = TRUE),
            ind_dman_1920 = mean(IND1950 == "Durable Manufacturing", na.rm = TRUE),
            ind_ndman_1920 = mean(IND1950 == "Non-Durable Manufacturing", na.rm = TRUE),
            ind_trans_1920 = mean(IND1950 == "Transportation", na.rm = TRUE),
            ind_tele_1920 = mean(IND1950 == "Telecommunications", na.rm = TRUE),
            ind_util_1920 = mean(IND1950 == "Utilities", na.rm = TRUE),
            ind_wt_1920 = mean(IND1950 == "Wholesale Trade", na.rm = TRUE),
            ind_rt_1920 = mean(IND1950 == "Retail Trade", na.rm = TRUE),
            ind_fin_1920 = mean(IND1950 == "Finance", na.rm = TRUE),
            ind_bs_1920 = mean(IND1950 == "Business Services", na.rm = TRUE),
            ind_ps_1920 = mean(IND1950 == "Personal Services", na.rm = TRUE),
            ind_ent_1920 = mean(IND1950 == "Entertainment", na.rm = TRUE),
            ind_prof_1920 = mean(IND1950 == "Professional and Related", na.rm = TRUE),
            ind_pa_1920 = mean(IND1950 == "Public Administration", na.rm = TRUE),
            ind_other_1920 = mean(IND1950 == "Other", na.rm = TRUE))



#ECONOMIC
ec1920 <- data1 %>%
  group_by(unique_id) %>%
  summarise(occ_score_1920 = mean(OCCSCORE, na.rm = TRUE),
            duncan1920 = mean(SEI, na.rm = TRUE))


#COMBINE DATA
final <- Reduce(function(x, y) merge(x, y, by = "unique_id"), list(hh_id1920, ec1920, person1920, emp1920))



#CLEAN
#Make NaN values NA for ease
final$owned_free_1920[is.nan(final$owned_free_1920)] <- NA 
#Make values where homeownership not reported NA
final$owned_1920[final$owned_1920 == 0 & final$rent_1920 == 0] <- NA
final$rent_1920[is.na(final$owned_1920) & final$rent_1920 == 0] <- NA

#SAVE
write.csv(final, "intermediate_outputs/census_with_labor/fullclean_1920.csv")
rm(list = ls())
gc()

###############
#### 1930 #####
###############


data=fread("input_data/step_3_input_data/census_with_labor/1930-raw.csv")
# Create unique identifier - CHANGE "DATA"
data$unique_id <- paste0(data$STATEICP, "_", data$COUNTYICP, "_", data$ENUMDIST)

# Prep data for processing
data$unique_id <- as.character(data$unique_id)
data$OCC1950 <- as.character(data$OCC1950)
data$IND1950 <- as.character(data$IND1950)
data$OCCSCORE <- as.numeric(data$OCCSCORE)
data$SEI <- as.numeric(data$SEI)


#HOUSEHOLDS
#Break down responses to home type by household
hh1930 <- data %>%
  group_by(SERIAL, unique_id) %>%
  summarise(owned = sum(OWNERSHP == 1, na.rm = TRUE),
            rent = sum(OWNERSHP == 2, na.rm = TRUE))

#Make ownership/rent variables binary
hh_serial1930 <- hh1930 %>%
  mutate(owned = ifelse(owned >= 1, 1, owned),
         rent = ifelse(rent >= 1, 1, rent)
  )

#Group categories by ED and get number of households for each ED
hh_serial1930 <- hh_serial1930 %>%
  group_by(unique_id) %>%
  summarise(owned = sum(owned == 1, na.rm = TRUE),
            hh = n_distinct(SERIAL),
            rent = sum(rent == 1, na.rm = TRUE)
            
  )

#Combine household data
#Combine household data
hh_id1930 <- data.frame(
  unique_id = hh_serial1930$unique_id,
  hh_1930 = hh_serial1930$hh,
  owned_1930 = hh_serial1930$owned / hh_serial1930$hh,
  rent_1930 = hh_serial1930$rent / hh_serial1930$hh
)

#EMPLOYMENT 
#Working age population
data1 <- data[data$AGE >= 14, ]

#General employment measures
emp1930 <- data1 %>%
  group_by(unique_id) %>%
  summarise(lforce_1930 = mean(LABFORCE == 2, na.rm = TRUE),
            emp_1930 = sum(EMPSTAT == 1, na.rm = TRUE) / sum(LABFORCE == 2, na.rm = TRUE),
            unemp_1930 = sum(EMPSTAT == 2, na.rm = TRUE) / sum(LABFORCE == 2, na.rm = TRUE),
            wap_1930 = n()
  )

#Industries
#Recode industries
data1$IND1950[data1$IND1950 %in% c("105", "116", "126")] <- "Agriculture"
data1$IND1950[data1$IND1950 %in% c( "206", "216", "226", "236", "239" )] <- "Mining"
data1$IND1950[data1$IND1950 == "246"] <- "Construction"
data1$IND1950[data1$IND1950 %in% c( "306", "307", "308", "309", "316", "317", "318", "319", 
                                    "326", "336", "337", "338", "346", "347", "348", "356", 
                                    "357","358", "367", "376", "377", "378", "379", "386", 
                                    "387", "388", "399" )] <- "Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "406", "407", "408", "409", "416", "417", "418", "419", "426", "429", 
                                    "436", "437", "438", "439", "446", "448", "449","456", "457", "458", 
                                    "459", "466", "467", "468", "469", "476", "477", "478", "487", "488", 
                                    "489", "499" )] <- "Non-Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "506", "516", "526", "527", "536", "546", "556", "567", "568" )] <- "Transportation"
data1$IND1950[data1$IND1950 %in% c("578", "579")] <- "Telecommunications"
data1$IND1950[data1$IND1950 %in% c( "586", "587", "588", "596", "597", "598" )] <- "Utilities"
data1$IND1950[data1$IND1950 %in% c( "606", "607", "608", "609", "616", "617", "618", "619", "626", "627" )] <- "Wholesale Trade"
data1$IND1950[data1$IND1950 %in% c( "636", "637", "646", "647", "656", "657", "658", "659", "667", "668", "669", 
                                    "679", "686", "687", "688", "689", "696","697", "698", "699" )] <- "Retail Trade"
data1$IND1950[data1$IND1950 %in% c( "716", "726", "736", "746", "756" )] <- "Finance"
data1$IND1950[data1$IND1950 %in% c("806", "807", "808", "816", "817")] <- "Business Services"
data1$IND1950[data1$IND1950 %in% c( "826", "836", "846", "847", "848", "849" )] <- "Personal Services"
data1$IND1950[data1$IND1950 %in% c("856", "857", "858", "859")] <- "Entertainment"
data1$IND1950[data1$IND1950 %in% c( "868", "869", "879", "888", "896", "897", "898", "899" )] <- "Professional and Related"
data1$IND1950[data1$IND1950 %in% c( "906", "916", "926", "936", "946", "976", "979", "980", "982", "983", 
                                    "984", "986", "987" )] <- "Public Administration"
data1$IND1950[data1$IND1950 %in% c("991", "995", "997", "998", "999")] <- "Other"


#Get means and wap and allathat
person1930 <- data1 %>%
  group_by(unique_id) %>%
  summarise(ind_ag_1930 = mean(IND1950 == "Agriculture", na.rm = TRUE),
            ind_min_1930 = mean(IND1950 == "Mining", na.rm = TRUE),
            ind_cons_1930 = mean(IND1950 == "Construction", na.rm = TRUE),
            ind_dman_1930 = mean(IND1950 == "Durable Manufacturing", na.rm = TRUE),
            ind_ndman_1930 = mean(IND1950 == "Non-Durable Manufacturing", na.rm = TRUE),
            ind_trans_1930 = mean(IND1950 == "Transportation", na.rm = TRUE),
            ind_tele_1930 = mean(IND1950 == "Telecommunications", na.rm = TRUE),
            ind_util_1930 = mean(IND1950 == "Utilities", na.rm = TRUE),
            ind_wt_1930 = mean(IND1950 == "Wholesale Trade", na.rm = TRUE),
            ind_rt_1930 = mean(IND1950 == "Retail Trade", na.rm = TRUE),
            ind_fin_1930 = mean(IND1950 == "Finance", na.rm = TRUE),
            ind_bs_1930 = mean(IND1950 == "Business Services", na.rm = TRUE),
            ind_ps_1930 = mean(IND1950 == "Personal Services", na.rm = TRUE),
            ind_ent_1930 = mean(IND1950 == "Entertainment", na.rm = TRUE),
            ind_prof_1930 = mean(IND1950 == "Professional and Related", na.rm = TRUE),
            ind_pa_1930 = mean(IND1950 == "Public Administration", na.rm = TRUE),
            ind_other_1930 = mean(IND1950 == "Other", na.rm = TRUE))



#ECONOMIC
ec1930 <- data1 %>%
  group_by(unique_id) %>%
  summarise(occ_score_1930 = mean(OCCSCORE, na.rm = TRUE),
            duncan1930 = mean(SEI, na.rm = TRUE))


#COMBINE DATA
final <- Reduce(function(x, y) merge(x, y, by = "unique_id"), list(hh_id1930, ec1930, person1930, emp1930))



#CLEAN
#Make NaN values NA for ease
#Make values where homeownership not reported NA
final$owned_1930[final$owned_1930 == 0 & final$rent_1930 == 0] <- NA
final$rent_1930[is.na(final$owned_1930) & final$rent_1930 == 0] <- NA

#SAVE
write.csv(final, "intermediate_outputs/census_with_labor/fullclean_1930.csv")
rm(list = ls())
gc()

###############
#### 1940 #####
###############


data=fread("input_data/step_3_input_data/census_with_labor/1940-raw.csv")
# Create unique identifier - CHANGE "DATA"
data$unique_id <- paste0(data$STATEICP, "_", data$COUNTYICP, "_", data$ENUMDIST)

# Prep data for processing
data$unique_id <- as.character(data$unique_id)
data$OCC1950 <- as.character(data$OCC1950)
data$IND1950 <- as.character(data$IND1950)
data$OCCSCORE <- as.numeric(data$OCCSCORE)
data$SEI <- as.numeric(data$SEI)


#HOUSEHOLDS
#Break down responses to home type by household
hh1940 <- data %>%
  group_by(SERIAL, unique_id) %>%
  summarise(owned = sum(OWNERSHP == 1, na.rm = TRUE),
            rent = sum(OWNERSHP == 2, na.rm = TRUE))

#Make ownership/rent variables binary
hh_serial1940 <- hh1940 %>%
  mutate(owned = ifelse(owned >= 1, 1, owned),
         rent = ifelse(rent >= 1, 1, rent)
  )

#Group categories by ED and get number of households for each ED
hh_serial1940 <- hh_serial1940 %>%
  group_by(unique_id) %>%
  summarise(owned = sum(owned == 1, na.rm = TRUE),
            hh = n_distinct(SERIAL),
            rent = sum(rent == 1, na.rm = TRUE)
            
  )

#Combine household data
#Combine household data
hh_id1940 <- data.frame(
  unique_id = hh_serial1940$unique_id,
  hh_1940 = hh_serial1940$hh,
  owned_1940 = hh_serial1940$owned / hh_serial1940$hh,
  rent_1940 = hh_serial1940$rent / hh_serial1940$hh
)

#EMPLOYMENT 
#Working age population
data1 <- data[data$AGE >= 14, ]

#General employment measures
emp1940 <- data1 %>%
  group_by(unique_id) %>%
  summarise(lforce_1940 = sum(LABFORCE == 2, na.rm = TRUE),
            emp_1940 = sum(EMPSTAT == 1, na.rm = TRUE),
            unemp_1940 = sum(EMPSTAT == 2, na.rm = TRUE),
            wap_1940 = n()
  )

#Industries
#Recode industries
data1$IND1950[data1$IND1950 %in% c("105", "116", "126")] <- "Agriculture"
data1$IND1950[data1$IND1950 %in% c( "206", "216", "226", "236", "239" )] <- "Mining"
data1$IND1950[data1$IND1950 == "246"] <- "Construction"
data1$IND1950[data1$IND1950 %in% c( "306", "307", "308", "309", "316", "317", "318", "319", 
                                    "326", "336", "337", "338", "346", "347", "348", "356", 
                                    "357","358", "367", "376", "377", "378", "379", "386", 
                                    "387", "388", "399" )] <- "Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "406", "407", "408", "409", "416", "417", "418", "419", "426", "429", 
                                    "436", "437", "438", "439", "446", "448", "449","456", "457", "458", 
                                    "459", "466", "467", "468", "469", "476", "477", "478", "487", "488", 
                                    "489", "499" )] <- "Non-Durable Manufacturing"
data1$IND1950[data1$IND1950 %in% c( "506", "516", "526", "527", "536", "546", "556", "567", "568" )] <- "Transportation"
data1$IND1950[data1$IND1950 %in% c("578", "579")] <- "Telecommunications"
data1$IND1950[data1$IND1950 %in% c( "586", "587", "588", "596", "597", "598" )] <- "Utilities"
data1$IND1950[data1$IND1950 %in% c( "606", "607", "608", "609", "616", "617", "618", "619", "626", "627" )] <- "Wholesale Trade"
data1$IND1950[data1$IND1950 %in% c( "636", "637", "646", "647", "656", "657", "658", "659", "667", "668", "669", 
                                    "679", "686", "687", "688", "689", "696","697", "698", "699" )] <- "Retail Trade"
data1$IND1950[data1$IND1950 %in% c( "716", "726", "736", "746", "756" )] <- "Finance"
data1$IND1950[data1$IND1950 %in% c("806", "807", "808", "816", "817")] <- "Business Services"
data1$IND1950[data1$IND1950 %in% c( "826", "836", "846", "847", "848", "849" )] <- "Personal Services"
data1$IND1950[data1$IND1950 %in% c("856", "857", "858", "859")] <- "Entertainment"
data1$IND1950[data1$IND1950 %in% c( "868", "869", "879", "888", "896", "897", "898", "899" )] <- "Professional and Related"
data1$IND1950[data1$IND1950 %in% c( "906", "916", "926", "936", "946", "976", "979", "980", "982", "983", 
                                    "984", "986", "987" )] <- "Public Administration"
data1$IND1950[data1$IND1950 %in% c("991", "995", "997", "998", "999")] <- "Other"


#Get means and wap and allathat
person1940 <- data1 %>%
  group_by(unique_id) %>%
  summarise(ind_ag_1940 = mean(IND1950 == "Agriculture", na.rm = TRUE),
            ind_min_1940 = mean(IND1950 == "Mining", na.rm = TRUE),
            ind_cons_1940 = mean(IND1950 == "Construction", na.rm = TRUE),
            ind_dman_1940 = mean(IND1950 == "Durable Manufacturing", na.rm = TRUE),
            ind_ndman_1940 = mean(IND1950 == "Non-Durable Manufacturing", na.rm = TRUE),
            ind_trans_1940 = mean(IND1950 == "Transportation", na.rm = TRUE),
            ind_tele_1940 = mean(IND1950 == "Telecommunications", na.rm = TRUE),
            ind_util_1940 = mean(IND1950 == "Utilities", na.rm = TRUE),
            ind_wt_1940 = mean(IND1950 == "Wholesale Trade", na.rm = TRUE),
            ind_rt_1940 = mean(IND1950 == "Retail Trade", na.rm = TRUE),
            ind_fin_1940 = mean(IND1950 == "Finance", na.rm = TRUE),
            ind_bs_1940 = mean(IND1950 == "Business Services", na.rm = TRUE),
            ind_ps_1940 = mean(IND1950 == "Personal Services", na.rm = TRUE),
            ind_ent_1940 = mean(IND1950 == "Entertainment", na.rm = TRUE),
            ind_prof_1940 = mean(IND1950 == "Professional and Related", na.rm = TRUE),
            ind_pa_1940 = mean(IND1950 == "Public Administration", na.rm = TRUE),
            ind_other_1940 = mean(IND1950 == "Other", na.rm = TRUE))



#ECONOMIC
ec1940 <- data1 %>%
  group_by(unique_id) %>%
  summarise(occ_score_1940 = mean(OCCSCORE, na.rm = TRUE),
            duncan1940 = mean(SEI, na.rm = TRUE))


#COMBINE DATA
final <- Reduce(function(x, y) merge(x, y, by = "unique_id"), list(hh_id1940, ec1940, person1940, emp1940))



#CLEAN
#Make NaN values NA for ease
#Make values where homeownership not reported NA
final$owned_1940[final$owned_1940 == 0 & final$rent_1940 == 0] <- NA
final$rent_1940[is.na(final$owned_1940) & final$rent_1940 == 0] <- NA

#SAVE
write.csv(final, "intermediate_outputs/census_with_labor/fullclean_1940.csv")
rm(list = ls())
gc()