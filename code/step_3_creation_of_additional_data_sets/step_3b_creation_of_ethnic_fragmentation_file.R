#################################################################################################################################
#################### STEP 3B: CONSTRUCTION OF ENUMERATION DISTRICT LEVEL ETHNIC FRAGMENTATION INDEXES           #################
####################          THIS TAKES FULL COUNT  CENSUSES AS INPUTS AND COMPUTES SHARES BASED ON            #################
####################          ETHNICITY ANDNATIONAL BACKGROUND                                                  #################
#################################################################################################################################

library(haven)

#1900
census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
library(memisc)
census_1900$BPL=haven::zap_label(census_1900$BPL)
census_1900$BPL=as.numeric(census_1900$BPL)
census_1900=as.data.frame(census_1900)
census_1900$birthplace <- ifelse(census_1900$BPL < 100, "USA",
                                 ifelse(census_1900$BPL >= 200 & census_1900$BPL <= 300, "Latin America/ Caribbean",
                                        ifelse(census_1900$BPL >= 400 & census_1900$BPL <= 405, "Scandinavia",
                                               ifelse(census_1900$BPL >= 410 & census_1900$BPL <= 413, "United Kingdom",
                                                      ifelse(census_1900$BPL == 414, "Ireland",
                                                             ifelse(census_1900$BPL >= 420 & census_1900$BPL <= 429, "Other Western Europe",
                                                                    ifelse(census_1900$BPL >= 430 & census_1900$BPL <= 440 & census_1900$BPL != 434, "Other Southern Europe",
                                                                           ifelse(census_1900$BPL == 434, "Italy",
                                                                                  ifelse(census_1900$BPL >= 450 & census_1900$BPL <= 459, "Central/Eastern Europe",
                                                                                         ifelse(census_1900$BPL >= 460 & census_1900$BPL <= 465, "Russia/Empire",
                                                                                                ifelse(census_1900$BPL == 499 | census_1900$BPL == 419, "Other Europe",
                                                                                                       ifelse(census_1900$BPL >= 500 & census_1900$BPL <= 509, "East Asia",
                                                                                                              ifelse(census_1900$BPL >= 510 & census_1900$BPL <= 519, "Southeast Asia",
                                                                                                                     ifelse((census_1900$BPL >= 520 & census_1900$BPL <= 524) | census_1900$BPL == 548 | census_1900$BPL == 550, "India and Southern Asia",
                                                                                                                            ifelse((census_1900$BPL >= 530 & census_1900$BPL <= 547) | census_1900$BPL == 549, "Middle East",
                                                                                                                                   ifelse(census_1900$BPL == 599, "Other Asia",
                                                                                                                                          ifelse(census_1900$BPL >= 600 & census_1900$BPL <= 699, "Africa",
                                                                                                                                                 ifelse(census_1900$BPL >= 700 & census_1900$BPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                        ifelse(census_1900$BPL==150, "Canada", "Other")))))))))))))))))))




census_1900$FBPL=haven::zap_label(census_1900$FBPL)
census_1900$FBPL=as.numeric(census_1900$FBPL)
census_1900$father_birthplace <- ifelse(census_1900$FBPL < 100, "USA",
                                        ifelse(census_1900$FBPL >= 200 & census_1900$FBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1900$FBPL >= 400 & census_1900$FBPL <= 405, "Scandinavia",
                                                      ifelse(census_1900$FBPL >= 410 & census_1900$FBPL <= 413, "United Kingdom",
                                                             ifelse(census_1900$FBPL == 414, "Ireland",
                                                                    ifelse(census_1900$FBPL >= 420 & census_1900$FBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1900$FBPL >= 430 & census_1900$FBPL <= 440 & census_1900$FBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1900$FBPL == 434, "Italy",
                                                                                         ifelse(census_1900$FBPL >= 450 & census_1900$FBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1900$FBPL >= 460 & census_1900$FBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1900$FBPL == 499 | census_1900$FBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1900$FBPL >= 500 & census_1900$FBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1900$FBPL >= 510 & census_1900$FBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1900$FBPL >= 520 & census_1900$FBPL <= 524) | census_1900$FBPL == 548 | census_1900$FBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1900$FBPL >= 530 & census_1900$FBPL <= 547) | census_1900$FBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1900$FBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1900$FBPL >= 600 & census_1900$FBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1900$FBPL >= 700 & census_1900$FBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1900$FBPL==150, "Canada", "Other")))))))))))))))))))


census_1900$MBPL=haven::zap_label(census_1900$MBPL)
census_1900$MBPL=as.numeric(census_1900$MBPL)
census_1900$mother_birthplace <- ifelse(census_1900$MBPL < 100, "USA",
                                        ifelse(census_1900$MBPL >= 200 & census_1900$MBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1900$MBPL >= 400 & census_1900$MBPL <= 405, "Scandinavia",
                                                      ifelse(census_1900$MBPL >= 410 & census_1900$MBPL <= 413, "United Kingdom",
                                                             ifelse(census_1900$MBPL == 414, "Ireland",
                                                                    ifelse(census_1900$MBPL >= 420 & census_1900$MBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1900$MBPL >= 430 & census_1900$MBPL <= 440 & census_1900$MBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1900$MBPL == 434, "Italy",
                                                                                         ifelse(census_1900$MBPL >= 450 & census_1900$MBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1900$MBPL >= 460 & census_1900$MBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1900$MBPL == 499 | census_1900$MBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1900$MBPL >= 500 & census_1900$MBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1900$MBPL >= 510 & census_1900$MBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1900$MBPL >= 520 & census_1900$MBPL <= 524) | census_1900$MBPL == 548 | census_1900$MBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1900$MBPL >= 530 & census_1900$MBPL <= 547) | census_1900$MBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1900$MBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1900$MBPL >= 600 & census_1900$MBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1900$MBPL >= 700 & census_1900$MBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1900$MBPL==150, "Canada", "Other")))))))))))))))))))
census_1900$enum_dist=paste0(census_1900$STATEICP, "_", census_1900$COUNTYICP, "_", census_1900$ENUMDIST)
ethnic_frag_1900=matrix(ncol=(2+length(unique(census_1900$father_birthplace))), nrow=length(unique(census_1900$enum_dist)))
ethnic_frag_1900=as.data.frame(ethnic_frag_1900)
colnames=c("enum_dist", unique(census_1900$father_birthplace), "total_pop")
colnames(ethnic_frag_1900)=colnames
ethnic_frag_1900[,1]=unique(census_1900$enum_dist)
timeNow <- Sys.time()
for (i in 1:nrow(ethnic_frag_1900)){
  tmp=census_1900[which(census_1900$enum_dist==ethnic_frag_1900[i,1]),]
  for (j in 2:(length(colnames)-1)){
    ethnic_frag_1900[i,j]=nrow(tmp[which(tmp[,"father_birthplace"]==colnames[j]),])
  }
  ethnic_frag_1900[i,length(colnames)]=nrow(tmp)
  cat("\r", round(i*100/nrow(ethnic_frag_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
save(ethnic_frag_1900, file="intermediate_outputs/ethnic_frag_1900.rda")
rm(census_1900)
gc()

#1910

census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
library(memisc)
census_1910$BPL=haven::zap_label(census_1910$BPL)
census_1910$BPL=as.numeric(census_1910$BPL)
census_1910=as.data.frame(census_1910)
census_1910$birthplace <- ifelse(census_1910$BPL < 100, "USA",
                                 ifelse(census_1910$BPL >= 200 & census_1910$BPL <= 300, "Latin America/ Caribbean",
                                        ifelse(census_1910$BPL >= 400 & census_1910$BPL <= 405, "Scandinavia",
                                               ifelse(census_1910$BPL >= 410 & census_1910$BPL <= 413, "United Kingdom",
                                                      ifelse(census_1910$BPL == 414, "Ireland",
                                                             ifelse(census_1910$BPL >= 420 & census_1910$BPL <= 429, "Other Western Europe",
                                                                    ifelse(census_1910$BPL >= 430 & census_1910$BPL <= 440 & census_1910$BPL != 434, "Other Southern Europe",
                                                                           ifelse(census_1910$BPL == 434, "Italy",
                                                                                  ifelse(census_1910$BPL >= 450 & census_1910$BPL <= 459, "Central/Eastern Europe",
                                                                                         ifelse(census_1910$BPL >= 460 & census_1910$BPL <= 465, "Russia/Empire",
                                                                                                ifelse(census_1910$BPL == 499 | census_1910$BPL == 419, "Other Europe",
                                                                                                       ifelse(census_1910$BPL >= 500 & census_1910$BPL <= 509, "East Asia",
                                                                                                              ifelse(census_1910$BPL >= 510 & census_1910$BPL <= 519, "Southeast Asia",
                                                                                                                     ifelse((census_1910$BPL >= 520 & census_1910$BPL <= 524) | census_1910$BPL == 548 | census_1910$BPL == 550, "India and Southern Asia",
                                                                                                                            ifelse((census_1910$BPL >= 530 & census_1910$BPL <= 547) | census_1910$BPL == 549, "Middle East",
                                                                                                                                   ifelse(census_1910$BPL == 599, "Other Asia",
                                                                                                                                          ifelse(census_1910$BPL >= 600 & census_1910$BPL <= 699, "Africa",
                                                                                                                                                 ifelse(census_1910$BPL >= 700 & census_1910$BPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                        ifelse(census_1910$BPL==150, "Canada", "Other")))))))))))))))))))




census_1910$FBPL=haven::zap_label(census_1910$FBPL)
census_1910$FBPL=as.numeric(census_1910$FBPL)
census_1910$father_birthplace <- ifelse(census_1910$FBPL < 100, "USA",
                                        ifelse(census_1910$FBPL >= 200 & census_1910$FBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1910$FBPL >= 400 & census_1910$FBPL <= 405, "Scandinavia",
                                                      ifelse(census_1910$FBPL >= 410 & census_1910$FBPL <= 413, "United Kingdom",
                                                             ifelse(census_1910$FBPL == 414, "Ireland",
                                                                    ifelse(census_1910$FBPL >= 420 & census_1910$FBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1910$FBPL >= 430 & census_1910$FBPL <= 440 & census_1910$FBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1910$FBPL == 434, "Italy",
                                                                                         ifelse(census_1910$FBPL >= 450 & census_1910$FBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1910$FBPL >= 460 & census_1910$FBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1910$FBPL == 499 | census_1910$FBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1910$FBPL >= 500 & census_1910$FBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1910$FBPL >= 510 & census_1910$FBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1910$FBPL >= 520 & census_1910$FBPL <= 524) | census_1910$FBPL == 548 | census_1910$FBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1910$FBPL >= 530 & census_1910$FBPL <= 547) | census_1910$FBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1910$FBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1910$FBPL >= 600 & census_1910$FBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1910$FBPL >= 700 & census_1910$FBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1910$FBPL==150, "Canada", "Other")))))))))))))))))))


census_1910$MBPL=haven::zap_label(census_1910$MBPL)
census_1910$MBPL=as.numeric(census_1910$MBPL)
census_1910$mother_birthplace <- ifelse(census_1910$MBPL < 100, "USA",
                                        ifelse(census_1910$MBPL >= 200 & census_1910$MBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1910$MBPL >= 400 & census_1910$MBPL <= 405, "Scandinavia",
                                                      ifelse(census_1910$MBPL >= 410 & census_1910$MBPL <= 413, "United Kingdom",
                                                             ifelse(census_1910$MBPL == 414, "Ireland",
                                                                    ifelse(census_1910$MBPL >= 420 & census_1910$MBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1910$MBPL >= 430 & census_1910$MBPL <= 440 & census_1910$MBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1910$MBPL == 434, "Italy",
                                                                                         ifelse(census_1910$MBPL >= 450 & census_1910$MBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1910$MBPL >= 460 & census_1910$MBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1910$MBPL == 499 | census_1910$MBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1910$MBPL >= 500 & census_1910$MBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1910$MBPL >= 510 & census_1910$MBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1910$MBPL >= 520 & census_1910$MBPL <= 524) | census_1910$MBPL == 548 | census_1910$MBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1910$MBPL >= 530 & census_1910$MBPL <= 547) | census_1910$MBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1910$MBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1910$MBPL >= 600 & census_1910$MBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1910$MBPL >= 700 & census_1910$MBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1910$MBPL==150, "Canada", "Other")))))))))))))))))))
census_1910$enum_dist=paste0(census_1910$STATEICP, "_", census_1910$COUNTYICP, "_", census_1910$ENUMDIST)
ethnic_frag_1910=matrix(ncol=(2+length(unique(census_1910$father_birthplace))), nrow=length(unique(census_1910$enum_dist)))
ethnic_frag_1910=as.data.frame(ethnic_frag_1910)
colnames=c("enum_dist", unique(census_1910$father_birthplace), "total_pop")
colnames(ethnic_frag_1910)=colnames
ethnic_frag_1910[,1]=unique(census_1910$enum_dist)
timeNow <- Sys.time()
for (i in 1:nrow(ethnic_frag_1910)){
  tmp=census_1910[which(census_1910$enum_dist==ethnic_frag_1910[i,1]),]
  for (j in 2:(length(colnames)-1)){
    ethnic_frag_1910[i,j]=nrow(tmp[which(tmp[,"father_birthplace"]==colnames[j]),])
  }
  ethnic_frag_1910[i,length(colnames)]=nrow(tmp)
  cat("\r", round(i*100/nrow(ethnic_frag_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
save(ethnic_frag_1910, file="intermediate_outputs/ethnic_frag_1910.rda")
rm(census_1910)
gc()

#1920

census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
library(memisc)
census_1920$BPL=haven::zap_label(census_1920$BPL)
census_1920$BPL=as.numeric(census_1920$BPL)
census_1920=as.data.frame(census_1920)
census_1920$birthplace <- ifelse(census_1920$BPL < 100, "USA",
                                 ifelse(census_1920$BPL >= 200 & census_1920$BPL <= 300, "Latin America/ Caribbean",
                                        ifelse(census_1920$BPL >= 400 & census_1920$BPL <= 405, "Scandinavia",
                                               ifelse(census_1920$BPL >= 410 & census_1920$BPL <= 413, "United Kingdom",
                                                      ifelse(census_1920$BPL == 414, "Ireland",
                                                             ifelse(census_1920$BPL >= 420 & census_1920$BPL <= 429, "Other Western Europe",
                                                                    ifelse(census_1920$BPL >= 430 & census_1920$BPL <= 440 & census_1920$BPL != 434, "Other Southern Europe",
                                                                           ifelse(census_1920$BPL == 434, "Italy",
                                                                                  ifelse(census_1920$BPL >= 450 & census_1920$BPL <= 459, "Central/Eastern Europe",
                                                                                         ifelse(census_1920$BPL >= 460 & census_1920$BPL <= 465, "Russia/Empire",
                                                                                                ifelse(census_1920$BPL == 499 | census_1920$BPL == 419, "Other Europe",
                                                                                                       ifelse(census_1920$BPL >= 500 & census_1920$BPL <= 509, "East Asia",
                                                                                                              ifelse(census_1920$BPL >= 510 & census_1920$BPL <= 519, "Southeast Asia",
                                                                                                                     ifelse((census_1920$BPL >= 520 & census_1920$BPL <= 524) | census_1920$BPL == 548 | census_1920$BPL == 550, "India and Southern Asia",
                                                                                                                            ifelse((census_1920$BPL >= 530 & census_1920$BPL <= 547) | census_1920$BPL == 549, "Middle East",
                                                                                                                                   ifelse(census_1920$BPL == 599, "Other Asia",
                                                                                                                                          ifelse(census_1920$BPL >= 600 & census_1920$BPL <= 699, "Africa",
                                                                                                                                                 ifelse(census_1920$BPL >= 700 & census_1920$BPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                        ifelse(census_1920$BPL==150, "Canada", "Other")))))))))))))))))))




census_1920$FBPL=haven::zap_label(census_1920$FBPL)
census_1920$FBPL=as.numeric(census_1920$FBPL)
census_1920$father_birthplace <- ifelse(census_1920$FBPL < 100, "USA",
                                        ifelse(census_1920$FBPL >= 200 & census_1920$FBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1920$FBPL >= 400 & census_1920$FBPL <= 405, "Scandinavia",
                                                      ifelse(census_1920$FBPL >= 410 & census_1920$FBPL <= 413, "United Kingdom",
                                                             ifelse(census_1920$FBPL == 414, "Ireland",
                                                                    ifelse(census_1920$FBPL >= 420 & census_1920$FBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1920$FBPL >= 430 & census_1920$FBPL <= 440 & census_1920$FBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1920$FBPL == 434, "Italy",
                                                                                         ifelse(census_1920$FBPL >= 450 & census_1920$FBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1920$FBPL >= 460 & census_1920$FBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1920$FBPL == 499 | census_1920$FBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1920$FBPL >= 500 & census_1920$FBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1920$FBPL >= 510 & census_1920$FBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1920$FBPL >= 520 & census_1920$FBPL <= 524) | census_1920$FBPL == 548 | census_1920$FBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1920$FBPL >= 530 & census_1920$FBPL <= 547) | census_1920$FBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1920$FBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1920$FBPL >= 600 & census_1920$FBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1920$FBPL >= 700 & census_1920$FBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1920$FBPL==150, "Canada", "Other")))))))))))))))))))


census_1920$MBPL=haven::zap_label(census_1920$MBPL)
census_1920$MBPL=as.numeric(census_1920$MBPL)
census_1920$mother_birthplace <- ifelse(census_1920$MBPL < 100, "USA",
                                        ifelse(census_1920$MBPL >= 200 & census_1920$MBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1920$MBPL >= 400 & census_1920$MBPL <= 405, "Scandinavia",
                                                      ifelse(census_1920$MBPL >= 410 & census_1920$MBPL <= 413, "United Kingdom",
                                                             ifelse(census_1920$MBPL == 414, "Ireland",
                                                                    ifelse(census_1920$MBPL >= 420 & census_1920$MBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1920$MBPL >= 430 & census_1920$MBPL <= 440 & census_1920$MBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1920$MBPL == 434, "Italy",
                                                                                         ifelse(census_1920$MBPL >= 450 & census_1920$MBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1920$MBPL >= 460 & census_1920$MBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1920$MBPL == 499 | census_1920$MBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1920$MBPL >= 500 & census_1920$MBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1920$MBPL >= 510 & census_1920$MBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1920$MBPL >= 520 & census_1920$MBPL <= 524) | census_1920$MBPL == 548 | census_1920$MBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1920$MBPL >= 530 & census_1920$MBPL <= 547) | census_1920$MBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1920$MBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1920$MBPL >= 600 & census_1920$MBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1920$MBPL >= 700 & census_1920$MBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1920$MBPL==150, "Canada", "Other")))))))))))))))))))
census_1920$enum_dist=paste0(census_1920$STATEICP, "_", census_1920$COUNTYICP, "_", census_1920$ENUMDIST)
ethnic_frag_1920=matrix(ncol=(2+length(unique(census_1920$father_birthplace))), nrow=length(unique(census_1920$enum_dist)))
ethnic_frag_1920=as.data.frame(ethnic_frag_1920)
colnames=c("enum_dist", unique(census_1920$father_birthplace), "total_pop")
colnames(ethnic_frag_1920)=colnames
ethnic_frag_1920[,1]=unique(census_1920$enum_dist)
timeNow <- Sys.time()
for (i in 1:nrow(ethnic_frag_1920)){
  tmp=census_1920[which(census_1920$enum_dist==ethnic_frag_1920[i,1]),]
  for (j in 2:(length(colnames)-1)){
    ethnic_frag_1920[i,j]=nrow(tmp[which(tmp[,"father_birthplace"]==colnames[j]),])
  }
  ethnic_frag_1920[i,length(colnames)]=nrow(tmp)
  cat("\r", round(i*100/nrow(ethnic_frag_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
save(ethnic_frag_1920, file="intermediate_outputs/ethnic_frag_1920.rda")
rm(census_1920)
gc()


#1930

census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))

library(memisc)
census_1930$BPL=haven::zap_label(census_1930$BPL)
census_1930$BPL=as.numeric(census_1930$BPL)
census_1930=as.data.frame(census_1930)
census_1930$birthplace <- ifelse(census_1930$BPL < 100, "USA",
                                 ifelse(census_1930$BPL >= 200 & census_1930$BPL <= 300, "Latin America/ Caribbean",
                                        ifelse(census_1930$BPL >= 400 & census_1930$BPL <= 405, "Scandinavia",
                                               ifelse(census_1930$BPL >= 410 & census_1930$BPL <= 413, "United Kingdom",
                                                      ifelse(census_1930$BPL == 414, "Ireland",
                                                             ifelse(census_1930$BPL >= 420 & census_1930$BPL <= 429, "Other Western Europe",
                                                                    ifelse(census_1930$BPL >= 430 & census_1930$BPL <= 440 & census_1930$BPL != 434, "Other Southern Europe",
                                                                           ifelse(census_1930$BPL == 434, "Italy",
                                                                                  ifelse(census_1930$BPL >= 450 & census_1930$BPL <= 459, "Central/Eastern Europe",
                                                                                         ifelse(census_1930$BPL >= 460 & census_1930$BPL <= 465, "Russia/Empire",
                                                                                                ifelse(census_1930$BPL == 499 | census_1930$BPL == 419, "Other Europe",
                                                                                                       ifelse(census_1930$BPL >= 500 & census_1930$BPL <= 509, "East Asia",
                                                                                                              ifelse(census_1930$BPL >= 510 & census_1930$BPL <= 519, "Southeast Asia",
                                                                                                                     ifelse((census_1930$BPL >= 520 & census_1930$BPL <= 524) | census_1930$BPL == 548 | census_1930$BPL == 550, "India and Southern Asia",
                                                                                                                            ifelse((census_1930$BPL >= 530 & census_1930$BPL <= 547) | census_1930$BPL == 549, "Middle East",
                                                                                                                                   ifelse(census_1930$BPL == 599, "Other Asia",
                                                                                                                                          ifelse(census_1930$BPL >= 600 & census_1930$BPL <= 699, "Africa",
                                                                                                                                                 ifelse(census_1930$BPL >= 700 & census_1930$BPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                        ifelse(census_1930$BPL==150, "Canada", "Other")))))))))))))))))))




census_1930$FBPL=haven::zap_label(census_1930$FBPL)
census_1930$FBPL=as.numeric(census_1930$FBPL)
census_1930$father_birthplace <- ifelse(census_1930$FBPL < 100, "USA",
                                        ifelse(census_1930$FBPL >= 200 & census_1930$FBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1930$FBPL >= 400 & census_1930$FBPL <= 405, "Scandinavia",
                                                      ifelse(census_1930$FBPL >= 410 & census_1930$FBPL <= 413, "United Kingdom",
                                                             ifelse(census_1930$FBPL == 414, "Ireland",
                                                                    ifelse(census_1930$FBPL >= 420 & census_1930$FBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1930$FBPL >= 430 & census_1930$FBPL <= 440 & census_1930$FBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1930$FBPL == 434, "Italy",
                                                                                         ifelse(census_1930$FBPL >= 450 & census_1930$FBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1930$FBPL >= 460 & census_1930$FBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1930$FBPL == 499 | census_1930$FBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1930$FBPL >= 500 & census_1930$FBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1930$FBPL >= 510 & census_1930$FBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1930$FBPL >= 520 & census_1930$FBPL <= 524) | census_1930$FBPL == 548 | census_1930$FBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1930$FBPL >= 530 & census_1930$FBPL <= 547) | census_1930$FBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1930$FBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1930$FBPL >= 600 & census_1930$FBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1930$FBPL >= 700 & census_1930$FBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1930$FBPL==150, "Canada", "Other")))))))))))))))))))


census_1930$MBPL=haven::zap_label(census_1930$MBPL)
census_1930$MBPL=as.numeric(census_1930$MBPL)
census_1930$mother_birthplace <- ifelse(census_1930$MBPL < 100, "USA",
                                        ifelse(census_1930$MBPL >= 200 & census_1930$MBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1930$MBPL >= 400 & census_1930$MBPL <= 405, "Scandinavia",
                                                      ifelse(census_1930$MBPL >= 410 & census_1930$MBPL <= 413, "United Kingdom",
                                                             ifelse(census_1930$MBPL == 414, "Ireland",
                                                                    ifelse(census_1930$MBPL >= 420 & census_1930$MBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1930$MBPL >= 430 & census_1930$MBPL <= 440 & census_1930$MBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1930$MBPL == 434, "Italy",
                                                                                         ifelse(census_1930$MBPL >= 450 & census_1930$MBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1930$MBPL >= 460 & census_1930$MBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1930$MBPL == 499 | census_1930$MBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1930$MBPL >= 500 & census_1930$MBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1930$MBPL >= 510 & census_1930$MBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1930$MBPL >= 520 & census_1930$MBPL <= 524) | census_1930$MBPL == 548 | census_1930$MBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1930$MBPL >= 530 & census_1930$MBPL <= 547) | census_1930$MBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1930$MBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1930$MBPL >= 600 & census_1930$MBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1930$MBPL >= 700 & census_1930$MBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1930$MBPL==150, "Canada", "Other")))))))))))))))))))
census_1930$enum_dist=paste0(census_1930$STATEICP, "_", census_1930$COUNTYICP, "_", census_1930$ENUMDIST)
ethnic_frag_1930=matrix(ncol=(2+length(unique(census_1930$father_birthplace))), nrow=length(unique(census_1930$enum_dist)))
ethnic_frag_1930=as.data.frame(ethnic_frag_1930)
colnames=c("enum_dist", unique(census_1930$father_birthplace), "total_pop")
colnames(ethnic_frag_1930)=colnames
ethnic_frag_1930[,1]=unique(census_1930$enum_dist)
timeNow <- Sys.time()
for (i in 1:nrow(ethnic_frag_1930)){
  tmp=census_1930[which(census_1930$enum_dist==ethnic_frag_1930[i,1]),]
  for (j in 2:(length(colnames)-1)){
    ethnic_frag_1930[i,j]=nrow(tmp[which(tmp[,"father_birthplace"]==colnames[j]),])
  }
  ethnic_frag_1930[i,length(colnames)]=nrow(tmp)
  cat("\r", round(i*100/nrow(ethnic_frag_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
save(ethnic_frag_1930, file="intermediate_outputs/ethnic_frag_1930.rda")
rm(census_1930)
gc()


#1940

census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))

library(memisc)
census_1940$BPL=haven::zap_label(census_1940$BPL)
census_1940$BPL=as.numeric(census_1940$BPL)
census_1940=as.data.frame(census_1940)
census_1940$birthplace <- ifelse(census_1940$BPL < 100, "USA",
                                 ifelse(census_1940$BPL >= 200 & census_1940$BPL <= 300, "Latin America/ Caribbean",
                                        ifelse(census_1940$BPL >= 400 & census_1940$BPL <= 405, "Scandinavia",
                                               ifelse(census_1940$BPL >= 410 & census_1940$BPL <= 413, "United Kingdom",
                                                      ifelse(census_1940$BPL == 414, "Ireland",
                                                             ifelse(census_1940$BPL >= 420 & census_1940$BPL <= 429, "Other Western Europe",
                                                                    ifelse(census_1940$BPL >= 430 & census_1940$BPL <= 440 & census_1940$BPL != 434, "Other Southern Europe",
                                                                           ifelse(census_1940$BPL == 434, "Italy",
                                                                                  ifelse(census_1940$BPL >= 450 & census_1940$BPL <= 459, "Central/Eastern Europe",
                                                                                         ifelse(census_1940$BPL >= 460 & census_1940$BPL <= 465, "Russia/Empire",
                                                                                                ifelse(census_1940$BPL == 499 | census_1940$BPL == 419, "Other Europe",
                                                                                                       ifelse(census_1940$BPL >= 500 & census_1940$BPL <= 509, "East Asia",
                                                                                                              ifelse(census_1940$BPL >= 510 & census_1940$BPL <= 519, "Southeast Asia",
                                                                                                                     ifelse((census_1940$BPL >= 520 & census_1940$BPL <= 524) | census_1940$BPL == 548 | census_1940$BPL == 550, "India and Southern Asia",
                                                                                                                            ifelse((census_1940$BPL >= 530 & census_1940$BPL <= 547) | census_1940$BPL == 549, "Middle East",
                                                                                                                                   ifelse(census_1940$BPL == 599, "Other Asia",
                                                                                                                                          ifelse(census_1940$BPL >= 600 & census_1940$BPL <= 699, "Africa",
                                                                                                                                                 ifelse(census_1940$BPL >= 700 & census_1940$BPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                        ifelse(census_1940$BPL==150, "Canada", "Other")))))))))))))))))))




census_1940$FBPL=haven::zap_label(census_1940$FBPL)
census_1940$FBPL=as.numeric(census_1940$FBPL)
census_1940$father_birthplace <- ifelse(census_1940$FBPL < 100, "USA",
                                        ifelse(census_1940$FBPL >= 200 & census_1940$FBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1940$FBPL >= 400 & census_1940$FBPL <= 405, "Scandinavia",
                                                      ifelse(census_1940$FBPL >= 410 & census_1940$FBPL <= 413, "United Kingdom",
                                                             ifelse(census_1940$FBPL == 414, "Ireland",
                                                                    ifelse(census_1940$FBPL >= 420 & census_1940$FBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1940$FBPL >= 430 & census_1940$FBPL <= 440 & census_1940$FBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1940$FBPL == 434, "Italy",
                                                                                         ifelse(census_1940$FBPL >= 450 & census_1940$FBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1940$FBPL >= 460 & census_1940$FBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1940$FBPL == 499 | census_1940$FBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1940$FBPL >= 500 & census_1940$FBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1940$FBPL >= 510 & census_1940$FBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1940$FBPL >= 520 & census_1940$FBPL <= 524) | census_1940$FBPL == 548 | census_1940$FBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1940$FBPL >= 530 & census_1940$FBPL <= 547) | census_1940$FBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1940$FBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1940$FBPL >= 600 & census_1940$FBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1940$FBPL >= 700 & census_1940$FBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1940$FBPL==150, "Canada", "Other")))))))))))))))))))


census_1940$MBPL=haven::zap_label(census_1940$MBPL)
census_1940$MBPL=as.numeric(census_1940$MBPL)
census_1940$mother_birthplace <- ifelse(census_1940$MBPL < 100, "USA",
                                        ifelse(census_1940$MBPL >= 200 & census_1940$MBPL <= 300, "Latin America/ Caribbean",
                                               ifelse(census_1940$MBPL >= 400 & census_1940$MBPL <= 405, "Scandinavia",
                                                      ifelse(census_1940$MBPL >= 410 & census_1940$MBPL <= 413, "United Kingdom",
                                                             ifelse(census_1940$MBPL == 414, "Ireland",
                                                                    ifelse(census_1940$MBPL >= 420 & census_1940$MBPL <= 429, "Other Western Europe",
                                                                           ifelse(census_1940$MBPL >= 430 & census_1940$MBPL <= 440 & census_1940$MBPL != 434, "Other Southern Europe",
                                                                                  ifelse(census_1940$MBPL == 434, "Italy",
                                                                                         ifelse(census_1940$MBPL >= 450 & census_1940$MBPL <= 459, "Central/Eastern Europe",
                                                                                                ifelse(census_1940$MBPL >= 460 & census_1940$MBPL <= 465, "Russia/Empire",
                                                                                                       ifelse(census_1940$MBPL == 499 | census_1940$MBPL == 419, "Other Europe",
                                                                                                              ifelse(census_1940$MBPL >= 500 & census_1940$MBPL <= 509, "East Asia",
                                                                                                                     ifelse(census_1940$MBPL >= 510 & census_1940$MBPL <= 519, "Southeast Asia",
                                                                                                                            ifelse((census_1940$MBPL >= 520 & census_1940$MBPL <= 524) | census_1940$MBPL == 548 | census_1940$MBPL == 550, "India and Southern Asia",
                                                                                                                                   ifelse((census_1940$MBPL >= 530 & census_1940$MBPL <= 547) | census_1940$MBPL == 549, "Middle East",
                                                                                                                                          ifelse(census_1940$MBPL == 599, "Other Asia",
                                                                                                                                                 ifelse(census_1940$MBPL >= 600 & census_1940$MBPL <= 699, "Africa",
                                                                                                                                                        ifelse(census_1940$MBPL >= 700 & census_1940$MBPL <= 710, "Australia and Pacific Islands",
                                                                                                                                                               ifelse(census_1940$MBPL==150, "Canada", "Other")))))))))))))))))))
census_1940$enum_dist=paste0(census_1940$STATEICP, "_", census_1940$COUNTYICP, "_", census_1940$ENUMDIST)
ethnic_frag_1940=matrix(ncol=(2+length(unique(census_1940$father_birthplace))), nrow=length(unique(census_1940$enum_dist)))
ethnic_frag_1940=as.data.frame(ethnic_frag_1940)
colnames=c("enum_dist", unique(census_1940$father_birthplace), "total_pop")
colnames(ethnic_frag_1940)=colnames
ethnic_frag_1940[,1]=unique(census_1940$enum_dist)
timeNow <- Sys.time()
for (i in 1:nrow(ethnic_frag_1940)){
  tmp=census_1940[which(census_1940$enum_dist==ethnic_frag_1940[i,1]),]
  for (j in 2:(length(colnames)-1)){
    ethnic_frag_1940[i,j]=nrow(tmp[which(tmp[,"father_birthplace"]==colnames[j]),])
  }
  ethnic_frag_1940[i,length(colnames)]=nrow(tmp)
  cat("\r", round(i*100/nrow(ethnic_frag_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
save(ethnic_frag_1940, file="intermediate_outputs/ethnic_frag_1940.rda")
rm(census_1940)
gc()