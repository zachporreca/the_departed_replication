##########################################################################################################################################
#################### STEP 1D: MIMICKING NUMIDENT TO CENSUS MATCH ALGORITH WITH MAFIA BOOK GUYS AND GEOCODING LISTED ADDRESSES ############
####################          TO CREATE UNDERLYING DATA FOR 1960 MAFIA OUTCOMES                                               ############
##########################################################################################################################################


library(stringdist)
library(phonics)
library(haven)
library(readr)

########################################################################################
########## STEP 1: READING IN 1960 MAFIA REPORT DATA MNUALLY DIGITIZED BY RA ###########
##########     AND RAW DATA PROVIDED BY GIOVANNI MASTROBUONI                 ###########
##########              AND RA pre-1926 MAFIA BOOK EXTRACTIONS               ###########        
########################################################################################
nameadbday=read.csv("input_data/step_1_misc_data/nameadbday.csv") #RA DIGITIZED
pinotti_mafia_main=as.data.frame(read_dta("input_data/step_1_misc_data/pinotti_mafia.dta")) #MASTROBOUNI DATA
pinotti_mafia_main$match_name=paste0(tolower(pinotti_mafia_main$criminal_name)," ", tolower(pinotti_mafia_main$criminal_surname))
mafia=read.csv(file="input_data/step_1_misc_data/MafiaNAAD.csv") #RA DIGITZED (NAME NOT SEPERATED INTO NAME/SURNAME)
nelli=as.data.frame(read_delim(file="input_data/step_1_misc_data/nelli_pre1926_mafia.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE))
petrosino=as.data.frame(read_delim(file="input_data/step_1_misc_data/petrosino_pre1926_mafia.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE))
critchley=as.data.frame(read_delim(file="input_data/step_1_misc_data/critchley_pre1926_mafia.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE))
nelli=nelli[,c(1:(ncol(nelli)-2))]
petrosino=petrosino[,c(1:(ncol(petrosino)-2))]
critchley=critchley[,c(1:(ncol(critchley)-2))]

APIKEY="AIzaSyAkTSI03_Q_9xRrHzf4kQB7QsAZHV9QlOw" ##### RESEARCHER NEEDS TO INPUT API KEY FOR GEOLOCATING
################################################################
######## PROCEEDING YEAR BY YEAR ###############################
################################################################

#################################
############# 1900 ##############
#################################
pinotti_mafia=pinotti_mafia_main
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")

#preliminary cleaning
census_1900=census_1900[,c(2,10,24,25,27,37,42,53,54,134,135,140,142,145,146,147,148,149,150,
                           191,194,196,197,198,199,200,222,213)]
census_1900=census_1900[which(is.na(census_1900$namefrst)==FALSE),]
census_1900=census_1900[which(is.na(census_1900$namelast)==FALSE),]
census_1900=census_1900[which(census_1900$namefrst!=""),]
census_1900=census_1900[which(census_1900$namelast!=""),]

nameadbday$namefrst=nameadbday$criminal_name
nameadbday$namelast=nameadbday$criminal_surname
nameadbday$birth_date=as.Date(lubridate::dmy(nameadbday$birthcr))
nameadbday$birth_date=nameadbday$birth_date - lubridate::years(100)
nameadbday$name=paste0(nameadbday$namefrst, " ", nameadbday$namelast)
set_1900=nameadbday[which(as.Date(nameadbday$birth_date)<
                            as.Date("1899-12-31")),]
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other=other[which(other$death_year>1900|other$pre1926_end>1900),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==TRUE|other$birth_year<1900),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==FALSE),]
set_1900_other=data.frame(matrix(nrow=nrow(other), ncol=ncol(set_1900)))
colnames(set_1900_other)=colnames(set_1900)
set_1900_other$criminal_surname=other$surname
set_1900_other$criminal_name=other$first_name
set_1900_other$birth_date=paste0(other$birth_year, "-03-16") #random birthday (my own) from birthyear for conformity (only year is used in code)
set_1900_other$name=paste0(set_1900_other$criminal_name, " ", set_1900_other$criminal_surname)
census_1900$match_name=paste0(tolower(census_1900$namefrst), " ", tolower(census_1900$namelast))
census_1900$match_string=paste0(census_1900$match_name,"_",census_1900$birthyr)
set_1900$match_name=tolower(set_1900$name)
set_1900$matched=0
set_1900$matched2=0
set_1900$matched_record=0
set_1900$matched_record_2=0
set_1900_other$match_name=tolower(set_1900_other$name)
set_1900_other$matched=0
set_1900_other$matched2=0
set_1900_other$matched_record=0
set_1900_other$matched_record_2=0


set_1900$match_string=paste0(set_1900$match_name, "_", lubridate::year(set_1900$birth_date))
set_1900_other$match_string=paste0(set_1900_other$match_name, "_", lubridate::year(set_1900_other$birth_date))
set_1900_other$source_origin="pre_1926"
set_1900$source_origin="1959"
set_1900=rbind.data.frame(set_1900, set_1900_other)

strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(set_1900)){
  strings[[i]]=paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i]))
  strings[[i]]=append(strings[[i]], paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i])+1))
  strings[[i]]=append(strings[[i]], paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i])-1))
  cat("\r", round(i*100/nrow(set_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#matching on likely keys
timeNow <- Sys.time()
for (i in 1:nrow(set_1900)){
  tmp=vector(mode = "numeric", length=length(strings[[i]]))
  for (j in 1:length(strings[[i]])){
    tmp[j]=ifelse((set_1900$match_string[i] %in% unique(census_1900$match_string))==TRUE, 1, 0)
  }
  set_1900[i,10]=ifelse(sum(tmp)>0, 1, 0)
  set_1900[i,12]=ifelse(sum(tmp)>0, 
                        census_1900[which(census_1900$match_string==set_1900$match_string[[i]][which(tmp==1)][1]),28], 
                        "none")
  cat("\r", round(i*100/nrow(set_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}


#second round for misspellings
timeNow <- Sys.time()
for (i in 1:nrow(set_1900)){
  tmp=stringdist::amatch(set_1900$match_name[i], census_1900$match_name)
  if (is.na(tmp)==TRUE){
    set_1900[i,13]="none"
  } else{  
    set_1900[i,14]=paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i]))
    set_1900[i,11]=ifelse(set_1900[i,14]==paste0(
      census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
      census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]), 1, 
      0)
    if (set_1900[i,11]==1){
      set_1900[i,13]=ifelse(set_1900[i,14]==paste0(
        census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
        census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]),
        census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),28], 
        "none") 
    } else{
      set_1900[i,14]=paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i])+1)
      set_1900[i,11]=ifelse(set_1900[i,14]==paste0(
        census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
        census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]), 1, 
        0)
      if (set_1900[i,11]==1){
        set_1900[i,13]=ifelse(set_1900[i,14]==paste0(
          census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
          census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]),
          census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),28], 
          "none")
      } 
      else{
        set_1900[i,14]=paste0(set_1900[i,9],"_",lubridate::year(set_1900$birth_date[i])-1)
        set_1900[i,11]=ifelse(set_1900[i,14]==paste0(
          census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
          census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]), 1, 
          0)
        if (set_1900[i,11]==1){
          set_1900[i,13]=ifelse(set_1900[i,14]==paste0(
            census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),29], "_", 
            census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),21]),
            census_1900[stringdist::amatch(set_1900$match_name[i], census_1900$match_name),28], 
            "none")
        }
        else{
          set_1900[i,13]="none"
        }}}}
  cat("\r", round(i*100/nrow(set_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")}

set_1900$any_match=as.numeric(ifelse(as.numeric(set_1900$matched)==1|as.numeric(set_1900$matched2)==1, 1, 0))
set_1900$birth_cr=pinotti_mafia[match(set_1900$match_name, pinotti_mafia$match_name), "birthcountrycr"]
set_1900$US_born=ifelse(set_1900$birth_cr==224, 1, 0)

set_1900_matched=set_1900[which(set_1900$any_match==1),]
set_1900_unmatched=set_1900[which(set_1900$any_match==0),]

save(set_1900_matched, file="intermediate_outputs/set_1900_matched.rda")
save(set_1900_unmatched, file="intermediate_outputs/set_1900_unmatched.rda")

rm(census_1900)
gc()

#################################
############# 1910 ##############
#################################
census_1910 <- read.csv("input_data/census_restricted/census_1910.csv")
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")
col_names=colnames(census_1900)[c(2,10,24,25,27,37,42,53,54,134,135,140,142,145,146,147,148,149,150,
                                  191,194,196,197,198,199,200,222,213)]
colname_matrix=matrix(nrow=length(col_names), ncol=2)
colname_matrix[,1]=col_names
for (i in 1:nrow(colname_matrix)){
  colname_matrix[i,2]=which(colnames(census_1910)==colname_matrix[i,1])
}
cols=colname_matrix[,2]
census_1910=census_1910[,as.numeric(cols)]
census_1910=census_1910[which(is.na(census_1910$namefrst)==FALSE),]
census_1910=census_1910[which(is.na(census_1910$namelast)==FALSE),]
census_1910=census_1910[which(census_1910$namefrst!=""),]
census_1910=census_1910[which(census_1910$namelast!=""),]
set_1910=nameadbday[which(as.Date(nameadbday$birth_date)<
                            as.Date("1909-12-31") &
                            as.Date(nameadbday$birth_date)>
                            as.Date("1899-12-31")),]
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other=other[which(other$death_year>1910|other$pre1926_end>1910),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==TRUE|other$birth_year<1910),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==FALSE),]
set_1910_other=data.frame(matrix(nrow=nrow(other), ncol=ncol(set_1910)))
colnames(set_1910_other)=colnames(set_1910)
set_1910_other$criminal_surname=other$surname
set_1910_other$criminal_name=other$first_name
set_1910_other$birth_date=paste0(other$birth_year, "-03-16") #random birthday (my own) from birthyear for conformity (only year is used in code)
set_1910_other$name=paste0(set_1910_other$criminal_name, " ", set_1910_other$criminal_surname)
census_1910$match_name=paste0(tolower(census_1910$namefrst), " ", tolower(census_1910$namelast))
census_1910$match_string=paste0(census_1910$match_name,"_",census_1910$birthyr)
set_1910$match_name=tolower(set_1910$name)
set_1910$matched=0
set_1910$matched2=0
set_1910$matched_record=0
set_1910$matched_record_2=0
set_1910_other$match_name=tolower(set_1910_other$name)
set_1910_other$matched=0
set_1910_other$matched2=0
set_1910_other$matched_record=0
set_1910_other$matched_record_2=0


set_1910$match_string=paste0(set_1910$match_name, "_", lubridate::year(set_1910$birth_date))
set_1910_other$match_string=paste0(set_1910_other$match_name, "_", lubridate::year(set_1910_other$birth_date))
set_1910_other$source_origin="pre_1926"
set_1910$source_origin="1959"
set_1910=rbind.data.frame(set_1910, set_1910_other)
set_1910=unique(set_1910)
set_1910$any_match=0
set_1910$birth_cr=0
set_1910$US_born=0
set_1910=rbind.data.frame(set_1900_unmatched, set_1910)

strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(set_1910)){
  strings[[i]]=paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i]))
  strings[[i]]=append(strings[[i]], paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i])+1))
  strings[[i]]=append(strings[[i]], paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i])-1))
  cat("\r", round(i*100/nrow(set_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

timeNow <- Sys.time()
for (i in 1:nrow(set_1910)){
  tmp=vector(mode = "numeric", length=length(strings[[i]]))
  for (j in 1:length(strings[[i]])){
    tmp[j]=ifelse((set_1910$match_string[i] %in% unique(census_1910$match_string))==TRUE, 1, 0)
  }
  set_1910[i,10]=ifelse(sum(tmp)>0, 1, 0)
  set_1910[i,12]=ifelse(sum(tmp)>0, 
                        census_1910[which(census_1910$match_string==set_1910$match_string[[i]][which(tmp==1)][1]),28], 
                        "none")
  cat("\r", round(i*100/nrow(set_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}


set_1910$match_string=0
#second round for misspellings
timeNow <- Sys.time()
for (i in 1:nrow(set_1910)){
  tmp=stringdist::amatch(set_1910$match_name[i], census_1910$match_name)
  if (is.na(tmp)==TRUE){
    set_1910[i,13]="none"
  } else{  
    set_1910[i,14]=paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i]))
    set_1910[i,11]=ifelse(set_1910[i,14]==paste0(
      census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
      census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]), 1, 
      0)
    if (set_1910[i,11]==1){
      set_1910[i,13]=ifelse(set_1910[i,14]==paste0(
        census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
        census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]),
        census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),28], 
        "none") 
    } else{
      set_1910[i,14]=paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i])+1)
      set_1910[i,11]=ifelse(set_1910[i,14]==paste0(
        census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
        census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]), 1, 
        0)
      if (set_1910[i,11]==1){
        set_1910[i,13]=ifelse(set_1910[i,14]==paste0(
          census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
          census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]),
          census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),28], 
          "none")
      } 
      else{
        set_1910[i,14]=paste0(set_1910[i,9],"_",lubridate::year(set_1910$birth_date[i])-1)
        set_1910[i,11]=ifelse(set_1910[i,14]==paste0(
          census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
          census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]), 1, 
          0)
        if (set_1910[i,11]==1){
          set_1910[i,13]=ifelse(set_1910[i,14]==paste0(
            census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),29], "_", 
            census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),21]),
            census_1910[stringdist::amatch(set_1910$match_name[i], census_1910$match_name),28], 
            "none")
        }
        else{
          set_1910[i,13]="none"
        }}}}
  cat("\r", round(i*100/nrow(set_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")}

set_1910$any_match=as.numeric(ifelse(as.numeric(set_1910$matched)==1|as.numeric(set_1910$matched2)==1, 1, 0))
set_1910$birth_cr=pinotti_mafia[match(set_1910$match_name, pinotti_mafia$match_name), "birthcountrycr"]
set_1910$US_born=ifelse(set_1910$birth_cr==224, 1, 0)

set_1910_matched=set_1910[which(set_1910$any_match==1),]
set_1910_unmatched=set_1910[which(set_1910$any_match==0),]

save(set_1910_matched, file="intermediate_outputs/set_1910_matched.rda")
save(set_1910_unmatched, file="intermediate_outputs/set_1910_unmatched.rda")

rm(census_1910)
rm(census_1900)
gc()

#################################
############# 1920 ##############
#################################
census_1920 <- read.csv("input_data/census_restricted/census_1920.csv")
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")
col_names=colnames(census_1900)[c(2,10,24,25,27,37,42,53,54,134,135,140,142,145,146,147,148,149,150,
                                  191,194,196,197,198,199,200,222,213)]
colname_matrix=matrix(nrow=length(col_names), ncol=2)
colname_matrix[,1]=col_names
for (i in 1:nrow(colname_matrix)){
  colname_matrix[i,2]=which(colnames(census_1920)==colname_matrix[i,1])
}
cols=colname_matrix[,2]
census_1920=census_1920[,as.numeric(cols)]
census_1920=census_1920[which(is.na(census_1920$namefrst)==FALSE),]
census_1920=census_1920[which(is.na(census_1920$namelast)==FALSE),]
census_1920=census_1920[which(census_1920$namefrst!=""),]
census_1920=census_1920[which(census_1920$namelast!=""),]
set_1920=nameadbday[which(as.Date(nameadbday$birth_date)<
                            as.Date("1919-12-31") &
                            as.Date(nameadbday$birth_date)>
                            as.Date("1909-12-31")),]
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other=other[which(other$death_year>1920|other$pre1926_end>1920),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==TRUE|other$birth_year<1920),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==FALSE),]
set_1920_other=data.frame(matrix(nrow=nrow(other), ncol=ncol(set_1920)))
colnames(set_1920_other)=colnames(set_1920)
set_1920_other$criminal_surname=other$surname
set_1920_other$criminal_name=other$first_name
set_1920_other$birth_date=paste0(other$birth_year, "-03-16") #random birthday (my own) from birthyear for conformity (only year is used in code)
set_1920_other$name=paste0(set_1920_other$criminal_name, " ", set_1920_other$criminal_surname)
census_1920$match_name=paste0(tolower(census_1920$namefrst), " ", tolower(census_1920$namelast))
census_1920$match_string=paste0(census_1920$match_name,"_",census_1920$birthyr)
set_1920$match_name=tolower(set_1920$name)
set_1920$matched=0
set_1920$matched2=0
set_1920$matched_record=0
set_1920$matched_record_2=0
set_1920_other$match_name=tolower(set_1920_other$name)
set_1920_other$matched=0
set_1920_other$matched2=0
set_1920_other$matched_record=0
set_1920_other$matched_record_2=0


set_1920$match_string=paste0(set_1920$match_name, "_", lubridate::year(set_1920$birth_date))
set_1920_other$match_string=paste0(set_1920_other$match_name, "_", lubridate::year(set_1920_other$birth_date))
set_1920_other$source_origin="pre_1926"
set_1920$source_origin="1959"
set_1920=rbind.data.frame(set_1920, set_1920_other)
set_1920=unique(set_1920)
set_1920$any_match=0
set_1920$birth_cr=0
set_1920$US_born=0
set_1920=rbind.data.frame(set_1910_unmatched, set_1920)

strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(set_1920)){
  strings[[i]]=paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i]))
  strings[[i]]=append(strings[[i]], paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i])+1))
  strings[[i]]=append(strings[[i]], paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i])-1))
  cat("\r", round(i*100/nrow(set_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}
timeNow <- Sys.time()
for (i in 1:nrow(set_1920)){
  tmp=vector(mode = "numeric", length=length(strings[[i]]))
  for (j in 1:length(strings[[i]])){
    tmp[j]=ifelse((set_1920$match_string[i] %in% unique(census_1920$match_string))==TRUE, 1, 0)
  }
  set_1920[i,10]=ifelse(sum(tmp)>0, 1, 0)
  set_1920[i,12]=ifelse(sum(tmp)>0, 
                        census_1920[which(census_1920$match_string==set_1920$match_string[[i]][which(tmp==1)][1]),28], 
                        "none")
  cat("\r", round(i*100/nrow(set_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}


set_1920$match_string=0
#second round for misspellings
timeNow <- Sys.time()
for (i in 1:nrow(set_1920)){
  tmp=stringdist::amatch(set_1920$match_name[i], census_1920$match_name)
  if (is.na(tmp)==TRUE){
    set_1920[i,13]="none"
  } else{  
    set_1920[i,14]=paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i]))
    set_1920[i,11]=ifelse(set_1920[i,14]==paste0(
      census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
      census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]), 1, 
      0)
    if (set_1920[i,11]==1){
      set_1920[i,13]=ifelse(set_1920[i,14]==paste0(
        census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
        census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]),
        census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),28], 
        "none") 
    } else{
      set_1920[i,14]=paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i])+1)
      set_1920[i,11]=ifelse(set_1920[i,14]==paste0(
        census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
        census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]), 1, 
        0)
      if (set_1920[i,11]==1){
        set_1920[i,13]=ifelse(set_1920[i,14]==paste0(
          census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
          census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]),
          census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),28], 
          "none")
      } 
      else{
        set_1920[i,14]=paste0(set_1920[i,9],"_",lubridate::year(set_1920$birth_date[i])-1)
        set_1920[i,11]=ifelse(set_1920[i,14]==paste0(
          census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
          census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]), 1, 
          0)
        if (set_1920[i,11]==1){
          set_1920[i,13]=ifelse(set_1920[i,14]==paste0(
            census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),29], "_", 
            census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),21]),
            census_1920[stringdist::amatch(set_1920$match_name[i], census_1920$match_name),28], 
            "none")
        }
        else{
          set_1920[i,13]="none"
        }}}}
  cat("\r", round(i*100/nrow(set_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")}

set_1920$any_match=as.numeric(ifelse(as.numeric(set_1920$matched)==1|as.numeric(set_1920$matched2)==1, 1, 0))
set_1920$birth_cr=pinotti_mafia[match(set_1920$match_name, pinotti_mafia$match_name), "birthcountrycr"]
set_1920$US_born=ifelse(set_1920$birth_cr==224, 1, 0)

set_1920_matched=set_1920[which(set_1920$any_match==1),]
set_1920_unmatched=set_1920[which(set_1920$any_match==0),]

save(set_1920_matched, file="intermediate_outputs/set_1920_matched.rda")
save(set_1920_unmatched, file="intermediate_outputs/set_1920_unmatched.rda")

rm(census_1920)
rm(census_1900)
gc()

#################################
############# 1930 ##############
#################################
census_1930 <- read.csv("input_data/census_restricted/census_1930.csv")
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")
col_names=colnames(census_1900)[c(2,10,24,25,27,37,42,53,54,134,135,140,142,145,146,147,148,149,150,
                                  191,194,196,197,198,199,200,222,213)]
colname_matrix=matrix(nrow=length(col_names), ncol=2)
colname_matrix[,1]=col_names
for (i in 1:nrow(colname_matrix)){
  tryCatch({
    colname_matrix[i,2]=which(colnames(census_1930)==colname_matrix[i,1])
  }, error=function(e){})
}
colname_matrix[,2]=ifelse(is.na(colname_matrix[,2])==TRUE, 1, colname_matrix[,2])
cols=colname_matrix[,2]
census_1930=census_1930[,as.numeric(cols)]
census_1930=census_1930[which(is.na(census_1930$namefrst)==FALSE),]
census_1930=census_1930[which(is.na(census_1930$namelast)==FALSE),]
census_1930=census_1930[which(census_1930$namefrst!=""),]
census_1930=census_1930[which(census_1930$namelast!=""),]
set_1930=nameadbday[which(as.Date(nameadbday$birth_date)<
                            as.Date("1929-12-31") &
                            as.Date(nameadbday$birth_date)>
                            as.Date("1919-12-31")),]
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other=other[which(other$death_year>1930|other$pre1926_end>1930),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==TRUE|other$birth_year<1930),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==FALSE),]
set_1930_other=data.frame(matrix(nrow=nrow(other), ncol=ncol(set_1930)))
colnames(set_1930_other)=colnames(set_1930)
set_1930_other$criminal_surname=other$surname
set_1930_other$criminal_name=other$first_name
set_1930_other$birth_date=paste0(other$birth_year, "-03-16") #random birthday (my own) from birthyear for conformity (only year is used in code)
set_1930_other$name=paste0(set_1930_other$criminal_name, " ", set_1930_other$criminal_surname)
census_1930$match_name=paste0(tolower(census_1930$namefrst), " ", tolower(census_1930$namelast))
census_1930$match_string=paste0(census_1930$match_name,"_",census_1930$birthyr)
set_1930$match_name=tolower(set_1930$name)
set_1930$matched=0
set_1930$matched2=0
set_1930$matched_record=0
set_1930$matched_record_2=0
set_1930_other$match_name=tolower(set_1930_other$name)
set_1930_other$matched=0
set_1930_other$matched2=0
set_1930_other$matched_record=0
set_1930_other$matched_record_2=0


set_1930$match_string=paste0(set_1930$match_name, "_", lubridate::year(set_1930$birth_date))
set_1930_other$match_string=paste0(set_1930_other$match_name, "_", lubridate::year(set_1930_other$birth_date))
set_1930_other$source_origin="pre_1926"
set_1930$source_origin="1959"
set_1930=rbind.data.frame(set_1930, set_1930_other)
set_1930=unique(set_1930)

set_1930$any_match=0
set_1930$birth_cr=0
set_1930$US_born=0
set_1930=rbind.data.frame(set_1920_unmatched, set_1930)

strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(set_1930)){
  strings[[i]]=paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i]))
  strings[[i]]=append(strings[[i]], paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i])+1))
  strings[[i]]=append(strings[[i]], paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i])-1))
  cat("\r", round(i*100/nrow(set_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

timeNow <- Sys.time()
for (i in 1:nrow(set_1930)){
  tmp=vector(mode = "numeric", length=length(strings[[i]]))
  for (j in 1:length(strings[[i]])){
    tmp[j]=ifelse((set_1930$match_string[i] %in% unique(census_1930$match_string))==TRUE, 1, 0)
  }
  set_1930[i,10]=ifelse(sum(tmp)>0, 1, 0)
  set_1930[i,12]=ifelse(sum(tmp)>0, 
                        census_1930[which(census_1930$match_string==set_1930$match_string[[i]][which(tmp==1)][1]),28], 
                        "none")
  cat("\r", round(i*100/nrow(set_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}


set_1930$match_string=0
#second round for misspellings
timeNow <- Sys.time()
for (i in 1:nrow(set_1930)){
  tmp=stringdist::amatch(set_1930$match_name[i], census_1930$match_name)
  if (is.na(tmp)==TRUE){
    set_1930[i,13]="none"
  } else{  
    set_1930[i,14]=paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i]))
    set_1930[i,11]=ifelse(set_1930[i,14]==paste0(
      census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
      census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]), 1, 
      0)
    if (set_1930[i,11]==1){
      set_1930[i,13]=ifelse(set_1930[i,14]==paste0(
        census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
        census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]),
        census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),28], 
        "none") 
    } else{
      set_1930[i,14]=paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i])+1)
      set_1930[i,11]=ifelse(set_1930[i,14]==paste0(
        census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
        census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]), 1, 
        0)
      if (set_1930[i,11]==1){
        set_1930[i,13]=ifelse(set_1930[i,14]==paste0(
          census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
          census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]),
          census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),28], 
          "none")
      } 
      else{
        set_1930[i,14]=paste0(set_1930[i,9],"_",lubridate::year(set_1930$birth_date[i])-1)
        set_1930[i,11]=ifelse(set_1930[i,14]==paste0(
          census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
          census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]), 1, 
          0)
        if (set_1930[i,11]==1){
          set_1930[i,13]=ifelse(set_1930[i,14]==paste0(
            census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),29], "_", 
            census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),21]),
            census_1930[stringdist::amatch(set_1930$match_name[i], census_1930$match_name),28], 
            "none")
        }
        else{
          set_1930[i,13]="none"
        }}}}
  cat("\r", round(i*100/nrow(set_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")}

set_1930$any_match=as.numeric(ifelse(as.numeric(set_1930$matched)==1|as.numeric(set_1930$matched2)==1, 1, 0))
set_1930$birth_cr=pinotti_mafia[match(set_1930$match_name, pinotti_mafia$match_name), "birthcountrycr"]
set_1930$US_born=ifelse(set_1930$birth_cr==224, 1, 0)

set_1930_matched=set_1930[which(set_1930$any_match==1),]
set_1930_unmatched=set_1930[which(set_1930$any_match==0),]

save(set_1930_matched, file="intermediate_outputs/set_1930_matched.rda")
save(set_1930_unmatched, file="intermediate_outputs/set_1930_unmatched.rda")

rm(census_1930)
rm(census_1900)
gc()

#################################
############# 1940 ##############
#################################
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")
col_names=colnames(census_1900)[c(2,10,24,25,27,37,42,53,54,134,135,140,142,145,146,147,148,149,150,
                                  191,194,196,197,198,199,200,222,213)]
colname_matrix=matrix(nrow=length(col_names), ncol=2)
colname_matrix[,1]=col_names
for (i in 1:nrow(colname_matrix)){
  tryCatch({
    colname_matrix[i,2]=which(colnames(census_1940)==colname_matrix[i,1])
  }, error=function(e){})
}
colname_matrix[,2]=ifelse(is.na(colname_matrix[,2])==TRUE, 1, colname_matrix[,2])
cols=colname_matrix[,2]
census_1940=census_1940[,as.numeric(cols)]
census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]
set_1940=nameadbday[which(as.Date(nameadbday$birth_date)<
                            as.Date("1939-12-31") &
                            as.Date(nameadbday$birth_date)>
                            as.Date("1929-12-31")),]
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other=other[which(other$death_year>1940|other$pre1926_end>1940),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==TRUE|other$birth_year<1940),] #ensures individual is alive at relevant census
other=other[which(is.na(other$birth_year)==FALSE),]
set_1940_other=data.frame(matrix(nrow=nrow(other), ncol=ncol(set_1940)))
colnames(set_1940_other)=colnames(set_1940)
set_1940_other$criminal_surname=other$surname
set_1940_other$criminal_name=other$first_name
set_1940_other$birth_date=paste0(other$birth_year, "-03-16") #random birthday (my own) from birthyear for conformity (only year is used in code)
set_1940_other$name=paste0(set_1940_other$criminal_name, " ", set_1940_other$criminal_surname)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$birthyr)
set_1940$match_name=tolower(set_1940$name)
set_1940$matched=0
set_1940$matched2=0
set_1940$matched_record=0
set_1940$matched_record_2=0
set_1940_other$match_name=tolower(set_1940_other$name)
set_1940_other$matched=0
set_1940_other$matched2=0
set_1940_other$matched_record=0
set_1940_other$matched_record_2=0


set_1940$match_string=paste0(set_1940$match_name, "_", lubridate::year(set_1940$birth_date))
set_1940_other$match_string=paste0(set_1940_other$match_name, "_", lubridate::year(set_1940_other$birth_date))
set_1940_other$source_origin="pre_1926"
set_1940$source_origin="1959"
set_1940=rbind.data.frame(set_1940, set_1940_other)
set_1940=unique(set_1940)

set_1940$any_match=0
set_1940$birth_cr=0
set_1940$US_born=0
set_1940=rbind.data.frame(set_1930_unmatched, set_1940)

strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(set_1940)){
  strings[[i]]=paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i]))
  strings[[i]]=append(strings[[i]], paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i])+1))
  strings[[i]]=append(strings[[i]], paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i])-1))
  cat("\r", round(i*100/nrow(set_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

timeNow <- Sys.time()
for (i in 1:nrow(set_1940)){
  tmp=vector(mode = "numeric", length=length(strings[[i]]))
  for (j in 1:length(strings[[i]])){
    tmp[j]=ifelse((set_1940$match_string[i] %in% unique(census_1940$match_string))==TRUE, 1, 0)
  }
  set_1940[i,10]=ifelse(sum(tmp)>0, 1, 0)
  set_1940[i,12]=ifelse(sum(tmp)>0, 
                        census_1940[which(census_1940$match_string==set_1940$match_string[[i]][which(tmp==1)][1]),28], 
                        "none")
  cat("\r", round(i*100/nrow(set_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}


set_1940$match_string=0
#second round for misspellings
timeNow <- Sys.time()
for (i in 1:nrow(set_1940)){
  tmp=stringdist::amatch(set_1940$match_name[i], census_1940$match_name)
  if (is.na(tmp)==TRUE){
    set_1940[i,13]="none"
  } else{  
    set_1940[i,14]=paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i]))
    set_1940[i,11]=ifelse(set_1940[i,14]==paste0(
      census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
      census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]), 1, 
      0)
    if (set_1940[i,11]==1){
      set_1940[i,13]=ifelse(set_1940[i,14]==paste0(
        census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
        census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]),
        census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),28], 
        "none") 
    } else{
      set_1940[i,14]=paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i])+1)
      set_1940[i,11]=ifelse(set_1940[i,14]==paste0(
        census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
        census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]), 1, 
        0)
      if (set_1940[i,11]==1){
        set_1940[i,13]=ifelse(set_1940[i,14]==paste0(
          census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
          census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]),
          census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),28], 
          "none")
      } 
      else{
        set_1940[i,14]=paste0(set_1940[i,9],"_",lubridate::year(set_1940$birth_date[i])-1)
        set_1940[i,11]=ifelse(set_1940[i,14]==paste0(
          census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
          census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]), 1, 
          0)
        if (set_1940[i,11]==1){
          set_1940[i,13]=ifelse(set_1940[i,14]==paste0(
            census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),29], "_", 
            census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),21]),
            census_1940[stringdist::amatch(set_1940$match_name[i], census_1940$match_name),28], 
            "none")
        }
        else{
          set_1940[i,13]="none"
        }}}}
  cat("\r", round(i*100/nrow(set_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")}

set_1940$any_match=as.numeric(ifelse(as.numeric(set_1940$matched)==1|as.numeric(set_1940$matched2)==1, 1, 0))
set_1940$birth_cr=pinotti_mafia[match(set_1940$match_name, pinotti_mafia$match_name), "birthcountrycr"]
set_1940$US_born=ifelse(set_1940$birth_cr==224, 1, 0)
set_1940_matched=set_1940[which(set_1940$any_match==1),]
set_1940_unmatched=set_1940[which(set_1940$any_match==0),]


save(set_1940_matched, file="intermediate_outputs/set_1940_matched.rda")
save(set_1940_unmatched, file="intermediate_outputs/set_1940_unmatched.rda")

rm(census_1940)
rm(census_1900)
gc()

#####################################################################################################
###################### STEP 2: PULLING CORRESPONDING CENSUS RECORDS OF MATCHED MAFIOSI ##############
#####################################################################################################
census_1900=as.data.frame(read_dta("input_data/census_unrestricted/census_1900.dta"))
colnames(census_1900)=tolower(colnames(census_1900))
set_1900_matched$record=ifelse(set_1900_matched$matched_record=="none", set_1900_matched$matched_record_2, set_1900_matched$matched_record)
matched_census_1900=census_1900[which((census_1900$histid %in% unique(set_1900_matched$record))==TRUE), ]
matched_census_1900=as.data.frame(matched_census_1900)
save(matched_census_1900, file="intermediate_outputs/matched_census_1900.rda")
matched_census_1900_book=census_1900[which((census_1900$histid %in% unique(set_1900_matched[which(set_1900_matched$source_origin=="1959"),]$record))==TRUE), ]
matched_census_1900_book=as.data.frame(matched_census_1900_book)
save(matched_census_1900_book, file="intermediate_outputs/matched_census_1900_book.rda")
matched_census_1900_pre=census_1900[which((census_1900$histid %in% unique(set_1900_matched[which(set_1900_matched$source_origin=="pre_1926"),]$record))==TRUE), ]
matched_census_1900_pre=as.data.frame(matched_census_1900_pre)
save(matched_census_1900_pre, file="intermediate_outputs/matched_census_1900_pre.rda")
rm(census_1900)
census_1910=as.data.frame(read_dta("input_data/census_unrestricted/census_1910.dta"))
colnames(census_1910)=tolower(colnames(census_1910))
set_1910_matched$record=ifelse(set_1910_matched$matched_record=="none", set_1910_matched$matched_record_2, set_1910_matched$matched_record)
matched_census_1910=census_1910[which((census_1910$histid %in% unique(set_1910_matched$record))==TRUE), ]
save(matched_census_1910, file="intermediate_outputs/matched_census_1910.rda")
matched_census_1910_book=census_1910[which((census_1910$histid %in% unique(set_1910_matched[which(set_1910_matched$source_origin=="1959"),]$record))==TRUE), ]
matched_census_1910_book=as.data.frame(matched_census_1910_book)
save(matched_census_1910_book, file="intermediate_outputs/matched_census_1910_book.rda")
matched_census_1910_pre=census_1910[which((census_1910$histid %in% unique(set_1910_matched[which(set_1910_matched$source_origin=="pre_1926"),]$record))==TRUE), ]
matched_census_1910_pre=as.data.frame(matched_census_1910_pre)
save(matched_census_1910_pre, file="intermediate_outputs/matched_census_1910_pre.rda")
rm(census_1910)
census_1920=as.data.frame(read_dta("input_data/census_unrestricted/census_1920.dta"))
colnames(census_1920)=tolower(colnames(census_1920))
set_1920_matched$record=ifelse(set_1920_matched$matched_record=="none", set_1920_matched$matched_record_2, set_1920_matched$matched_record)
matched_census_1920=census_1920[which((census_1920$histid %in% unique(set_1920_matched$record))==TRUE), ]
save(matched_census_1920, file="intermediate_outputs/matched_census_1920.rda")
matched_census_1920_book=census_1920[which((census_1920$histid %in% unique(set_1920_matched[which(set_1920_matched$source_origin=="1959"),]$record))==TRUE), ]
matched_census_1920_book=as.data.frame(matched_census_1920_book)
save(matched_census_1920_book, file="intermediate_outputs/matched_census_1920_book.rda")
matched_census_1920_pre=census_1920[which((census_1920$histid %in% unique(set_1920_matched[which(set_1920_matched$source_origin=="pre_1926"),]$record))==TRUE), ]
matched_census_1920_pre=as.data.frame(matched_census_1920_pre)
save(matched_census_1920_pre, file="intermediate_outputs/matched_census_1920_pre.rda")
rm(census_1920)
census_1930=as.data.frame(read_dta("input_data/census_unrestricted/census_1930.dta"))
colnames(census_1930)=tolower(colnames(census_1930))
set_1930_matched$record=ifelse(set_1930_matched$matched_record=="none", set_1930_matched$matched_record_2, set_1930_matched$matched_record)
matched_census_1930=census_1930[which((census_1930$histid %in% unique(set_1930_matched$record))==TRUE), ]
save(matched_census_1930, file="intermediate_outputs/matched_census_1930.rda")
matched_census_1930_book=census_1930[which((census_1930$histid %in% unique(set_1930_matched[which(set_1930_matched$source_origin=="1959"),]$record))==TRUE), ]
matched_census_1930_book=as.data.frame(matched_census_1930_book)
save(matched_census_1930_book, file="intermediate_outputs/matched_census_1930_book.rda")
matched_census_1930_pre=census_1930[which((census_1930$histid %in% unique(set_1930_matched[which(set_1930_matched$source_origin=="pre_1926"),]$record))==TRUE), ]
matched_census_1930_pre=as.data.frame(matched_census_1930_pre)
save(matched_census_1930_pre, file="intermediate_outputs/matched_census_1930_pre.rda")
rm(census_1930)
census_1940=as.data.frame(read_dta("input_data/census_unrestricted/census_1940.dta"))
colnames(census_1940)=tolower(colnames(census_1940))
set_1940_matched$record=ifelse(set_1940_matched$matched_record=="none", set_1940_matched$matched_record_2, set_1940_matched$matched_record)
matched_census_1940=census_1940[which((census_1940$histid %in% unique(set_1940_matched$record))==TRUE), ]
save(matched_census_1940, file="intermediate_outputs/matched_census_1940.rda")
matched_census_1940_book=census_1940[which((census_1940$histid %in% unique(set_1940_matched[which(set_1940_matched$source_origin=="1959"),]$record))==TRUE), ]
matched_census_1940_book=as.data.frame(matched_census_1940_book)
save(matched_census_1940_book, file="intermediate_outputs/matched_census_1940_book.rda")
matched_census_1940_pre=census_1940[which((census_1940$histid %in% unique(set_1940_matched[which(set_1940_matched$source_origin=="pre_1926"),]$record))==TRUE), ]
matched_census_1940_pre=as.data.frame(matched_census_1940_pre)
save(matched_census_1940_pre, file="intermediate_outputs/matched_census_1940_pre.rda")
rm(census_1940)


########## COUNTING GUYS FOUND FROM PRE-1926 BOOKS #########
test=append(unique(matched_census_1900_pre$histid), unique(matched_census_1910_pre$histid))
test=append(test, unique(matched_census_1920_pre$histid))
test=append(test, unique(matched_census_1930_pre$histid))
test=append(test, unique(matched_census_1940_pre$histid))
test=test[which((test %in% unique(matched_census_1900_book$histid))==FALSE)]
test=test[which((test %in% unique(matched_census_1910_book$histid))==FALSE)]
test=test[which((test %in% unique(matched_census_1920_book$histid))==FALSE)]
test=test[which((test %in% unique(matched_census_1930_book$histid))==FALSE)]
test=test[which((test %in% unique(matched_census_1940_book$histid))==FALSE)]
length(test) ### 64  individuals

##########THIS MAYBE DELTE? ###########################
#creating enum dist ID
setwd("~/linking_census/mafia_1960")
matched_census_1900$enum_dist_id=paste0(matched_census_1900$stateicp, "_", matched_census_1900$countyicp, "_",matched_census_1900$enumdist)
matched_census_1910$enum_dist_id=paste0(matched_census_1910$stateicp, "_", matched_census_1910$countyicp, "_",matched_census_1910$enumdist)
matched_census_1920$enum_dist_id=paste0(matched_census_1920$stateicp, "_", matched_census_1920$countyicp, "_",matched_census_1920$enumdist)
matched_census_1930$enum_dist_id=paste0(matched_census_1930$stateicp, "_", matched_census_1930$countyicp, "_",matched_census_1930$enumdist)
matched_census_1940$enum_dist_id=paste0(matched_census_1940$stateicp, "_", matched_census_1940$countyicp, "_",matched_census_1940$enumdist)

ed_1900=unique(matched_census_1900$enum_dist_id)
ed_1910=unique(matched_census_1910$enum_dist_id)
ed_1920=unique(matched_census_1920$enum_dist_id)
ed_1930=unique(matched_census_1930$enum_dist_id)
ed_1940=unique(matched_census_1940$enum_dist_id)

##############################################################################################
############################# STEP 3: GEOLOCATING RECORDED ADDRESSES #########################
##############################################################################################

#####################################
######### FROM 1959 BOOK ############
#####################################
mafia=mafia[,c(2,3)]
mafia=mafia[which(mafia[,2] != ""),]

addresses=strsplit(mafia[,2], " \\| ")

address_list=list()

for (i in 1:length(addresses)) {
  address_list[[i]]=matrix(nrow=length(addresses[[i]]), ncol=2)
  address_list[[i]][,1]=mafia[i,1]
  for (j in 1:length(addresses[[i]])){
    address_list[[i]][j,2]=addresses[[i]][j]
  }
}


addresses=do.call(rbind, address_list)
addresses=as.data.frame(addresses)
colnames(addresses)=c("name", "address")

library(tmaptools)
geocode_OSM("4707 Bay View Ave, Tampa, Florida", projection = 3857)
library(ggmap)
register_google(APIKEY, write = TRUE) ###### API KEY NEEDS TO BE SUPPLIED BY RESEARCHER
geocoded=geocode(addresses[,2], output = "latlon")
geocoded=as.data.frame(geocoded)
pattern="^[A-Za-z ]+, [A-Z]{2}, [A-Za-z ]+$"
local_list=list()
for (i in 1:nrow(geocoded)){
  result <- tryCatch({
    tmp=geocode(paste0(geocoded[i,2],",", geocoded[i,1]), output = "all")
    tmp=tmp$results
    tmp_vec=list()
    for (j in 1:length(tmp)){
      tmp_vec[[j]]=grepl(pattern, tmp[[j]]$formatted_address)
    }
    tmp=tmp[c(which(tmp_vec==TRUE))]
    tmp=tmp[1]
    local_list[[i]]=tmp[[1]]$formatted_address
  }, error = function(e){})
}
length_list=list()
for (i in 1:length(local_list)){
  length_list[[i]]=length(local_list[[i]])
}
length_list=do.call(rbind, length_list)
local_list[c(which(length_list==0))]=0
locality_vec=do.call(rbind, local_list)

sample_cities=c("New York, NY, USA", "Boston, MA, USA", "Brooklyn, NY, USA", "Chicago, IL, USA", "Cleveland, OH, USA",
                "Cuyahoga County, OH, USA", "Detroit, MI, USA", "Philadelphia, PA, USA", "Pittsburgh, PA, USA",
                "Baltimore, MD, USA", "Cincinnati, OH, USA", "St. Louis, MO, USA")
#no observations from st louis or cincinnati

geocoded$city=locality_vec
geocoded$sample_city=ifelse((geocoded$city %in% sample_cities)==TRUE, 1, 0)
addresses=cbind.data.frame(addresses, geocoded)
addresses_sample=addresses[which(addresses$sample_city==1),]
addresses_book=addresses
addresses_book_sample=addresses_sample
save(addresses, file = "intermediate_outputs/addresses_book.rda")
save(addresses_sample, file="intermediate_outputs/addresses_book_in_sample.rda")


##### FOLLOWING SECTION IS NOT WORTH IT ONLY 3 NEW ADDRESSES, 2 IN NYC OTHER OUT OF SAMPLE
#####################################
######### FROM pre-1926 #############
#####################################
other=rbind.data.frame(nelli, petrosino)
other=rbind.data.frame(other, critchley)
other=other[which(grepl("family", other$name)==FALSE),]
other$name=gsub("\\s*\\([^)]*\\)", "", other$name)
other$surname=trimws(sub(",.*$", "", other$name))
other$first_name=trimws(sub("^.*,", "", other$name))
other$Name=paste0(other$first_name, " ", other$surname)
other$Address=ifelse(is.na(other$other_locations), other$location_primary, paste0(other$location_primary, " | ",other$other_locations))
other=other[,c("Name", "Address")]
  
addresses <- strsplit(other[, 2], "\\s*(?:\\||;)\\s*")

address_list=list()

for (i in 1:length(addresses)) {
  address_list[[i]]=matrix(nrow=length(addresses[[i]]), ncol=2)
  address_list[[i]][,1]=other[i,1]
  for (j in 1:length(addresses[[i]])){
    address_list[[i]][j,2]=addresses[[i]][j]
  }
}


addresses=do.call(rbind, address_list)
addresses=as.data.frame(addresses)
colnames(addresses)=c("name", "address")
      #### introducing a extremely specific regex rule for determing which addresses are geocodable, because RA recorded a lot of nonsense
street_pattern <- paste0(
  "\\b\\d{1,4}\\s+",
  "(?:(?:N|S|E|W|North|South|East|West)\\.?\\s+)?",
  "(?:[A-Za-z][A-Za-z.'-]*|\\d{1,3}(?:st|nd|rd|th)?)",
  "(?:\\s+[A-Za-z][A-Za-z.'-]*){0,3}\\s+",
  "(?:Street|St\\.?|Avenue|Ave\\.?|Road|Rd\\.?|",
  "Place|Pl\\.?|Boulevard|Blvd\\.?|Drive|Dr\\.?|",
  "Lane|Ln\\.?|Court|Ct\\.?|Terrace|Ter\\.?|",
  "Parkway|Pkwy\\.?|Highway|Hwy\\.?|Basin)\\b"
)

locality_pattern <- paste0(
  "\\b(?:",
  "New York(?: City)?|NYC|NY|",
  "Manhattan|Brooklyn|Bronx|Queens|Staten Island|",
  "Philadelphia|New Orleans",
  ")\\b"
)

valid_address <- (
  grepl(street_pattern, addresses$address,
        perl = TRUE, ignore.case = TRUE) &
    grepl(locality_pattern, addresses$address,
          perl = TRUE, ignore.case = TRUE)
)
addresses=addresses[valid_address, ] #59 valid addresses

register_google(APIKEY, write = TRUE) ###### API KEY NEEDS TO BE SUPPLIED BY RESEARCHER
geocoded=geocode(addresses[,2], output = "latlon")
geocoded=as.data.frame(geocoded)
pattern="^[A-Za-z ]+, [A-Z]{2}, [A-Za-z ]+$"
local_list=list()
for (i in 1:nrow(geocoded)){
  result <- tryCatch({
    tmp=geocode(paste0(geocoded[i,2],",", geocoded[i,1]), output = "all")
    tmp=tmp$results
    tmp_vec=list()
    for (j in 1:length(tmp)){
      tmp_vec[[j]]=grepl(pattern, tmp[[j]]$formatted_address)
    }
    tmp=tmp[c(which(tmp_vec==TRUE))]
    tmp=tmp[1]
    local_list[[i]]=tmp[[1]]$formatted_address
  }, error = function(e){})
}
length_list=list()
for (i in 1:length(local_list)){
  length_list[[i]]=length(local_list[[i]])
}
length_list=do.call(rbind, length_list)
local_list[c(which(length_list==0))]=0
locality_vec=do.call(rbind, local_list)

sample_cities=c("New York, NY, USA", "Boston, MA, USA", "Brooklyn, NY, USA", "Chicago, IL, USA", "Cleveland, OH, USA",
                "Cuyahoga County, OH, USA", "Detroit, MI, USA", "Philadelphia, PA, USA", "Pittsburgh, PA, USA",
                "Baltimore, MD, USA", "Cincinnati, OH, USA", "St. Louis, MO, USA")
#no observations from st louis or cincinatti

geocoded$city=locality_vec
geocoded$sample_city=ifelse((geocoded$city %in% sample_cities)==TRUE, 1, 0)
addresses=cbind.data.frame(addresses, geocoded)
addresses_sample=addresses[which(addresses$sample_city==1),]
addresses_pre_1926=addresses
addresses_pre_1926_sample=addresses_sample
save(addresses_pre_1926, file = "intermediate_outputs/addresses_pre_1926.rda")
save(addresses_pre_1926_sample, file="intermediate_outputs/addresses_pre_1926_in_sample.rda")
