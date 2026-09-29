###############################################################################################################################
############## Step 1b: generating potential links between individuals in numident and census records  ########################
###############################################################################################################################


library(stringdist)
library(phonics)

  

######################################################
############ 1940 to 1940 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
load("input_data/step_1_misc_data/name_dict.rda")


#preliminary cleaning
census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1940=numident[which(numident$birth_year>=1930 & numident$birth_year<1940),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1940), ncol=3)
census_age_set[,1]=1941-as.numeric(numident_1940$birth_year)
census_age_set[,2]=1940-as.numeric(numident_1940$birth_year)
census_age_set[,3]=1939-as.numeric(numident_1940$birth_year)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$age)
numident_1940$short_name=gsub(" .*", "", numident_1940$first_name)
numident_1940=numident_1940[which(nchar(numident_1940$first_name)>0 & nchar(numident_1940$surname)>0),]
numident_1940$match_name_1=paste0(tolower(numident_1940$first_name), " ", tolower(numident_1940$surname))
numident_1940$match_name_2=paste0(tolower(numident_1940$short_name), " ", tolower(numident_1940$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1940)){
  strings[[i]]=paste0(numident_1940[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1940[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1940[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1940[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1940[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1940[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1940[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1940[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1940[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1940[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1940[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1940)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1940_1940.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1940)){
  if ((tolower(numident_1940[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1940[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1940[i,4])), census_1940$match_name, method = "lv")
      tmp=census_1940[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1940$match_name_1[i], 1, 
                          regexpr("_", numident_1940$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1940$match_name_2[i], 1, 
                           regexpr("_", numident_1940$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1940$match_name_1[i], 1, 
                          regexpr("_", numident_1940$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1940$match_name_2[i], 1, 
                           regexpr("_", numident_1940$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1940)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1940), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1940_1940.rda")

rm(list = ls())
gc()

######################################################
############ 1930 to 1940 ############################
#####################################################

load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1930=numident[which(numident$birth_year>=1920 & numident$birth_year<1930),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1930), ncol=3)
census_age_set[,1]=1941-as.numeric(numident_1930$birth_year)
census_age_set[,2]=1940-as.numeric(numident_1930$birth_year)
census_age_set[,3]=1939-as.numeric(numident_1930$birth_year)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$age)
numident_1930$short_name=gsub(" .*", "", numident_1930$first_name)
numident_1930=numident_1930[which(nchar(numident_1930$first_name)>0 & nchar(numident_1930$surname)>0),]
numident_1930$match_name_1=paste0(tolower(numident_1930$first_name), " ", tolower(numident_1930$surname))
numident_1930$match_name_2=paste0(tolower(numident_1930$short_name), " ", tolower(numident_1930$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  strings[[i]]=paste0(numident_1930[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1930[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1930[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1930_1940.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  if ((tolower(numident_1930[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1930[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1930[i,4])), census_1940$match_name, method = "lv")
      tmp=census_1940[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1930$match_name_1[i], 1, 
                          regexpr("_", numident_1930$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1930$match_name_2[i], 1, 
                           regexpr("_", numident_1930$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1930$match_name_1[i], 1, 
                          regexpr("_", numident_1930$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1930$match_name_2[i], 1, 
                           regexpr("_", numident_1930$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1930_1940.rda")
rm(list = ls())
gc()

######################################################
############ 1930 to 1930 ############################
#####################################################

load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
load("input_data/step_1_misc_data/name_dict.rda")
census_1930 <- read.csv("input_data/census_restricted/census_1930.csv")

#preliminary cleaning
census_1930=census_1930[which(is.na(census_1930$namefrst)==FALSE),]
census_1930=census_1930[which(is.na(census_1930$namelast)==FALSE),]
census_1930=census_1930[which(census_1930$namefrst!=""),]
census_1930=census_1930[which(census_1930$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1930=numident[which(numident$birth_year>=1920 & numident$birth_year<1930),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1930), ncol=3)
census_age_set[,1]=1931-as.numeric(numident_1930$birth_year)
census_age_set[,2]=1930-as.numeric(numident_1930$birth_year)
census_age_set[,3]=1929-as.numeric(numident_1930$birth_year)
census_1930$match_name=paste0(tolower(census_1930$namefrst), " ", tolower(census_1930$namelast))
census_1930$match_string=paste0(census_1930$match_name,"_",census_1930$age)
numident_1930$short_name=gsub(" .*", "", numident_1930$first_name)
numident_1930=numident_1930[which(nchar(numident_1930$first_name)>0 & nchar(numident_1930$surname)>0),]
numident_1930$match_name_1=paste0(tolower(numident_1930$first_name), " ", tolower(numident_1930$surname))
numident_1930$match_name_2=paste0(tolower(numident_1930$short_name), " ", tolower(numident_1930$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  strings[[i]]=paste0(numident_1930[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1930[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1930[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1930[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1930[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1930_1930.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  if ((tolower(numident_1930[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1930[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1930[i,4])), census_1930$match_name, method = "lv")
      tmp=census_1930[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1930$match_name_1[i], 1, 
                          regexpr("_", numident_1930$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1930$match_name_2[i], 1, 
                           regexpr("_", numident_1930$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1930$match_name_1[i], 1, 
                          regexpr("_", numident_1930$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1930$match_name_2[i], 1, 
                           regexpr("_", numident_1930$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1930)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1930), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1930_1930.rda")
rm(list = ls())
gc()
######################################################
############ 1920 to 1940 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1920=numident[which(numident$birth_year>=1910 & numident$birth_year<1920),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1920), ncol=3)
census_age_set[,1]=1941-as.numeric(numident_1920$birth_year)
census_age_set[,2]=1940-as.numeric(numident_1920$birth_year)
census_age_set[,3]=1939-as.numeric(numident_1920$birth_year)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$age)
numident_1920$short_name=gsub(" .*", "", numident_1920$first_name)
numident_1920=numident_1920[which(nchar(numident_1920$first_name)>0 & nchar(numident_1920$surname)>0),]
numident_1920$match_name_1=paste0(tolower(numident_1920$first_name), " ", tolower(numident_1920$surname))
numident_1920$match_name_2=paste0(tolower(numident_1920$short_name), " ", tolower(numident_1920$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  strings[[i]]=paste0(numident_1920[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1920[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1920_1940.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if ((tolower(numident_1920[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1920[i,4])), census_1940$match_name, method = "lv")
      tmp=census_1940[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1920_1940.rda")
rm(list = ls())
gc()
######################################################
############ 1920 to 1930 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1930 <- read.csv("input_data/census_restricted/census_1930.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1930=census_1930[which(is.na(census_1930$namefrst)==FALSE),]
census_1930=census_1930[which(is.na(census_1930$namelast)==FALSE),]
census_1930=census_1930[which(census_1930$namefrst!=""),]
census_1930=census_1930[which(census_1930$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1920=numident[which(numident$birth_year>=1910 & numident$birth_year<1920),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1920), ncol=3)
census_age_set[,1]=1931-as.numeric(numident_1920$birth_year)
census_age_set[,2]=1930-as.numeric(numident_1920$birth_year)
census_age_set[,3]=1929-as.numeric(numident_1920$birth_year)
census_1930$match_name=paste0(tolower(census_1930$namefrst), " ", tolower(census_1930$namelast))
census_1930$match_string=paste0(census_1930$match_name,"_",census_1930$age)
numident_1920$short_name=gsub(" .*", "", numident_1920$first_name)
numident_1920=numident_1920[which(nchar(numident_1920$first_name)>0 & nchar(numident_1920$surname)>0),]
numident_1920$match_name_1=paste0(tolower(numident_1920$first_name), " ", tolower(numident_1920$surname))
numident_1920$match_name_2=paste0(tolower(numident_1920$short_name), " ", tolower(numident_1920$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  strings[[i]]=paste0(numident_1920[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1920[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1920_1930.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if ((tolower(numident_1920[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1920[i,4])), census_1930$match_name, method = "lv")
      tmp=census_1930[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1920_1930.rda")
rm(list = ls())
gc()
######################################################
############ 1920 to 1920 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1920 <- read.csv("input_data/census_restricted/census_1920.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1920=census_1920[which(is.na(census_1920$namefrst)==FALSE),]
census_1920=census_1920[which(is.na(census_1920$namelast)==FALSE),]
census_1920=census_1920[which(census_1920$namefrst!=""),]
census_1920=census_1920[which(census_1920$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1920=numident[which(numident$birth_year>=1910 & numident$birth_year<1920),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1920), ncol=3)
census_age_set[,1]=1921-as.numeric(numident_1920$birth_year)
census_age_set[,2]=1920-as.numeric(numident_1920$birth_year)
census_age_set[,3]=1919-as.numeric(numident_1920$birth_year)
census_1920$match_name=paste0(tolower(census_1920$namefrst), " ", tolower(census_1920$namelast))
census_1920$match_string=paste0(census_1920$match_name,"_",census_1920$age)
numident_1920$short_name=gsub(" .*", "", numident_1920$first_name)
numident_1920=numident_1920[which(nchar(numident_1920$first_name)>0 & nchar(numident_1920$surname)>0),]
numident_1920$match_name_1=paste0(tolower(numident_1920$first_name), " ", tolower(numident_1920$surname))
numident_1920$match_name_2=paste0(tolower(numident_1920$short_name), " ", tolower(numident_1920$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  strings[[i]]=paste0(numident_1920[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1920[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1920[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1920[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1920_1920.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if ((tolower(numident_1920[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1920[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1920[i,4])), census_1920$match_name, method = "lv")
      tmp=census_1920[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1920$match_name_1[i], 1, 
                          regexpr("_", numident_1920$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1920$match_name_2[i], 1, 
                           regexpr("_", numident_1920$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1920)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1920), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1920_1920.rda")
rm(list = ls())
gc()
######################################################
############ 1910 to 1940 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
load("input_data/step_1_misc_data/name_dict.rda")


#preliminary cleaning

census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1910=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1910), ncol=3)
census_age_set[,1]=1941-as.numeric(numident_1910$birth_year)
census_age_set[,2]=1940-as.numeric(numident_1910$birth_year)
census_age_set[,3]=1939-as.numeric(numident_1910$birth_year)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$age)
numident_1910$short_name=gsub(" .*", "", numident_1910$first_name)
numident_1910=numident_1910[which(nchar(numident_1910$first_name)>0 & nchar(numident_1910$surname)>0),]
numident_1910$match_name_1=paste0(tolower(numident_1910$first_name), " ", tolower(numident_1910$surname))
numident_1910$match_name_2=paste0(tolower(numident_1910$short_name), " ", tolower(numident_1910$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  strings[[i]]=paste0(numident_1910[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1910[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1910_1940.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if ((tolower(numident_1910[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1910[i,4])), census_1940$match_name, method = "lv")
      tmp=census_1940[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1910_1940.rda")
rm(list = ls())
gc()
######################################################
############ 1910 to 1930 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1930 <- read.csv("input_data/census_restricted/census_1930.csv")
load("input_data/step_1_misc_data/name_dict.rda")


#preliminary cleaning

census_1930=census_1930[which(is.na(census_1930$namefrst)==FALSE),]
census_1930=census_1930[which(is.na(census_1930$namelast)==FALSE),]
census_1930=census_1930[which(census_1930$namefrst!=""),]
census_1930=census_1930[which(census_1930$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1910=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1910), ncol=3)
census_age_set[,1]=1931-as.numeric(numident_1910$birth_year)
census_age_set[,2]=1930-as.numeric(numident_1910$birth_year)
census_age_set[,3]=1929-as.numeric(numident_1910$birth_year)
census_1930$match_name=paste0(tolower(census_1930$namefrst), " ", tolower(census_1930$namelast))
census_1930$match_string=paste0(census_1930$match_name,"_",census_1930$age)
numident_1910$short_name=gsub(" .*", "", numident_1910$first_name)
numident_1910=numident_1910[which(nchar(numident_1910$first_name)>0 & nchar(numident_1910$surname)>0),]
numident_1910$match_name_1=paste0(tolower(numident_1910$first_name), " ", tolower(numident_1910$surname))
numident_1910$match_name_2=paste0(tolower(numident_1910$short_name), " ", tolower(numident_1910$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  strings[[i]]=paste0(numident_1910[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1910[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1910_1930.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if ((tolower(numident_1910[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1910[i,4])), census_1930$match_name, method = "lv")
      tmp=census_1930[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1910_1930.rda")
rm(list = ls())
gc()
######################################################
############ 1910 to 1920 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1920 <- read.csv("input_data/census_restricted/census_1920.csv")
load("input_data/step_1_misc_data/name_dict.rda")


#preliminary cleaning

census_1920=census_1920[which(is.na(census_1920$namefrst)==FALSE),]
census_1920=census_1920[which(is.na(census_1920$namelast)==FALSE),]
census_1920=census_1920[which(census_1920$namefrst!=""),]
census_1920=census_1920[which(census_1920$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1910=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1910), ncol=3)
census_age_set[,1]=1921-as.numeric(numident_1910$birth_year)
census_age_set[,2]=1920-as.numeric(numident_1910$birth_year)
census_age_set[,3]=1919-as.numeric(numident_1910$birth_year)
census_1920$match_name=paste0(tolower(census_1920$namefrst), " ", tolower(census_1920$namelast))
census_1920$match_string=paste0(census_1920$match_name,"_",census_1920$age)
numident_1910$short_name=gsub(" .*", "", numident_1910$first_name)
numident_1910=numident_1910[which(nchar(numident_1910$first_name)>0 & nchar(numident_1910$surname)>0),]
numident_1910$match_name_1=paste0(tolower(numident_1910$first_name), " ", tolower(numident_1910$surname))
numident_1910$match_name_2=paste0(tolower(numident_1910$short_name), " ", tolower(numident_1910$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  strings[[i]]=paste0(numident_1910[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1910[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1910_1920.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if ((tolower(numident_1910[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1910[i,4])), census_1920$match_name, method = "lv")
      tmp=census_1920[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1910_1920.rda")
rm(list = ls())
gc()
######################################################
############ 1910 to 1910 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1910 <- read.csv("input_data/census_restricted/census_1910.csv")
load("input_data/step_1_misc_data/name_dict.rda")


#preliminary cleaning

census_1910=census_1910[which(is.na(census_1910$namefrst)==FALSE),]
census_1910=census_1910[which(is.na(census_1910$namelast)==FALSE),]
census_1910=census_1910[which(census_1910$namefrst!=""),]
census_1910=census_1910[which(census_1910$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1910=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1910), ncol=3)
census_age_set[,1]=1911-as.numeric(numident_1910$birth_year)
census_age_set[,2]=1910-as.numeric(numident_1910$birth_year)
census_age_set[,3]=1909-as.numeric(numident_1910$birth_year)
census_1910$match_name=paste0(tolower(census_1910$namefrst), " ", tolower(census_1910$namelast))
census_1910$match_string=paste0(census_1910$match_name,"_",census_1910$age)
numident_1910$short_name=gsub(" .*", "", numident_1910$first_name)
numident_1910=numident_1910[which(nchar(numident_1910$first_name)>0 & nchar(numident_1910$surname)>0),]
numident_1910$match_name_1=paste0(tolower(numident_1910$first_name), " ", tolower(numident_1910$surname))
numident_1910$match_name_2=paste0(tolower(numident_1910$short_name), " ", tolower(numident_1910$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  strings[[i]]=paste0(numident_1910[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1910[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1910[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1910[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1910$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1910_1910.rda")

#second round for misspellings

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if ((tolower(numident_1910[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1910[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1910[i,4])), census_1910$match_name, method = "lv")
      tmp=census_1910[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1910$match_name, method = "lv")
    tmp=census_1910[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1910$match_name, method = "lv")
    tmp2=census_1910[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1910$match_name_1[i], 1, 
                          regexpr("_", numident_1910$match_name_1[i]) - 1), census_1910$match_name, method = "lv")
    tmp=census_1910[which(tmp<2),]
    tmp2=stringdist(substr(numident_1910$match_name_2[i], 1, 
                           regexpr("_", numident_1910$match_name_2[i]) - 1), census_1910$match_name, method = "lv")
    tmp2=census_1910[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1910)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1910$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1910), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1910_1910.rda")
rm(list = ls())
gc()
######################################################
############ 1900 to 1940 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1940 <- read.csv("input_data/census_restricted/census_1940.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1940=census_1940[which(is.na(census_1940$namefrst)==FALSE),]
census_1940=census_1940[which(is.na(census_1940$namelast)==FALSE),]
census_1940=census_1940[which(census_1940$namefrst!=""),]
census_1940=census_1940[which(census_1940$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1900=numident[which(numident$birth_year<1900),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1900), ncol=3)
census_age_set[,1]=1941-as.numeric(numident_1900$birth_year)
census_age_set[,2]=1940-as.numeric(numident_1900$birth_year)
census_age_set[,3]=1939-as.numeric(numident_1900$birth_year)
census_1940$match_name=paste0(tolower(census_1940$namefrst), " ", tolower(census_1940$namelast))
census_1940$match_string=paste0(census_1940$match_name,"_",census_1940$age)
numident_1900$short_name=gsub(" .*", "", numident_1900$first_name)
numident_1900=numident_1900[which(nchar(numident_1900$first_name)>0 & nchar(numident_1900$surname)>0),]
numident_1900$match_name_1=paste0(tolower(numident_1900$first_name), " ", tolower(numident_1900$surname))
numident_1900$match_name_2=paste0(tolower(numident_1900$short_name), " ", tolower(numident_1900$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  strings[[i]]=paste0(numident_1900[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1900[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1900_1940.rda")

#second round for misspellings
library(stringdist)

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if ((tolower(numident_1900[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1900[i,4])), census_1940$match_name, method = "lv")
      tmp=census_1940[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1940$match_name, method = "lv")
    tmp=census_1940[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1940$match_name, method = "lv")
    tmp2=census_1940[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1940$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1940[which(census_1940$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1940[which(census_1940$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1940[which(census_1940$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1900_1940.rda")

rm(list = ls())
gc()
######################################################
############ 1900 to 1930 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1930 <- read.csv("input_data/census_restricted/census_1930.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1930=census_1930[which(is.na(census_1930$namefrst)==FALSE),]
census_1930=census_1930[which(is.na(census_1930$namelast)==FALSE),]
census_1930=census_1930[which(census_1930$namefrst!=""),]
census_1930=census_1930[which(census_1930$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1900=numident[which(numident$birth_year<1900),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1900), ncol=3)
census_age_set[,1]=1931-as.numeric(numident_1900$birth_year)
census_age_set[,2]=1930-as.numeric(numident_1900$birth_year)
census_age_set[,3]=1929-as.numeric(numident_1900$birth_year)
census_1930$match_name=paste0(tolower(census_1930$namefrst), " ", tolower(census_1930$namelast))
census_1930$match_string=paste0(census_1930$match_name,"_",census_1930$age)
numident_1900$short_name=gsub(" .*", "", numident_1900$first_name)
numident_1900=numident_1900[which(nchar(numident_1900$first_name)>0 & nchar(numident_1900$surname)>0),]
numident_1900$match_name_1=paste0(tolower(numident_1900$first_name), " ", tolower(numident_1900$surname))
numident_1900$match_name_2=paste0(tolower(numident_1900$short_name), " ", tolower(numident_1900$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  strings[[i]]=paste0(numident_1900[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1900[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1900_1930.rda")

#second round for misspellings
library(stringdist)

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if ((tolower(numident_1900[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1900[i,4])), census_1930$match_name, method = "lv")
      tmp=census_1930[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1930$match_name, method = "lv")
    tmp=census_1930[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1930$match_name, method = "lv")
    tmp2=census_1930[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1930$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1930[which(census_1930$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1930[which(census_1930$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1930[which(census_1930$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1900_1930.rda")

rm(list = ls())
gc()
######################################################
############ 1900 to 1920 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1920 <- read.csv("input_data/census_restricted/census_1920.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1920=census_1920[which(is.na(census_1920$namefrst)==FALSE),]
census_1920=census_1920[which(is.na(census_1920$namelast)==FALSE),]
census_1920=census_1920[which(census_1920$namefrst!=""),]
census_1920=census_1920[which(census_1920$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1900=numident[which(numident$birth_year<1900),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1900), ncol=3)
census_age_set[,1]=1921-as.numeric(numident_1900$birth_year)
census_age_set[,2]=1920-as.numeric(numident_1900$birth_year)
census_age_set[,3]=1919-as.numeric(numident_1900$birth_year)
census_1920$match_name=paste0(tolower(census_1920$namefrst), " ", tolower(census_1920$namelast))
census_1920$match_string=paste0(census_1920$match_name,"_",census_1920$age)
numident_1900$short_name=gsub(" .*", "", numident_1900$first_name)
numident_1900=numident_1900[which(nchar(numident_1900$first_name)>0 & nchar(numident_1900$surname)>0),]
numident_1900$match_name_1=paste0(tolower(numident_1900$first_name), " ", tolower(numident_1900$surname))
numident_1900$match_name_2=paste0(tolower(numident_1900$short_name), " ", tolower(numident_1900$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  strings[[i]]=paste0(numident_1900[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1900[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1900_1920.rda")

#second round for misspellings
library(stringdist)

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if ((tolower(numident_1900[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1900[i,4])), census_1920$match_name, method = "lv")
      tmp=census_1920[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1920$match_name, method = "lv")
    tmp=census_1920[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1920$match_name, method = "lv")
    tmp2=census_1920[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1920$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1920[which(census_1920$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1920[which(census_1920$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1920[which(census_1920$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1900_1920.rda")
rm(list = ls())
gc()
######################################################
############ 1900 to 1910 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1910 <- read.csv("input_data/census_restricted/census_1910.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1910=census_1910[which(is.na(census_1910$namefrst)==FALSE),]
census_1910=census_1910[which(is.na(census_1910$namelast)==FALSE),]
census_1910=census_1910[which(census_1910$namefrst!=""),]
census_1910=census_1910[which(census_1910$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1900=numident[which(numident$birth_year<1900),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1900), ncol=3)
census_age_set[,1]=1911-as.numeric(numident_1900$birth_year)
census_age_set[,2]=1910-as.numeric(numident_1900$birth_year)
census_age_set[,3]=1909-as.numeric(numident_1900$birth_year)
census_1910$match_name=paste0(tolower(census_1910$namefrst), " ", tolower(census_1910$namelast))
census_1910$match_string=paste0(census_1910$match_name,"_",census_1910$age)
numident_1900$short_name=gsub(" .*", "", numident_1900$first_name)
numident_1900=numident_1900[which(nchar(numident_1900$first_name)>0 & nchar(numident_1900$surname)>0),]
numident_1900$match_name_1=paste0(tolower(numident_1900$first_name), " ", tolower(numident_1900$surname))
numident_1900$match_name_2=paste0(tolower(numident_1900$short_name), " ", tolower(numident_1900$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  strings[[i]]=paste0(numident_1900[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1900[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1910$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1900_1910.rda")

#second round for misspellings
library(stringdist)

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if ((tolower(numident_1900[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1900[i,4])), census_1910$match_name, method = "lv")
      tmp=census_1910[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1910$match_name, method = "lv")
    tmp=census_1910[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1910$match_name, method = "lv")
    tmp2=census_1910[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1910$match_name, method = "lv")
    tmp=census_1910[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1910$match_name, method = "lv")
    tmp2=census_1910[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1910$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1910[which(census_1910$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1910[which(census_1910$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1910[which(census_1910$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1900_1910.rda")

rm(list = ls())
gc()
######################################################
############ 1900 to 1900 ############################
#####################################################
load("intermediate_outputs/sicilian_numident.rda")
numident=numident[,c(1:12)] #restoring column positions temporarily because of over time variable addiitons for later stages
census_1900 <- read.csv("input_data/census_restricted/census_1900.csv")
load("input_data/step_1_misc_data/name_dict.rda")

#preliminary cleaning
census_1900=census_1900[which(is.na(census_1900$namefrst)==FALSE),]
census_1900=census_1900[which(is.na(census_1900$namelast)==FALSE),]
census_1900=census_1900[which(census_1900$namefrst!=""),]
census_1900=census_1900[which(census_1900$namelast!=""),]

numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
#limiting numident to those alive in 1900
numident_1900=numident[which(numident$birth_year<1900),]

#key generation
census_age_set=matrix(nrow=nrow(numident_1900), ncol=3)
census_age_set[,1]=1901-as.numeric(numident_1900$birth_year)
census_age_set[,2]=1900-as.numeric(numident_1900$birth_year)
census_age_set[,3]=1899-as.numeric(numident_1900$birth_year)
census_1900$match_name=paste0(tolower(census_1900$namefrst), " ", tolower(census_1900$namelast))
census_1900$match_string=paste0(census_1900$match_name,"_",census_1900$age)
numident_1900$short_name=gsub(" .*", "", numident_1900$first_name)
numident_1900=numident_1900[which(nchar(numident_1900$first_name)>0 & nchar(numident_1900$surname)>0),]
numident_1900$match_name_1=paste0(tolower(numident_1900$first_name), " ", tolower(numident_1900$surname))
numident_1900$match_name_2=paste0(tolower(numident_1900$short_name), " ", tolower(numident_1900$surname))


all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)


strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  strings[[i]]=paste0(numident_1900[i,15],"_",census_age_set[i,1])
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,1]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,2]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,15],"_",census_age_set[i,3]))
  strings[[i]]=append(strings[[i]], paste0(numident_1900[i,16],"_",census_age_set[i,3]))
  if ((tolower(numident_1900[i,3]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    for (j in 1:n_alt_names){
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,1]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,2]))
      strings[[i]]=append(strings[[i]],
                          paste0(tolower(alt_names[j])," ", tolower(numident_1900[i,4]),"_",census_age_set[i,3]))
    }
    
  }else{
    print("0")
  }
  strings[[i]]=unique(strings[[i]])
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above key generation, creates match keys for exact name for the age range of individual at time of census
#it repeats process for shortened version of first name (i.e. John Salvator Surname as John Surname)
#it then repeats for likely anglicanizations of first name
#then limits to unique values


#matching on likely keys
direct_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  tmp_list=list()
  mat=matrix(nrow=length(strings[[i]]), ncol=4)
  mat[,1]=strings[[i]]
  tmp_list_2=list()
  for (j in 1:nrow(mat)){
    mat[j,2]=ifelse((mat[j,1] %in% census_1900$match_string)==TRUE, 1, 0)
    if (mat[j,2]==1){
      if (length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"])>1){
        tmp=matrix(nrow=length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]), 
                   ncol=4)
        tmp[,1]=rep(mat[j,1], times=length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]))
        tmp[,2]=1
        for (z in 1:length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"])){
          tmp[z,3]=census_1900[which(census_1900$match_string==mat[j,1]), "match_string"][z]
          tmp[z,4]=census_1900[which(census_1900$match_string==mat[j,1]), "histid"][z]
        }
        tmp_list_2[[j]]=tmp
      } else {
        mat[j,3]=census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]
        mat[j,4]=census_1900[which(census_1900$match_string==mat[j,1]), "histid"]
      }
    }
    else {
      mat[j,3]=NA
      mat[j,4]=NA
    }
  }
  tmp_list[[i]]=do.call(rbind, tmp_list_2)
  if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
    if (length(tmp_list)>0){
      direct_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
    } else{
      direct_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
    }
  } else{
    direct_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all direct matches (and what was matched)

save(direct_matches, file="intermediate_outputs/direct_matches_1900_1900.rda")

#second round for misspellings
library(stringdist)

#key generation
fuzzy_strings=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if ((tolower(numident_1900[i,14]) %in% tolower(first_names_ita))==TRUE){
    alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(numident_1900[i,14]))]][2])
    alt_names=alt_names[which(alt_names!="none")]
    n_alt_names=length(alt_names)
  } else{n_alt_names=0}
  if (n_alt_names>0){
    ita_fuzzy=list()
    for (z in 1:n_alt_names){
      tmp=stringdist(paste0(tolower(alt_names[z])," ", tolower(numident_1900[i,4])), census_1900$match_name, method = "lv")
      tmp=census_1900[which(tmp<2),]
      ita_fuzzy[[z]]=tmp
    }
    ita_fuzzy=do.call(rbind.data.frame, ita_fuzzy)
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1900$match_name, method = "lv")
    tmp=census_1900[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1900$match_name, method = "lv")
    tmp2=census_1900[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
    tmp=unique(rbind.data.frame(tmp, ita_fuzzy))
  } else{
    tmp=stringdist(substr(numident_1900$match_name_1[i], 1, 
                          regexpr("_", numident_1900$match_name_1[i]) - 1), census_1900$match_name, method = "lv")
    tmp=census_1900[which(tmp<2),]
    tmp2=stringdist(substr(numident_1900$match_name_2[i], 1, 
                           regexpr("_", numident_1900$match_name_2[i]) - 1), census_1900$match_name, method = "lv")
    tmp2=census_1900[which(tmp2<2),]
    tmp=unique(rbind.data.frame(tmp, tmp2))
  }
  if (nrow(tmp)>0){
    for (j in 1:nrow(tmp)){
      fuzzy_strings[[i]]=paste0(tmp$match_name[j],"_",census_age_set[i,1])
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,2]))
      fuzzy_strings[[i]]=append(fuzzy_strings[[i]], paste0(tmp$match_name[j],"_",census_age_set[i,3]))
    }
    fuzzy_strings[[i]]=unique(fuzzy_strings[[i]])
  } else {
    fuzzy_strings[[i]]=0
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}  

#this creates match strings for fuzzy matches on both given name and anglicanized name if first name is italian
# match strings include same age range as before, but now include fuzzy matches
# first search for fuzzy match of name only based on LV distance. Limit to 1 change or less for misspel
#then pull those names from census and add the age range string from indvidiual as known
# next stage will look for census records again with age range and fuzzy match condition met
#broken into two steps because not wanting to incorporate fuzzy matching on age component of match string


#now matching those keys into census and saving records of matches

fuzzy_matches=list()
timeNow <- Sys.time()
for (i in 1:nrow(numident_1900)){
  if (fuzzy_strings[[i]][1] != 0){ 
    tmp_list=list()
    mat=matrix(nrow=length(fuzzy_strings[[i]]), ncol=4)
    mat[,1]=fuzzy_strings[[i]]
    tmp_list_2=list()
    for (j in 1:nrow(mat)){
      mat[j,2]=ifelse((mat[j,1] %in% census_1900$match_string)==TRUE, 1, 0)
      if (mat[j,2]==1){
        if (length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"])>1){
          tmp=matrix(nrow=length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]), 
                     ncol=4)
          tmp[,1]=rep(mat[j,1], times=length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]))
          tmp[,2]=1
          for (z in 1:length(census_1900[which(census_1900$match_string==mat[j,1]), "match_string"])){
            tmp[z,3]=census_1900[which(census_1900$match_string==mat[j,1]), "match_string"][z]
            tmp[z,4]=census_1900[which(census_1900$match_string==mat[j,1]), "histid"][z]
          }
          tmp_list_2[[j]]=tmp
        } else {
          mat[j,3]=census_1900[which(census_1900$match_string==mat[j,1]), "match_string"]
          mat[j,4]=census_1900[which(census_1900$match_string==mat[j,1]), "histid"]
        }
      }
      else {
        mat[j,3]=NA
        mat[j,4]=NA
      }
    }
    tmp_list[[i]]=do.call(rbind, tmp_list_2)
    if (length(complete.cases(mat)[which(complete.cases(mat)==TRUE)])>0){
      if (length(tmp_list)>0){
        fuzzy_matches[[i]]=rbind(mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),], tmp_list[[i]]) 
      } else{
        fuzzy_matches[[i]]=mat[which(mat[,2]==1 & (is.na(mat[,3])==FALSE)),]
      }
    } else{
      fuzzy_matches[[i]]=NA
    }
  } else{
    fuzzy_matches[[i]]=NA
  }
  cat("\r", round(i*100/nrow(numident_1900), 2), "% done in ", Sys.time() - timeNow, " ... ")
}

#the above loop stores all fuzzy matches (and what was matched)

save(fuzzy_matches, file="intermediate_outputs/fuzzy_matches_1900_1900.rda")
rm(list = ls())
gc()

