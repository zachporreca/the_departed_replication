###############################################################################################################################
################### Step 1g: Matching indiduals over time via CensusTree cross-census linkages and ############################
###################           selecting best match based on parent names                           ############################
###############################################################################################################################


###############################################
###################  1900 #####################
###############################################

load("intermediate_outputs/step_1_potential_census_matches/matches_1900_1940.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1900_1930.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1900_1920.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1900_1910.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1900_1900.rda")


#now to combine lists into singular individual level record and drop no matches
#combining
cohort_1900=list()
for (i in 1:length(matches_1900_1940)){
  tmp=list()
  tmp[[1]]=i
  tmp[[2]]=matches_1900_1900[[i]]
  tmp[[3]]=matches_1900_1910[[i]]
  tmp[[4]]=matches_1900_1920[[i]]
  tmp[[5]]=matches_1900_1930[[i]]
  tmp[[6]]=matches_1900_1940[[i]]
  cohort_1900[[i]]=tmp
}

#dropping no matches
double_na = function(x, y, z, f, g){!(is.na(x) & is.na(y) & is.na(z) & is.na(f)  & is.na(g))}
keep_indices=vector(length = length(cohort_1900))
for (i in 1:length(cohort_1900)){
  keep_indices[i]=double_na(cohort_1900[[i]][2], cohort_1900[[i]][3], cohort_1900[[i]][4], cohort_1900[[i]][5], cohort_1900[[i]][6])
}
keep_indices=which(keep_indices==TRUE)
cohort_1900=cohort_1900[keep_indices]

#counting number of records matched per numident individual
test_1=vector(length=length(cohort_1900))
test_2=vector(length=length(cohort_1900))
test_3=vector(length=length(cohort_1900))
test_4=vector(length=length(cohort_1900))
test_5=vector(length=length(cohort_1900))


for (i in 1:length(cohort_1900)){
  if (is.na(cohort_1900[[i]][2])==TRUE){
    test_1[i]=0
  } else{
    test_1[i]=nrow(as.data.frame(cohort_1900[[i]][2]))
  }
  if (is.na(cohort_1900[[i]][3])==TRUE){
    test_2[i]=0
  } else{
    test_2[i]=nrow(as.data.frame(cohort_1900[[i]][3]))
  }
  if (is.na(cohort_1900[[i]][3])==TRUE){
    test_3[i]=0
  } else{
    test_3[i]=nrow(as.data.frame(cohort_1900[[i]][4]))
  }
  if (is.na(cohort_1900[[i]][4])==TRUE){
    test_4[i]=0
  } else{
    test_4[i]=nrow(as.data.frame(cohort_1900[[i]][5]))
  }
  if (is.na(cohort_1900[[i]][5])==TRUE){
    test_5[i]=0
  } else{
    test_5[i]=nrow(as.data.frame(cohort_1900[[i]][6]))
  }
}
table(test_1)
table(test_2)
table(test_3)
table(test_4)
table(test_5)
table(test_1+test_2+test_3+test_4+test_5)

#creating structure for bringing in census tree matching
list_1=list()
list_2=list()
list_3=list()
list_4=list()
list_5=list()
for (i in 1:length(cohort_1900)){
  if (is.na(cohort_1900[[i]][2])){
    cohort_1900[[i]][2]=NA
  } else{
    tmp=as.data.frame(cohort_1900[[i]][2])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1900[[i]][1])
    tmp$census_year=1900
    list_1[[i]]=tmp
  }
  if (is.na(cohort_1900[[i]][3])){
    cohort_1900[[i]][3]=NA
  } else{
    tmp=as.data.frame(cohort_1900[[i]][3])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1900[[i]][1])
    tmp$census_year=1910
    list_2[[i]]=tmp
  }
  if (is.na(cohort_1900[[i]][4])){
    cohort_1900[[i]][4]=NA
  } else{
    tmp=as.data.frame(cohort_1900[[i]][4])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1900[[i]][1])
    tmp$census_year=1920
    list_3[[i]]=tmp
  }
  if (is.na(cohort_1900[[i]][5])){
    cohort_1900[[i]][5]=NA
  } else{
    tmp=as.data.frame(cohort_1900[[i]][5])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1900[[i]][1])
    tmp$census_year=1930
    list_4[[i]]=tmp
  }
  if (is.na(cohort_1900[[i]][6])){
    cohort_1900[[i]][6]=NA
  } else{
    tmp=as.data.frame(cohort_1900[[i]][6])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1900[[i]][1])
    tmp$census_year=1940
    list_5[[i]]=tmp
  }
}
list_1=do.call(rbind.data.frame, list_1)
list_2=do.call(rbind.data.frame, list_2)
list_3=do.call(rbind.data.frame, list_3)
list_4=do.call(rbind.data.frame, list_4)
list_5=do.call(rbind.data.frame, list_5)

cohort_1900=rbind.data.frame(list_1, list_2)
cohort_1900=rbind.data.frame(cohort_1900, list_3)
cohort_1900=rbind.data.frame(cohort_1900, list_4)
cohort_1900=rbind.data.frame(cohort_1900, list_5)
length(unique(cohort_1900$cohort_member_id))/nrow(cohort_1900)

#now to load census tree and see if same matchee individuals are apeparing twice
ct_1930_1940=read.csv(file="input_data/census_tree_crosswalks/1930_1940.csv")
ct_1920_1930=read.csv(file="input_data/census_tree_crosswalks/1920_1930.csv")
ct_1910_1920=read.csv(file="input_data/census_tree_crosswalks/1910_1920.csv")
ct_1900_1910=read.csv(file="input_data/census_tree_crosswalks/1900_1910.csv")

cohort_1900_1900=cohort_1900[which(cohort_1900$census_year==1900),]
cohort_1900_1910=cohort_1900[which(cohort_1900$census_year==1910),]
cohort_1900_1920=cohort_1900[which(cohort_1900$census_year==1920),]
cohort_1900_1930=cohort_1900[which(cohort_1900$census_year==1930),]
cohort_1900_1940=cohort_1900[which(cohort_1900$census_year==1940),]

cohort_1900_1900$histid_ct_1900=cohort_1900_1900$histid_match
cohort_1900_1900$histid_ct_1910=ct_1900_1910[match(cohort_1900_1900$histid_match, ct_1900_1910$histid1900), "histid1910"]
cohort_1900_1900$histid_ct_1920=ct_1910_1920[match(cohort_1900_1900$histid_ct_1910, ct_1910_1920$histid1910), "histid1920"]
cohort_1900_1900$histid_ct_1930=ct_1920_1930[match(cohort_1900_1900$histid_ct_1920, ct_1920_1930$histid1920), "histid1930"]
cohort_1900_1900$histid_ct_1940=ct_1930_1940[match(cohort_1900_1900$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1900_1910$histid_ct_1900=ct_1900_1910[match(cohort_1900_1910$histid_match, ct_1900_1910$histid1910), "histid1900"]
cohort_1900_1910$histid_ct_1910=cohort_1900_1910$histid_match
cohort_1900_1910$histid_ct_1920=ct_1910_1920[match(cohort_1900_1910$histid_match, ct_1910_1920$histid1910), "histid1920"]
cohort_1900_1910$histid_ct_1930=ct_1920_1930[match(cohort_1900_1910$histid_ct_1920, ct_1920_1930$histid1920), "histid1930"]
cohort_1900_1910$histid_ct_1940=ct_1930_1940[match(cohort_1900_1910$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1900_1920$histid_ct_1900=NA
cohort_1900_1920$histid_ct_1910=ct_1910_1920[match(cohort_1900_1920$histid_match, ct_1910_1920$histid1920), "histid1910"]
cohort_1900_1920$histid_ct_1900=ct_1900_1910[match(cohort_1900_1920$histid_ct_1910, ct_1910_1920$histid1910), "histid1900"]
cohort_1900_1920$histid_ct_1920=cohort_1900_1920$histid_match
cohort_1900_1920$histid_ct_1930=ct_1920_1930[match(cohort_1900_1920$histid_ct_1920, ct_1920_1930$histid1920), "histid1930"]
cohort_1900_1920$histid_ct_1940=ct_1930_1940[match(cohort_1900_1920$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1900_1930$histid_ct_1900=NA
cohort_1900_1930$histid_ct_1910=NA
cohort_1900_1930$histid_ct_1920=NA
cohort_1900_1930$histid_ct_1920=ct_1920_1930[match(cohort_1900_1930$histid_match, ct_1920_1930$histid1930), "histid1920"]
cohort_1900_1930$histid_ct_1910=ct_1910_1920[match(cohort_1900_1930$histid_ct_1920, ct_1910_1920$histid1920), "histid1910"]
cohort_1900_1930$histid_ct_1900=ct_1900_1910[match(cohort_1900_1930$histid_ct_1910, ct_1910_1920$histid1910), "histid1900"]
cohort_1900_1930$histid_ct_1930=cohort_1900_1930$histid_match
cohort_1900_1930$histid_ct_1940=ct_1930_1940[match(cohort_1900_1930$histid_match, ct_1930_1940$histid1930), "histid1940"]

cohort_1900_1940$histid_ct_1900=NA
cohort_1900_1940$histid_ct_1910=NA
cohort_1900_1940$histid_ct_1920=NA
cohort_1900_1940$histid_ct_1930=NA
cohort_1900_1940$histid_ct_1940=cohort_1900_1940$histid_match
cohort_1900_1940$histid_ct_1930=ct_1930_1940[match(cohort_1900_1940$histid_match, ct_1930_1940$histid1940), "histid1930"]
cohort_1900_1940$histid_ct_1920=ct_1920_1930[match(cohort_1900_1940$histid_ct_1930, ct_1920_1930$histid1930), "histid1920"]
cohort_1900_1940$histid_ct_1910=ct_1910_1920[match(cohort_1900_1940$histid_ct_1920, ct_1910_1920$histid1920), "histid1910"]
cohort_1900_1940$histid_ct_1900=ct_1900_1910[match(cohort_1900_1940$histid_ct_1910, ct_1910_1920$histid1910), "histid1900"]

cohort_1900=rbind.data.frame(cohort_1900_1900, cohort_1900_1910)
cohort_1900=rbind.data.frame(cohort_1900, cohort_1900_1920)
cohort_1900=rbind.data.frame(cohort_1900, cohort_1900_1930)
cohort_1900=rbind.data.frame(cohort_1900, cohort_1900_1940)


cohort_1900$individual_string=paste0(cohort_1900$histid_ct_1900, " ", cohort_1900$histid_ct_1910, " ", cohort_1900$histid_ct_1920, " ", cohort_1900$histid_ct_1930, " ", cohort_1900$histid_ct_1940) #indvidual identity strings

cohort=matrix(nrow=length(unique(cohort_1900$cohort_member_id)), ncol=2)
cohort[,1]=unique(cohort_1900$cohort_member_id)
for (i in 1:nrow(cohort)){
  cohort[i,2]=nrow(cohort_1900[which(cohort_1900[,"cohort_member_id"]==cohort[i,1]),])
}
multiples=cohort[which(cohort[,2]>1),1] #generating IDs for sample of cohort that matched to multiple indviduals
singles=cohort[which(cohort[,2]==1),1]
single_sample=cohort_1900[which(cohort_1900$cohort_member_id %in% singles),]
#indvidual identity strings to NA for NA
multiple_sample=cohort_1900[which(cohort_1900$cohort_member_id %in% multiples),]

#loading numident and relevant census samples
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident=numident[which(numident$birth_year<1900),]
numident$cohort_member_id=seq(1:nrow(numident))
numident=numident[which(numident$cohort_member_id %in% multiples),]
multiple_sample$mother_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "mother_name"])
multiple_sample$father_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "father_name"])
load("intermediate_outputs/extract_census_samples/households_1900.rda")
multiple_sample$hhid_1900=households_1900_histid[match(multiple_sample$histid_ct_1900, households_1900_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1910.rda")
multiple_sample$hhid_1910=households_1910_histid[match(multiple_sample$histid_ct_1910, households_1910_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1920.rda")
multiple_sample$hhid_1920=households_1920_histid[match(multiple_sample$histid_ct_1920, households_1920_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1930.rda")
multiple_sample$hhid_1930=households_1930_histid[match(multiple_sample$histid_ct_1930, households_1930_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1940.rda")
multiple_sample$hhid_1940=households_1940_histid[match(multiple_sample$histid_ct_1940, households_1940_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1930.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1920.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1910.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1900.rda")
multiple_sample$father_name_census_1900=0
multiple_sample$father_name_census_1910=0
multiple_sample$father_name_census_1920=0
multiple_sample$father_name_census_1930=0
multiple_sample$father_name_census_1940=0
multiple_sample$mother_name_census_1900=0
multiple_sample$mother_name_census_1910=0
multiple_sample$mother_name_census_1920=0
multiple_sample$mother_name_census_1930=0
multiple_sample$mother_name_census_1940=0
load("intermediate_outputs/extract_census_samples/public/public_1940.rda")
load("intermediate_outputs/extract_census_samples/public/public_1930.rda")
load("intermediate_outputs/extract_census_samples/public/public_1920.rda")
load("intermediate_outputs/extract_census_samples/public/public_1910.rda")
load("intermediate_outputs/extract_census_samples/public/public_1900.rda")

#pulling in parent names for each census year
for (i in 1:nrow(multiple_sample)){
  if (is.na(multiple_sample[i, "hhid_1900"])==FALSE){
    tmp=public_1900[which(public_1900$serial==multiple_sample[i, "hhid_1900"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1900[which(restricted_1900$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1900"]),"poploc"]==0){
      multiple_sample$father_name_census_1900[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1900"]),"poploc"]),]
      multiple_sample$father_name_census_1900[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1900"]),"momloc"]==0){
      multiple_sample$father_name_census_1900[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1900"]),"momloc"]),]
      multiple_sample$mother_name_census_1900[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1900[i]=NA
    multiple_sample$mother_name_census_1900[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1910"])==FALSE){
    tmp=public_1910[which(public_1910$serial==multiple_sample[i, "hhid_1910"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1910[which(restricted_1910$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"poploc"]==0){
      multiple_sample$father_name_census_1910[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"poploc"]),]
      multiple_sample$father_name_census_1910[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"momloc"]==0){
      multiple_sample$father_name_census_1910[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"momloc"]),]
      multiple_sample$mother_name_census_1910[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1910[i]=NA
    multiple_sample$mother_name_census_1910[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1920"])==FALSE){
    tmp=public_1920[which(public_1920$serial==multiple_sample[i, "hhid_1920"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1920[which(restricted_1920$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]),]
      multiple_sample$father_name_census_1920[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]),]
      multiple_sample$mother_name_census_1920[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1920[i]=NA
    multiple_sample$mother_name_census_1920[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1930"])==FALSE){
    tmp=public_1930[which(public_1930$serial==multiple_sample[i, "hhid_1930"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1930[which(restricted_1930$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]),]
      multiple_sample$father_name_census_1930[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]),]
      multiple_sample$mother_name_census_1930[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1930[i]=NA
    multiple_sample$mother_name_census_1930[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1940"])==FALSE){
    tmp=public_1940[which(public_1940$serial==multiple_sample[i, "hhid_1940"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1940[which(restricted_1940$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]),]
      multiple_sample$father_name_census_1940[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]),]
      multiple_sample$mother_name_census_1940[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1940[i]=NA
    multiple_sample$mother_name_census_1940[i]=NA
  }
}


#next stage string distances (soundex?) between parent names to find closest match! Then thats it, replicate for each cohort!

test=multiple_sample
library(stringdist)


load("input_data/step_1_misc_data/name_dict.rda")

all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)

test$mean_parent_difference=NA
#below loop handles NAs in parent names and anglicanizations of names
for (i in 1:nrow(test)){
  value=matrix(nrow=10, ncol=3) #number of rows is number of census years times 2
  if (is.na(test$father_name[i])){
    value[1,1]=0 #column 1 records distance column 2 records if NA
    value[1,2]=0 #0 means NA for this position- do not use in averages
    value[1,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[1,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1900[i])|test$father_name_census_1900[i]==0){
      value[1,1]=0
      value[1,2]=0
    } else{
      value[1,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1900[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[1,1]=min(stringdist(alt_names, test$father_name_census_1900[i], method="soundex"))
          } else{
            value[1,1]=a
          }
        } else{
          value[1,1]=a
        }
      } else{
        value[1,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[2,1]=0 #column 1 records distance column 2 records if NA
    value[2,2]=0 #0 means NA for this position- do not use in averages
    value[2,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[2,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1910[i])|test$father_name_census_1910[i]==0){
      value[2,1]=0
      value[2,2]=0
    } else{
      value[2,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1910[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[2,1]=min(stringdist(alt_names, test$father_name_census_1910[i], method="soundex"))
          } else{
            value[2,1]=a
          }
        } else{
          value[2,1]=a
        }
      } else{
        value[2,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[3,1]=0 #column 1 records distance column 2 records if NA
    value[3,2]=0 #0 means NA for this position- do not use in averages
    value[3,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[3,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1920[i])|test$father_name_census_1920[i]==0){
      value[3,1]=0
      value[3,2]=0
    } else{
      value[3,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[3,1]=min(stringdist(alt_names, test$father_name_census_1920[i], method="soundex"))
          } else{
            value[3,1]=a
          }
        } else{
          value[3,1]=a
        }
      } else{
        value[3,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[4,1]=0 #column 1 records distance column 2 records if NA
    value[4,2]=0 #0 means NA for this position- do not use in averages
    value[4,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[4,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1930[i])|test$father_name_census_1930[i]==0){
      value[4,1]=0
      value[4,2]=0
    } else{
      value[4,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[4,1]=min(stringdist(alt_names, test$father_name_census_1930[i], method="soundex"))
          } else{
            value[4,1]=a
          }
        } else{
          value[4,1]=a
        }
      } else{
        value[4,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[5,1]=0 #column 1 records distance column 2 records if NA
    value[5,2]=0 #0 means NA for this position- do not use in averages
    value[5,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[5,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1940[i])|test$father_name_census_1940[i]==0){
      value[5,1]=0
      value[5,2]=0
    } else{
      value[5,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[5,1]=min(stringdist(alt_names, test$father_name_census_1940[i], method="soundex"))
          } else{
            value[5,1]=a
          }
        } else{
          value[5,1]=a
        }
      } else{
        value[5,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[6,1]=0 #column 1 records distance column 2 records if NA
    value[6,2]=0 #0 means NA for this position- do not use in averages
    value[6,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[6,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1900[i])|test$mother_name_census_1900[i]==0){
      value[6,1]=0
      value[6,2]=0
    } else{
      value[6,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1900[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[6,1]=min(stringdist(alt_names, test$mother_name_census_1900[i], method="soundex"))
          } else{
            value[6,1]=a
          }
        } else{
          value[6,1]=a
        }
      } else{
        value[6,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[7,1]=0 #column 1 records distance column 2 records if NA
    value[7,2]=0 #0 means NA for this position- do not use in averages
    value[7,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[7,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1910[i])|test$mother_name_census_1910[i]==0){
      value[7,1]=0
      value[7,2]=0
    } else{
      value[7,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1910[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[7,1]=min(stringdist(alt_names, test$mother_name_census_1910[i], method="soundex"))
          } else{
            value[7,1]=a
          }
        } else{
          value[7,1]=a
        }
      } else{
        value[7,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[8,1]=0 #column 1 records distance column 2 records if NA
    value[8,2]=0 #0 means NA for this position- do not use in averages
    value[8,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[8,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1920[i])|test$mother_name_census_1920[i]==0){
      value[8,1]=0
      value[8,2]=0
    } else{
      value[8,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[8,1]=min(stringdist(alt_names, test$mother_name_census_1920[i], method="soundex"))
          } else{
            value[8,1]=a
          }
        } else{
          value[8,1]=a
        }
      } else{
        value[8,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[9,1]=0 #column 1 records distance column 2 records if NA
    value[9,2]=0 #0 means NA for this position- do not use in averages
    value[9,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[9,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1930[i])|test$mother_name_census_1930[i]==0){
      value[9,1]=0
      value[9,2]=0
    } else{
      value[9,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[9,1]=min(stringdist(alt_names, test$mother_name_census_1930[i], method="soundex"))
          } else{
            value[9,1]=a
          }
        } else{
          value[9,1]=a
        }
      } else{
        value[9,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[10,1]=0 #column 1 records distance column 2 records if NA
    value[10,2]=0 #0 means NA for this position- do not use in averages
    value[10,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[10,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1940[i])|test$mother_name_census_1940[i]==0){
      value[10,1]=0
      value[10,2]=0
    } else{
      value[10,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[10,1]=min(stringdist(alt_names, test$mother_name_census_1940[i], method="soundex"))
          } else{
            value[10,1]=a
          }
        } else{
          value[10,1]=a
        }
      } else{
        value[10,1]=a
      }
    }
  }
  if (sum(value[,3])==0){
    test$mean_parent_difference[i]=0.99 #no parent info available in numident is marginally better than parent info being available and not matching anywhere
  } else{
    if (sum(value[,2])==0){
      test$mean_parent_difference[i]=0.99 #no parent info available  in censuses is marginally better than parent info being available and not matching anywhere
    } else{
      test$mean_parent_difference[i]=mean(value[which(value[,2]==1),1]) #assigning average of non-na parent name distances
    }
  }
}


test2=matrix(nrow=length(unique(test$cohort_member_id)), ncol=5)
test2[,1]=unique(test$cohort_member_id)
for (i in 1:nrow(test2)){
  tmp=test[which(test$cohort_member_id==test2[i,1]),]
  tmp$mean_parent_difference=ifelse(tmp$match_method=="fuzzy", tmp$mean_parent_difference+0.1, tmp$mean_parent_difference) #small preference for direct matches
  test2[i,2]=min(tmp$mean_parent_difference)
  test2[i,3]=nrow(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
  if (test2[i,3]>1){
    a=nrow(tmp[which(is.na(tmp$histid_ct_1900)==FALSE & is.na(tmp$histid_ct_1910)==FALSE & is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
    if (a>0){
      tmp2=tmp[which(is.na(tmp$histid_ct_1900)==FALSE & is.na(tmp$histid_ct_1910)==FALSE & is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),]
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp2[which(tmp2$mean_parent_difference==min(tmp2$mean_parent_difference)),])))) #with ties, decision based on more complete data, extra preference towards complete histid set
    } else{
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])))) #with ties, decision based on more complete data
    }
  } else{
    test2[i,4]=which.min(tmp$mean_parent_difference)
  }
  test2[i,5]=paste0(tmp[as.numeric(test2[i,4]), "individual_string"], "_", rownames(tmp[as.numeric(test2[i,4]),]))
}

#last to combine with 1 to 1 individual matches and create matched data set
test$preferred=ifelse((paste0(test$individual_string,"_", rownames(test)) %in% test2[,5])==TRUE, 1, 0)
test=test[which(test$preferred==1),]
length(unique(test$cohort_member_id))

cohort_1900=rbind.data.frame(single_sample, test[,c(1:11)]) #up to individual string
length(unique(cohort_1900$cohort_member_id)) #PERFECT!!!!!!!!!!!!!
save(cohort_1900, file="intermediate_outputs/matched_records/cohort_1900.rda")

rm(list=ls())
gc()
###############################################
###################  1910 #####################
###############################################


load("intermediate_outputs/step_1_potential_census_matches/matches_1910_1940.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1910_1930.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1910_1920.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1910_1910.rda")


#now to combine lists into singular individual level record and drop no matches
#combining
cohort_1910=list()
for (i in 1:length(matches_1910_1940)){
  tmp=list()
  tmp[[1]]=i
  tmp[[2]]=matches_1910_1910[[i]]
  tmp[[3]]=matches_1910_1920[[i]]
  tmp[[4]]=matches_1910_1930[[i]]
  tmp[[5]]=matches_1910_1940[[i]]
  cohort_1910[[i]]=tmp
}

#dropping no matches
double_na = function(x, y, z, f){!(is.na(x) & is.na(y) & is.na(z) & is.na(f))}
keep_indices=vector(length = length(cohort_1910))
for (i in 1:length(cohort_1910)){
  keep_indices[i]=double_na(cohort_1910[[i]][2], cohort_1910[[i]][3], cohort_1910[[i]][4], cohort_1910[[i]][5])
}
keep_indices=which(keep_indices==TRUE)
cohort_1910=cohort_1910[keep_indices]

#counting number of records matched per numident individual
test_1=vector(length=length(cohort_1910))
test_2=vector(length=length(cohort_1910))
test_3=vector(length=length(cohort_1910))
test_4=vector(length=length(cohort_1910))

for (i in 1:length(cohort_1910)){
  if (is.na(cohort_1910[[i]][2])==TRUE){
    test_1[i]=0
  } else{
    test_1[i]=nrow(as.data.frame(cohort_1910[[i]][2]))
  }
  if (is.na(cohort_1910[[i]][3])==TRUE){
    test_2[i]=0
  } else{
    test_2[i]=nrow(as.data.frame(cohort_1910[[i]][3]))
  }
  if (is.na(cohort_1910[[i]][3])==TRUE){
    test_3[i]=0
  } else{
    test_3[i]=nrow(as.data.frame(cohort_1910[[i]][4]))
  }
  if (is.na(cohort_1910[[i]][4])==TRUE){
    test_4[i]=0
  } else{
    test_4[i]=nrow(as.data.frame(cohort_1910[[i]][5]))
  }
}
table(test_1)
table(test_2)
table(test_3)
table(test_4)
table(test_1+test_2+test_3+test_4)

#creating structure for bringing in census tree matching
list_1=list()
list_2=list()
list_3=list()
list_4=list()
for (i in 1:length(cohort_1910)){
  if (is.na(cohort_1910[[i]][2])){
    cohort_1910[[i]][2]=NA
  } else{
    tmp=as.data.frame(cohort_1910[[i]][2])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1910[[i]][1])
    tmp$census_year=1910
    list_1[[i]]=tmp
  }
  if (is.na(cohort_1910[[i]][3])){
    cohort_1910[[i]][3]=NA
  } else{
    tmp=as.data.frame(cohort_1910[[i]][3])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1910[[i]][1])
    tmp$census_year=1920
    list_2[[i]]=tmp
  }
  if (is.na(cohort_1910[[i]][4])){
    cohort_1910[[i]][4]=NA
  } else{
    tmp=as.data.frame(cohort_1910[[i]][4])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1910[[i]][1])
    tmp$census_year=1930
    list_3[[i]]=tmp
  }
  if (is.na(cohort_1910[[i]][5])){
    cohort_1910[[i]][5]=NA
  } else{
    tmp=as.data.frame(cohort_1910[[i]][5])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1910[[i]][1])
    tmp$census_year=1940
    list_4[[i]]=tmp
  }
}
list_1=do.call(rbind.data.frame, list_1)
list_2=do.call(rbind.data.frame, list_2)
list_3=do.call(rbind.data.frame, list_3)
list_4=do.call(rbind.data.frame, list_4)
cohort_1910=rbind.data.frame(list_1, list_2)
cohort_1910=rbind.data.frame(cohort_1910, list_3)
cohort_1910=rbind.data.frame(cohort_1910, list_4)
length(unique(cohort_1910$cohort_member_id))/nrow(cohort_1910)

#now to load census tree and see if same matchee individuals are apeparing twice
ct_1930_1940=read.csv(file="input_data/census_tree_crosswalks/1930_1940.csv")
ct_1920_1930=read.csv(file="input_data/census_tree_crosswalks/1920_1930.csv")
ct_1910_1920=read.csv(file="input_data/census_tree_crosswalks/1910_1920.csv")


cohort_1910_1910=cohort_1910[which(cohort_1910$census_year==1910),]
cohort_1910_1920=cohort_1910[which(cohort_1910$census_year==1920),]
cohort_1910_1930=cohort_1910[which(cohort_1910$census_year==1930),]
cohort_1910_1940=cohort_1910[which(cohort_1910$census_year==1940),]

cohort_1910_1910$histid_ct_1910=cohort_1910_1910$histid_match
cohort_1910_1910$histid_ct_1920=ct_1910_1920[match(cohort_1910_1910$histid_match, ct_1910_1920$histid1910), "histid1920"]
cohort_1910_1910$histid_ct_1930=ct_1920_1930[match(cohort_1910_1910$histid_ct_1920, ct_1920_1930$histid1920), "histid1930"]
cohort_1910_1910$histid_ct_1940=ct_1930_1940[match(cohort_1910_1910$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1910_1920$histid_ct_1910=ct_1910_1920[match(cohort_1910_1920$histid_match, ct_1910_1920$histid1920), "histid1910"]
cohort_1910_1920$histid_ct_1920=cohort_1910_1920$histid_match
cohort_1910_1920$histid_ct_1930=ct_1920_1930[match(cohort_1910_1920$histid_ct_1920, ct_1920_1930$histid1920), "histid1930"]
cohort_1910_1920$histid_ct_1940=ct_1930_1940[match(cohort_1910_1920$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1910_1930$histid_ct_1910=NA
cohort_1910_1930$histid_ct_1920=NA
cohort_1910_1930$histid_ct_1920=ct_1920_1930[match(cohort_1910_1930$histid_match, ct_1920_1930$histid1930), "histid1920"]
cohort_1910_1930$histid_ct_1910=ct_1910_1920[match(cohort_1910_1930$histid_ct_1920, ct_1910_1920$histid1920), "histid1910"]
cohort_1910_1930$histid_ct_1930=cohort_1910_1930$histid_match
cohort_1910_1930$histid_ct_1940=ct_1930_1940[match(cohort_1910_1930$histid_match, ct_1930_1940$histid1930), "histid1940"]

cohort_1910_1940$histid_ct_1910=NA
cohort_1910_1940$histid_ct_1920=NA
cohort_1910_1940$histid_ct_1930=NA
cohort_1910_1940$histid_ct_1940=cohort_1910_1940$histid_match
cohort_1910_1940$histid_ct_1930=ct_1930_1940[match(cohort_1910_1940$histid_match, ct_1930_1940$histid1940), "histid1930"]
cohort_1910_1940$histid_ct_1920=ct_1920_1930[match(cohort_1910_1940$histid_ct_1930, ct_1920_1930$histid1930), "histid1920"]
cohort_1910_1940$histid_ct_1910=ct_1910_1920[match(cohort_1910_1940$histid_ct_1920, ct_1910_1920$histid1920), "histid1910"]

cohort_1910=rbind.data.frame(cohort_1910_1910, cohort_1910_1920)
cohort_1910=rbind.data.frame(cohort_1910, cohort_1910_1930)
cohort_1910=rbind.data.frame(cohort_1910, cohort_1910_1940)


cohort_1910$individual_string=paste0(cohort_1910$histid_ct_1910, " ", cohort_1910$histid_ct_1920, " ", cohort_1910$histid_ct_1930, " ", cohort_1910$histid_ct_1940) #indvidual identity strings

cohort=matrix(nrow=length(unique(cohort_1910$cohort_member_id)), ncol=2)
cohort[,1]=unique(cohort_1910$cohort_member_id)
for (i in 1:nrow(cohort)){
  cohort[i,2]=nrow(cohort_1910[which(cohort_1910[,"cohort_member_id"]==cohort[i,1]),])
}
multiples=cohort[which(cohort[,2]>1),1] #generating IDs for sample of cohort that matched to multiple indviduals
singles=cohort[which(cohort[,2]==1),1]
single_sample=cohort_1910[which(cohort_1910$cohort_member_id %in% singles),]
#indvidual identity strings to NA for NA
multiple_sample=cohort_1910[which(cohort_1910$cohort_member_id %in% multiples),]

#loading numident and relevant census samples
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident=numident[which(numident$birth_year>=1900 & numident$birth_year<1910),]
numident$cohort_member_id=seq(1:nrow(numident))
numident=numident[which(numident$cohort_member_id %in% multiples),]
multiple_sample$mother_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "mother_name"])
multiple_sample$father_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "father_name"])
load("intermediate_outputs/extract_census_samples/households_1910.rda")
multiple_sample$hhid_1910=households_1910_histid[match(multiple_sample$histid_ct_1910, households_1910_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1920.rda")
multiple_sample$hhid_1920=households_1920_histid[match(multiple_sample$histid_ct_1920, households_1920_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1930.rda")
multiple_sample$hhid_1930=households_1930_histid[match(multiple_sample$histid_ct_1930, households_1930_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1940.rda")
multiple_sample$hhid_1940=households_1940_histid[match(multiple_sample$histid_ct_1940, households_1940_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1930.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1920.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1910.rda")
multiple_sample$father_name_census_1910=0
multiple_sample$father_name_census_1920=0
multiple_sample$father_name_census_1930=0
multiple_sample$father_name_census_1940=0
multiple_sample$mother_name_census_1910=0
multiple_sample$mother_name_census_1920=0
multiple_sample$mother_name_census_1930=0
multiple_sample$mother_name_census_1940=0
load("intermediate_outputs/extract_census_samples/public/public_1940.rda")
load("intermediate_outputs/extract_census_samples/public/public_1930.rda")
load("intermediate_outputs/extract_census_samples/public/public_1920.rda")
load("intermediate_outputs/extract_census_samples/public/public_1910.rda")

#pulling in parent names for each census year
for (i in 1:nrow(multiple_sample)){
  if (is.na(multiple_sample[i, "hhid_1910"])==FALSE){
    tmp=public_1910[which(public_1910$serial==multiple_sample[i, "hhid_1910"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1910[which(restricted_1910$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"poploc"]==0){
      multiple_sample$father_name_census_1910[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"poploc"]),]
      multiple_sample$father_name_census_1910[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"momloc"]==0){
      multiple_sample$father_name_census_1910[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1910"]),"momloc"]),]
      multiple_sample$mother_name_census_1910[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1910[i]=NA
    multiple_sample$mother_name_census_1910[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1920"])==FALSE){
    tmp=public_1920[which(public_1920$serial==multiple_sample[i, "hhid_1920"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1920[which(restricted_1920$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]),]
      multiple_sample$father_name_census_1920[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]),]
      multiple_sample$mother_name_census_1920[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1920[i]=NA
    multiple_sample$mother_name_census_1920[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1930"])==FALSE){
    tmp=public_1930[which(public_1930$serial==multiple_sample[i, "hhid_1930"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1930[which(restricted_1930$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]),]
      multiple_sample$father_name_census_1930[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]),]
      multiple_sample$mother_name_census_1930[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1930[i]=NA
    multiple_sample$mother_name_census_1930[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1940"])==FALSE){
    tmp=public_1940[which(public_1940$serial==multiple_sample[i, "hhid_1940"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1940[which(restricted_1940$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]),]
      multiple_sample$father_name_census_1940[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]),]
      multiple_sample$mother_name_census_1940[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1940[i]=NA
    multiple_sample$mother_name_census_1940[i]=NA
  }
}


#next stage string distances (soundex?) between parent names to find closest match! Then thats it, replicate for each cohort!

test=multiple_sample
library(stringdist)


load("input_data/step_1_misc_data/name_dict.rda")

all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)

test$mean_parent_difference=NA
#below loop handles NAs in parent names and anglicanizations of names
for (i in 1:nrow(test)){
  value=matrix(nrow=8, ncol=3) #number of rows is number of census years times 2
  if (is.na(test$father_name[i])){
    value[1,1]=0 #column 1 records distance column 2 records if NA
    value[1,2]=0 #0 means NA for this position- do not use in averages
    value[1,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[1,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1910[i])|test$father_name_census_1910[i]==0){
      value[1,1]=0
      value[1,2]=0
    } else{
      value[1,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1910[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[1,1]=min(stringdist(alt_names, test$father_name_census_1910[i], method="soundex"))
          } else{
            value[1,1]=a
          }
        } else{
          value[1,1]=a
        }
      } else{
        value[1,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[2,1]=0 #column 1 records distance column 2 records if NA
    value[2,2]=0 #0 means NA for this position- do not use in averages
    value[2,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[2,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1920[i])|test$father_name_census_1920[i]==0){
      value[2,1]=0
      value[2,2]=0
    } else{
      value[2,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[2,1]=min(stringdist(alt_names, test$father_name_census_1920[i], method="soundex"))
          } else{
            value[2,1]=a
          }
        } else{
          value[2,1]=a
        }
      } else{
        value[2,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[3,1]=0 #column 1 records distance column 2 records if NA
    value[3,2]=0 #0 means NA for this position- do not use in averages
    value[3,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[3,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1930[i])|test$father_name_census_1930[i]==0){
      value[3,1]=0
      value[3,2]=0
    } else{
      value[3,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[3,1]=min(stringdist(alt_names, test$father_name_census_1930[i], method="soundex"))
          } else{
            value[3,1]=a
          }
        } else{
          value[3,1]=a
        }
      } else{
        value[3,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[4,1]=0 #column 1 records distance column 2 records if NA
    value[4,2]=0 #0 means NA for this position- do not use in averages
    value[4,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[4,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1940[i])|test$father_name_census_1940[i]==0){
      value[4,1]=0
      value[4,2]=0
    } else{
      value[4,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[4,1]=min(stringdist(alt_names, test$father_name_census_1940[i], method="soundex"))
          } else{
            value[4,1]=a
          }
        } else{
          value[4,1]=a
        }
      } else{
        value[4,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[5,1]=0 #column 1 records distance column 2 records if NA
    value[5,2]=0 #0 means NA for this position- do not use in averages
    value[5,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[5,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1910[i])|test$mother_name_census_1910[i]==0){
      value[5,1]=0
      value[5,2]=0
    } else{
      value[5,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1910[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[5,1]=min(stringdist(alt_names, test$mother_name_census_1910[i], method="soundex"))
          } else{
            value[5,1]=a
          }
        } else{
          value[5,1]=a
        }
      } else{
        value[5,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[6,1]=0 #column 1 records distance column 2 records if NA
    value[6,2]=0 #0 means NA for this position- do not use in averages
    value[6,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[6,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1920[i])|test$mother_name_census_1920[i]==0){
      value[6,1]=0
      value[6,2]=0
    } else{
      value[6,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[6,1]=min(stringdist(alt_names, test$mother_name_census_1920[i], method="soundex"))
          } else{
            value[6,1]=a
          }
        } else{
          value[6,1]=a
        }
      } else{
        value[6,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[7,1]=0 #column 1 records distance column 2 records if NA
    value[7,2]=0 #0 means NA for this position- do not use in averages
    value[7,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[7,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1930[i])|test$mother_name_census_1930[i]==0){
      value[7,1]=0
      value[7,2]=0
    } else{
      value[7,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[7,1]=min(stringdist(alt_names, test$mother_name_census_1930[i], method="soundex"))
          } else{
            value[7,1]=a
          }
        } else{
          value[7,1]=a
        }
      } else{
        value[7,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[8,1]=0 #column 1 records distance column 2 records if NA
    value[8,2]=0 #0 means NA for this position- do not use in averages
    value[8,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[8,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1940[i])|test$mother_name_census_1940[i]==0){
      value[8,1]=0
      value[8,2]=0
    } else{
      value[8,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[8,1]=min(stringdist(alt_names, test$mother_name_census_1940[i], method="soundex"))
          } else{
            value[8,1]=a
          }
        } else{
          value[8,1]=a
        }
      } else{
        value[8,1]=a
      }
    }
  }
  if (sum(value[,3])==0){
    test$mean_parent_difference[i]=0.99 #no parent info available in numident is marginally better than parent info being available and not matching anywhere
  } else{
    if (sum(value[,2])==0){
      test$mean_parent_difference[i]=0.99 #no parent info available  in censuses is marginally better than parent info being available and not matching anywhere
    } else{
      test$mean_parent_difference[i]=mean(value[which(value[,2]==1),1]) #assigning average of non-na parent name distances
    }
  }
}


test2=matrix(nrow=length(unique(test$cohort_member_id)), ncol=5)
test2[,1]=unique(test$cohort_member_id)
for (i in 1:nrow(test2)){
  tmp=test[which(test$cohort_member_id==test2[i,1]),]
  tmp$mean_parent_difference=ifelse(tmp$match_method=="fuzzy", tmp$mean_parent_difference+0.1, tmp$mean_parent_difference) #small preference for direct matches
  test2[i,2]=min(tmp$mean_parent_difference)
  test2[i,3]=nrow(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
  if (test2[i,3]>1){
    a=nrow(tmp[which(is.na(tmp$histid_ct_1910)==FALSE & is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
    if (a>0){
      tmp2=tmp[which(is.na(tmp$histid_ct_1910)==FALSE & is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),]
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp2[which(tmp2$mean_parent_difference==min(tmp2$mean_parent_difference)),])))) #with ties, decision based on more complete data, extra preference towards complete histid set
    } else{
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])))) #with ties, decision based on more complete data
    }
  } else{
    test2[i,4]=which.min(tmp$mean_parent_difference)
  }
  test2[i,5]=paste0(tmp[as.numeric(test2[i,4]), "individual_string"], "_", rownames(tmp[as.numeric(test2[i,4]),]))
}

#last to combine with 1 to 1 individual matches and create matched data set
test$preferred=ifelse((paste0(test$individual_string,"_", rownames(test)) %in% test2[,5])==TRUE, 1, 0)
test=test[which(test$preferred==1),]
length(unique(test$cohort_member_id))

cohort_1910=rbind.data.frame(single_sample, test[,c(1:10)]) #up to individual string
length(unique(cohort_1910$cohort_member_id)) #PERFECT!!!!!!!!!!!!!
save(cohort_1910, file="intermediate_outputs/matched_records/cohort_1910.rda")
rm(list=ls())
gc()

###############################################
###################  1920 #####################
###############################################

load("intermediate_outputs/step_1_potential_census_matches/matches_1920_1940.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1920_1930.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1920_1920.rda")

#now to combine lists into singular individual level record and drop no matches
#combining
cohort_1920=list()
for (i in 1:length(matches_1920_1940)){
  tmp=list()
  tmp[[1]]=i
  tmp[[2]]=matches_1920_1920[[i]]
  tmp[[3]]=matches_1920_1930[[i]]
  tmp[[4]]=matches_1920_1940[[i]]
  cohort_1920[[i]]=tmp
}

#dropping no matches
double_na = function(x, y, z){!(is.na(x) & is.na(y) & is.na(z))}
keep_indices=vector(length = length(cohort_1920))
for (i in 1:length(cohort_1920)){
  keep_indices[i]=double_na(cohort_1920[[i]][2], cohort_1920[[i]][3], cohort_1920[[i]][4])
}
keep_indices=which(keep_indices==TRUE)
cohort_1920=cohort_1920[keep_indices]

#counting number of records matched per numident individual
test_1=vector(length=length(cohort_1920))
test_2=vector(length=length(cohort_1920))
test_3=vector(length=length(cohort_1920))

for (i in 1:length(cohort_1920)){
  if (is.na(cohort_1920[[i]][2])==TRUE){
    test_1[i]=0
  } else{
    test_1[i]=nrow(as.data.frame(cohort_1920[[i]][2]))
  }
  if (is.na(cohort_1920[[i]][3])==TRUE){
    test_2[i]=0
  } else{
    test_2[i]=nrow(as.data.frame(cohort_1920[[i]][3]))
  }
  if (is.na(cohort_1920[[i]][3])==TRUE){
    test_3[i]=0
  } else{
    test_3[i]=nrow(as.data.frame(cohort_1920[[i]][4]))
  }
}
table(test_1)
table(test_2)
table(test_3)
table(test_1+test_2+test_3)

#creating structure for bringing in census tree matching
list_1=list()
list_2=list()
list_3=list()
for (i in 1:length(cohort_1920)){
  if (is.na(cohort_1920[[i]][2])){
    cohort_1920[[i]][2]=NA
  } else{
    tmp=as.data.frame(cohort_1920[[i]][2])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1920[[i]][1])
    tmp$census_year=1920
    list_1[[i]]=tmp
  }
  if (is.na(cohort_1920[[i]][3])){
    cohort_1920[[i]][3]=NA
  } else{
    tmp=as.data.frame(cohort_1920[[i]][3])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1920[[i]][1])
    tmp$census_year=1930
    list_2[[i]]=tmp
  }
  if (is.na(cohort_1920[[i]][4])){
    cohort_1920[[i]][4]=NA
  } else{
    tmp=as.data.frame(cohort_1920[[i]][4])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1920[[i]][1])
    tmp$census_year=1940
    list_3[[i]]=tmp
  }
}
list_1=do.call(rbind.data.frame, list_1)
list_2=do.call(rbind.data.frame, list_2)
list_3=do.call(rbind.data.frame, list_3)
cohort_1920=rbind.data.frame(list_1, list_2)
cohort_1920=rbind.data.frame(cohort_1920, list_3)
length(unique(cohort_1920$cohort_member_id))/nrow(cohort_1920)

#now to load census tree and see if same matchee individuals are apeparing twice
ct_1930_1940=read.csv(file="input_data/census_tree_crosswalks/1930_1940.csv")
ct_1920_1930=read.csv(file="input_data/census_tree_crosswalks/1920_1930.csv")

cohort_1920_1920=cohort_1920[which(cohort_1920$census_year==1920),]
cohort_1920_1930=cohort_1920[which(cohort_1920$census_year==1930),]
cohort_1920_1940=cohort_1920[which(cohort_1920$census_year==1940),]
cohort_1920_1920$histid_ct_1920=cohort_1920_1920$histid_match
cohort_1920_1920$histid_ct_1930=ct_1920_1930[match(cohort_1920_1920$histid_match, ct_1920_1930$histid1920), "histid1930"]
cohort_1920_1920$histid_ct_1940=ct_1930_1940[match(cohort_1920_1920$histid_ct_1930, ct_1930_1940$histid1930), "histid1940"]

cohort_1920_1930$histid_ct_1920=ct_1920_1930[match(cohort_1920_1930$histid_match, ct_1920_1930$histid1930), "histid1920"]
cohort_1920_1930$histid_ct_1930=cohort_1920_1930$histid_match
cohort_1920_1930$histid_ct_1940=ct_1930_1940[match(cohort_1920_1930$histid_match, ct_1930_1940$histid1930), "histid1940"]

cohort_1920_1940$histid_ct_1920=NA
cohort_1920_1940$histid_ct_1930=NA
cohort_1920_1940$histid_ct_1930=ct_1930_1940[match(cohort_1920_1940$histid_match, ct_1930_1940$histid1940), "histid1930"]
cohort_1920_1940$histid_ct_1920=ct_1920_1930[match(cohort_1920_1940$histid_ct_1930, ct_1920_1930$histid1930), "histid1920"]
cohort_1920_1940$histid_ct_1940=cohort_1920_1940$histid_match

cohort_1920=rbind.data.frame(cohort_1920_1920, cohort_1920_1930)
cohort_1920=rbind.data.frame(cohort_1920, cohort_1920_1940)

cohort_1920$individual_string=paste0(cohort_1920$histid_ct_1920, " ", cohort_1920$histid_ct_1930, " ", cohort_1920$histid_ct_1940) #indvidual identity strings

cohort=matrix(nrow=length(unique(cohort_1920$cohort_member_id)), ncol=2)
cohort[,1]=unique(cohort_1920$cohort_member_id)
for (i in 1:nrow(cohort)){
  cohort[i,2]=nrow(cohort_1920[which(cohort_1920[,"cohort_member_id"]==cohort[i,1]),])
}
multiples=cohort[which(cohort[,2]>1),1] #generating IDs for sample of cohort that matched to multiple indviduals
singles=cohort[which(cohort[,2]==1),1]
single_sample=cohort_1920[which(cohort_1920$cohort_member_id %in% singles),]
#indvidual identity strings to NA for NA
multiple_sample=cohort_1920[which(cohort_1920$cohort_member_id %in% multiples),]

#loading numident and relevant census samples
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident=numident[which(numident$birth_year>=1910 & numident$birth_year<1920),]
numident$cohort_member_id=seq(1:nrow(numident))
numident=numident[which(numident$cohort_member_id %in% multiples),]
multiple_sample$mother_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "mother_name"])
multiple_sample$father_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "father_name"])
load("intermediate_outputs/extract_census_samples/households_1920.rda")
multiple_sample$hhid_1920=households_1920_histid[match(multiple_sample$histid_ct_1920, households_1920_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1930.rda")
multiple_sample$hhid_1930=households_1930_histid[match(multiple_sample$histid_ct_1930, households_1930_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1940.rda")
multiple_sample$hhid_1940=households_1940_histid[match(multiple_sample$histid_ct_1940, households_1940_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1930.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1920.rda")
multiple_sample$father_name_census_1920=0
multiple_sample$father_name_census_1930=0
multiple_sample$father_name_census_1940=0
multiple_sample$mother_name_census_1920=0
multiple_sample$mother_name_census_1930=0
multiple_sample$mother_name_census_1940=0
load("intermediate_outputs/extract_census_samples/public/public_1940.rda")
load("intermediate_outputs/extract_census_samples/public/public_1930.rda")
load("intermediate_outputs/extract_census_samples/public/public_1920.rda")

#pulling in parent names for each census year
for (i in 1:nrow(multiple_sample)){
  if (is.na(multiple_sample[i, "hhid_1920"])==FALSE){
    tmp=public_1920[which(public_1920$serial==multiple_sample[i, "hhid_1920"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1920[which(restricted_1920$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"poploc"]),]
      multiple_sample$father_name_census_1920[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]==0){
      multiple_sample$father_name_census_1920[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1920"]),"momloc"]),]
      multiple_sample$mother_name_census_1920[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1920[i]=NA
    multiple_sample$mother_name_census_1920[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1930"])==FALSE){
    tmp=public_1930[which(public_1930$serial==multiple_sample[i, "hhid_1930"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1930[which(restricted_1930$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]),]
      multiple_sample$father_name_census_1930[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]),]
      multiple_sample$mother_name_census_1930[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1930[i]=NA
    multiple_sample$mother_name_census_1930[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1940"])==FALSE){
    tmp=public_1940[which(public_1940$serial==multiple_sample[i, "hhid_1940"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1940[which(restricted_1940$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]),]
      multiple_sample$father_name_census_1940[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]),]
      multiple_sample$mother_name_census_1940[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1940[i]=NA
    multiple_sample$mother_name_census_1940[i]=NA
  }
}


#next stage string distances (soundex?) between parent names to find closest match! Then thats it, replicate for each cohort!

test=multiple_sample
library(stringdist)


load("input_data/step_1_misc_data/name_dict.rda")

all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)

test$mean_parent_difference=NA
#below loop handles NAs in parent names and anglicanizations of names
for (i in 1:nrow(test)){
  value=matrix(nrow=6, ncol=3) #number of rows is number of census years times 2
  if (is.na(test$father_name[i])){
    value[1,1]=0 #column 1 records distance column 2 records if NA
    value[1,2]=0 #0 means NA for this position- do not use in averages
    value[1,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[1,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1920[i])|test$father_name_census_1920[i]==0){
      value[1,1]=0
      value[1,2]=0
    } else{
      value[1,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[1,1]=min(stringdist(alt_names, test$father_name_census_1920[i], method="soundex"))
          } else{
            value[1,1]=a
          }
        } else{
          value[1,1]=a
        }
      } else{
        value[1,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[2,1]=0 #column 1 records distance column 2 records if NA
    value[2,2]=0 #0 means NA for this position- do not use in averages
    value[2,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[2,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1930[i])|test$father_name_census_1930[i]==0){
      value[2,1]=0
      value[2,2]=0
    } else{
      value[2,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[2,1]=min(stringdist(alt_names, test$father_name_census_1930[i], method="soundex"))
          } else{
            value[2,1]=a
          }
        } else{
          value[2,1]=a
        }
      } else{
        value[2,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[3,1]=0 #column 1 records distance column 2 records if NA
    value[3,2]=0 #0 means NA for this position- do not use in averages
    value[3,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[3,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1940[i])|test$father_name_census_1940[i]==0){
      value[3,1]=0
      value[3,2]=0
    } else{
      value[3,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[3,1]=min(stringdist(alt_names, test$father_name_census_1940[i], method="soundex"))
          } else{
            value[3,1]=a
          }
        } else{
          value[3,1]=a
        }
      } else{
        value[3,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[4,1]=0 #column 1 records distance column 2 records if NA
    value[4,2]=0 #0 means NA for this position- do not use in averages
    value[4,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[4,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1920[i])|test$mother_name_census_1920[i]==0){
      value[4,1]=0
      value[4,2]=0
    } else{
      value[4,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1920[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[4,1]=min(stringdist(alt_names, test$mother_name_census_1920[i], method="soundex"))
          } else{
            value[4,1]=a
          }
        } else{
          value[4,1]=a
        }
      } else{
        value[4,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[5,1]=0 #column 1 records distance column 2 records if NA
    value[5,2]=0 #0 means NA for this position- do not use in averages
    value[5,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[5,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1930[i])|test$mother_name_census_1930[i]==0){
      value[5,1]=0
      value[5,2]=0
    } else{
      value[5,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[5,1]=min(stringdist(alt_names, test$mother_name_census_1930[i], method="soundex"))
          } else{
            value[5,1]=a
          }
        } else{
          value[5,1]=a
        }
      } else{
        value[5,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[6,1]=0 #column 1 records distance column 2 records if NA
    value[6,2]=0 #0 means NA for this position- do not use in averages
    value[6,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[6,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1940[i])|test$mother_name_census_1940[i]==0){
      value[6,1]=0
      value[6,2]=0
    } else{
      value[6,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[6,1]=min(stringdist(alt_names, test$mother_name_census_1940[i], method="soundex"))
          } else{
            value[6,1]=a
          }
        } else{
          value[6,1]=a
        }
      } else{
        value[6,1]=a
      }
    }
  }
  if (sum(value[,3])==0){
    test$mean_parent_difference[i]=0.99 #no parent info available in numident is marginally better than parent info being available and not matching anywhere
  } else{
    if (sum(value[,2])==0){
      test$mean_parent_difference[i]=0.99 #no parent info available  in censuses is marginally better than parent info being available and not matching anywhere
    } else{
      test$mean_parent_difference[i]=mean(value[which(value[,2]==1),1]) #assigning average of non-na parent name distances
    }
  }
}


test2=matrix(nrow=length(unique(test$cohort_member_id)), ncol=5)
test2[,1]=unique(test$cohort_member_id)
for (i in 1:nrow(test2)){
  tmp=test[which(test$cohort_member_id==test2[i,1]),]
  tmp$mean_parent_difference=ifelse(tmp$match_method=="fuzzy", tmp$mean_parent_difference+0.1, tmp$mean_parent_difference) #small preference for direct matches
  test2[i,2]=min(tmp$mean_parent_difference)
  test2[i,3]=nrow(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
  if (test2[i,3]>1){
    a=nrow(tmp[which(is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
    if (a>0){
      tmp2=tmp[which(is.na(tmp$histid_ct_1920)==FALSE & is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),]
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp2[which(tmp2$mean_parent_difference==min(tmp2$mean_parent_difference)),])))) #with ties, decision based on more complete data, extra preference towards complete histid set
    } else{
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])))) #with ties, decision based on more complete data
    }
  } else{
    test2[i,4]=which.min(tmp$mean_parent_difference)
  }
  test2[i,5]=paste0(tmp[as.numeric(test2[i,4]), "individual_string"], "_", rownames(tmp[as.numeric(test2[i,4]),]))
}

#last to combine with 1 to 1 individual matches and create matched data set
test$preferred=ifelse((paste0(test$individual_string,"_", rownames(test)) %in% test2[,5])==TRUE, 1, 0)
test=test[which(test$preferred==1),]
length(unique(test$cohort_member_id))

cohort_1920=rbind.data.frame(single_sample, test[,c(1:9)]) #up to individual string
length(unique(cohort_1920$cohort_member_id)) #PERFECT!!!!!!!!!!!!!
save(cohort_1920, file="intermediate_outputs/matched_records/cohort_1920.rda")


rm(list=ls())
gc()
###############################################
###################  1930 #####################
###############################################

load("intermediate_outputs/step_1_potential_census_matches/matches_1930_1940.rda")
load("intermediate_outputs/step_1_potential_census_matches/matches_1930_1930.rda")
#now to combine lists into singular individual level record and drop no matches
#combining
cohort_1930=list()
for (i in 1:length(matches_1930_1940)){
  tmp=list()
  tmp[[1]]=i
  tmp[[2]]=matches_1930_1930[[i]]
  tmp[[3]]=matches_1930_1940[[i]]
  cohort_1930[[i]]=tmp
}

#dropping no matches
double_na = function(x, y){!(is.na(x) & is.na(y))}
keep_indices=vector(length = length(cohort_1930))
for (i in 1:length(cohort_1930)){
  keep_indices[i]=double_na(cohort_1930[[i]][2], cohort_1930[[i]][3])
}
keep_indices=which(keep_indices==TRUE)
cohort_1930=cohort_1930[keep_indices]

#counting number of records matched per numident individual
test_1=vector(length=length(cohort_1930))
test_2=vector(length=length(cohort_1930))
for (i in 1:length(cohort_1930)){
  if (is.na(cohort_1930[[i]][2])==TRUE){
    test_1[i]=0
  } else{
    test_1[i]=nrow(as.data.frame(cohort_1930[[i]][2]))
  }
  if (is.na(cohort_1930[[i]][3])==TRUE){
    test_2[i]=0
  } else{
    test_2[i]=nrow(as.data.frame(cohort_1930[[i]][3]))
  }
}
table(test_1)
table(test_2)
table(test_1+test_2)

#creating structure for bringing in census tree matching
list_1=list()
list_2=list()
for (i in 1:length(cohort_1930)){
  if (is.na(cohort_1930[[i]][2])){
    cohort_1930[[i]][2]=NA
  } else{
    tmp=as.data.frame(cohort_1930[[i]][2])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1930[[i]][1])
    tmp$census_year=1930
    list_1[[i]]=tmp
  }
  if (is.na(cohort_1930[[i]][3])){
    cohort_1930[[i]][3]=NA
  } else{
    tmp=as.data.frame(cohort_1930[[i]][3])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1930[[i]][1])
    tmp$census_year=1940
    list_2[[i]]=tmp
  }
}
list_1=do.call(rbind.data.frame, list_1)
list_2=do.call(rbind.data.frame, list_2)
cohort_1930=rbind.data.frame(list_1, list_2)
length(unique(cohort_1930$cohort_member_id))/nrow(cohort_1930)

#now to load census tree and see if same matchee individuals are apeparing twice
ct_1930_1940=read.csv(file="input_data/census_tree_crosswalks/1930_1940.csv")

cohort_1930_1930=cohort_1930[which(cohort_1930$census_year==1930),]
cohort_1930_1940=cohort_1930[which(cohort_1930$census_year==1940),]
cohort_1930_1930$histid_ct_1930=cohort_1930_1930$histid_match
cohort_1930_1930$histid_ct_1940=ct_1930_1940[match(cohort_1930_1930$histid_match, ct_1930_1940$histid1930), "histid1940"]
cohort_1930_1940$histid_ct_1930=ct_1930_1940[match(cohort_1930_1940$histid_match, ct_1930_1940$histid1940), "histid1930"]
cohort_1930_1940$histid_ct_1940=cohort_1930_1940$histid_match
cohort_1930=rbind.data.frame(cohort_1930_1930, cohort_1930_1940)
cohort_1930$individual_string=paste0(cohort_1930$histid_ct_1930, " ", cohort_1930$histid_ct_1940) #indvidual identity strings

cohort=matrix(nrow=length(unique(cohort_1930$cohort_member_id)), ncol=2)
cohort[,1]=unique(cohort_1930$cohort_member_id)
for (i in 1:nrow(cohort)){
  cohort[i,2]=nrow(cohort_1930[which(cohort_1930[,"cohort_member_id"]==cohort[i,1]),])
}
multiples=cohort[which(cohort[,2]>1),1] #generating IDs for sample of cohort that matched to multiple indviduals
singles=cohort[which(cohort[,2]==1),1]
single_sample=cohort_1930[which(cohort_1930$cohort_member_id %in% singles),]
#indvidual identity strings to NA for NA
multiple_sample=cohort_1930[which(cohort_1930$cohort_member_id %in% multiples),]

#loading numident and relevant census samples
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident=numident[which(numident$birth_year>=1920 & numident$birth_year<1930),]
numident$cohort_member_id=seq(1:nrow(numident))
numident=numident[which(numident$cohort_member_id %in% multiples),]
multiple_sample$mother_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "mother_name"])
multiple_sample$father_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "father_name"])
load("intermediate_outputs/extract_census_samples/households_1930.rda")
multiple_sample$hhid_1930=households_1930_histid[match(multiple_sample$histid_ct_1930, households_1930_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/households_1940.rda")
multiple_sample$hhid_1940=households_1940_histid[match(multiple_sample$histid_ct_1940, households_1940_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
load("intermediate_outputs/extract_census_samples/restricted/restricted_1930.rda")
multiple_sample$father_name_census_1930=0
multiple_sample$father_name_census_1940=0
multiple_sample$mother_name_census_1930=0
multiple_sample$mother_name_census_1940=0
load("intermediate_outputs/extract_census_samples/public/public_1940.rda")
load("intermediate_outputs/extract_census_samples/public/public_1930.rda")

for (i in 1:nrow(multiple_sample)){
  if (is.na(multiple_sample[i, "hhid_1930"])==FALSE){
    tmp=public_1930[which(public_1930$serial==multiple_sample[i, "hhid_1930"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1930[which(restricted_1930$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"poploc"]),]
      multiple_sample$father_name_census_1930[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]==0){
      multiple_sample$father_name_census_1930[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1930"]),"momloc"]),]
      multiple_sample$mother_name_census_1930[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1930[i]=NA
    multiple_sample$mother_name_census_1930[i]=NA
  }
  if (is.na(multiple_sample[i, "hhid_1940"])==FALSE){
    tmp=public_1940[which(public_1940$serial==multiple_sample[i, "hhid_1940"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1940[which(restricted_1940$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]),]
      multiple_sample$father_name_census_1940[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]),]
      multiple_sample$mother_name_census_1940[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1940[i]=NA
    multiple_sample$mother_name_census_1940[i]=NA
  }
}


#next stage string distances (soundex?) between parent names to find closest match! Then thats it, replicate for each cohort!

test=multiple_sample
library(stringdist)


load("input_data/step_1_misc_data/name_dict.rda")

all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)

test$mean_parent_difference=NA
#below loop handles NAs in parent names and anglicanizations of names
for (i in 1:nrow(test)){
  value=matrix(nrow=4, ncol=3)
  if (is.na(test$father_name[i])){
    value[1,1]=0 #column 1 records distance column 2 records if NA
    value[1,2]=0 #0 means NA for this position- do not use in averages
    value[1,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[1,3]=1 #1 RECORDS present parent info from numident for column 3
    if (is.na(test$father_name_census_1930[i])|test$father_name_census_1930[i]==0){
      value[1,1]=0
      value[1,2]=0
    } else{
      value[1,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[1,1]=min(stringdist(alt_names, test$father_name_census_1930[i], method="soundex"))
          } else{
            value[1,1]=a
          }
        } else{
          value[1,1]=a
        }
      } else{
        value[1,1]=a
      }
    }
  }
  if (is.na(test$father_name[i])){
    value[2,1]=0 #column 1 records distance column 2 records if NA
    value[2,2]=0 #0 means NA for this position- do not use in averages
    value[2,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[2,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1940[i])|test$father_name_census_1940[i]==0){
      value[2,1]=0
      value[2,2]=0
    } else{
      value[2,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[2,1]=min(stringdist(alt_names, test$father_name_census_1940[i], method="soundex"))
          } else{
            value[2,1]=a
          }
        } else{
          value[2,1]=a
        }
      } else{
        value[2,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[3,1]=0 #column 1 records distance column 2 records if NA
    value[3,2]=0 #0 means NA for this position- do not use in averages
    value[3,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[3,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1930[i])|test$mother_name_census_1930[i]==0){
      value[3,1]=0
      value[3,2]=0
    } else{
      value[3,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1930[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[3,1]=min(stringdist(alt_names, test$mother_name_census_1930[i], method="soundex"))
          } else{
            value[3,1]=a
          }
        } else{
          value[3,1]=a
        }
      } else{
        value[3,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[4,1]=0 #column 1 records distance column 2 records if NA
    value[4,2]=0 #0 means NA for this position- do not use in averages
    value[4,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[4,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1940[i])|test$mother_name_census_1940[i]==0){
      value[4,1]=0
      value[4,2]=0
    } else{
      value[4,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[4,1]=min(stringdist(alt_names, test$mother_name_census_1940[i], method="soundex"))
          } else{
            value[4,1]=a
          }
        } else{
          value[4,1]=a
        }
      } else{
        value[4,1]=a
      }
    }
  }
  if (sum(value[,3])==0){
    test$mean_parent_difference[i]=0.99 #no parent info available in numident is marginally better than parent info being available and not matching anywhere
  } else{
    if (sum(value[,2])==0){
      test$mean_parent_difference[i]=0.99 #no parent info available  in censuses is marginally better than parent info being available and not matching anywhere
    } else{
      test$mean_parent_difference[i]=mean(value[which(value[,2]==1),1]) #assigning average of non-na parent name distances
    }
  }
}


test2=matrix(nrow=length(unique(test$cohort_member_id)), ncol=5)
test2[,1]=unique(test$cohort_member_id)
for (i in 1:nrow(test2)){
  tmp=test[which(test$cohort_member_id==test2[i,1]),]
  tmp$mean_parent_difference=ifelse(tmp$match_method=="fuzzy", tmp$mean_parent_difference+0.1, tmp$mean_parent_difference) #small preference for direct matches
  test2[i,2]=min(tmp$mean_parent_difference)
  test2[i,3]=nrow(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
  if (test2[i,3]>1){
    a=nrow(tmp[which(is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
    if (a>0){
      tmp2=tmp[which(is.na(tmp$histid_ct_1930)==FALSE & is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),]
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp2[which(tmp2$mean_parent_difference==min(tmp2$mean_parent_difference)),])))) #with ties, decision based on more complete data, extra preference towards complete histid set
    } else{
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])))) #with ties, decision based on more complete data
    }
  } else{
    test2[i,4]=which.min(tmp$mean_parent_difference)
  }
  test2[i,5]=paste0(tmp[as.numeric(test2[i,4]), "individual_string"], "_", rownames(tmp[as.numeric(test2[i,4]),]))
}

#last to combine with 1 to 1 individual matches and create matched data set
test$preferred=ifelse((paste0(test$individual_string,"_", rownames(test)) %in% test2[,5])==TRUE, 1, 0)
test=test[which(test$preferred==1),]
length(unique(test$cohort_member_id))

cohort_1930=rbind.data.frame(single_sample, test[,c(1:8)])
length(unique(cohort_1930$cohort_member_id)) #PERFECT!!!!!!!!!!!!!
save(cohort_1930, file="intermediate_outputs/matched_records/cohort_1930.rda")


rm(list=ls())
gc()
###############################################
###################  1940 #####################
###############################################

load("intermediate_outputs/step_1_potential_census_matches/matches_1940_1940.rda")
#now to combine lists into singular individual level record and drop no matches
#combining
cohort_1940=list()
for (i in 1:length(matches_1940_1940)){
  tmp=list()
  tmp[[1]]=i
  tmp[[2]]=matches_1940_1940[[i]]
  cohort_1940[[i]]=tmp
}

#dropping no matches
double_na = function(x){!(is.na(x))}
keep_indices=vector(length = length(cohort_1940))
for (i in 1:length(cohort_1940)){
  keep_indices[i]=double_na(cohort_1940[[i]][2])
}
keep_indices=which(keep_indices==TRUE)
cohort_1940=cohort_1940[keep_indices]

#counting number of records matched per numident individual
test_1=vector(length=length(cohort_1940))
for (i in 1:length(cohort_1940)){
  if (is.na(cohort_1940[[i]][2])==TRUE){
    test_1[i]=0
  } else{
    test_1[i]=nrow(as.data.frame(cohort_1940[[i]][2]))
  }
}
table(test_1)

#creating structure for bringing in census tree matching
list_1=list()
for (i in 1:length(cohort_1940)){
  if (is.na(cohort_1940[[i]][2])){
    cohort_1940[[i]][2]=NA
  } else{
    tmp=as.data.frame(cohort_1940[[i]][2])[,c(1,2,4)]
    colnames(tmp)=c("match_string", "match_method", "histid_match")
    tmp$cohort_member_id=as.numeric(cohort_1940[[i]][1])
    tmp$census_year=1940
    list_1[[i]]=tmp
  }
}
list_1=do.call(rbind.data.frame, list_1)
cohort_1940=list_1
length(unique(cohort_1940$cohort_member_id))/nrow(cohort_1940)


cohort_1940$histid_ct_1940=cohort_1940$histid_match
cohort_1940$individual_string=paste0(cohort_1940$histid_ct_1940) #indvidual identity strings

cohort=matrix(nrow=length(unique(cohort_1940$cohort_member_id)), ncol=2)
cohort[,1]=unique(cohort_1940$cohort_member_id)
for (i in 1:nrow(cohort)){
  cohort[i,2]=nrow(cohort_1940[which(cohort_1940[,"cohort_member_id"]==cohort[i,1]),])
}
multiples=cohort[which(cohort[,2]>1),1] #generating IDs for sample of cohort that matched to multiple indviduals
singles=cohort[which(cohort[,2]==1),1]
single_sample=cohort_1940[which(cohort_1940$cohort_member_id %in% singles),]
#indvidual identity strings to NA for NA
multiple_sample=cohort_1940[which(cohort_1940$cohort_member_id %in% multiples),]

#loading numident and relevant census samples
load("intermediate_outputs/sicilian_numident.rda")
numident$birth_year=as.numeric(substr(numident$birth_date, nchar(numident$birth_date)-4, nchar(numident$birth_date)))
numident=numident[which(numident$birth_year>=1930 & numident$birth_year<1940),]
numident$cohort_member_id=seq(1:nrow(numident))
numident=numident[which(numident$cohort_member_id %in% multiples),]
multiple_sample$mother_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "mother_name"])
multiple_sample$father_name=tolower(numident[match(multiple_sample$cohort_member_id, numident$cohort_member_id), "father_name"])
load("intermediate_outputs/extract_census_samples/households_1940.rda")
multiple_sample$hhid_1940=households_1940_histid[match(multiple_sample$histid_ct_1940, households_1940_histid$histid),"serial"]
load("intermediate_outputs/extract_census_samples/restricted/restricted_1940.rda")
multiple_sample$father_name_census_1940=0
multiple_sample$mother_name_census_1940=0
load("intermediate_outputs/extract_census_samples/public/public_1940.rda")

for (i in 1:nrow(multiple_sample)){
  if (is.na(multiple_sample[i, "hhid_1940"])==FALSE){
    tmp=public_1940[which(public_1940$serial==multiple_sample[i, "hhid_1940"]),]
    tmp=unique(tmp$histid)
    tmp=restricted_1940[which(restricted_1940$histid %in% tmp),]
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"poploc"]),]
      multiple_sample$father_name_census_1940[i]=tolower(tmp2$namefrst)
    }
    if (tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]==0){
      multiple_sample$father_name_census_1940[i]=NA
    } else{
      tmp2=tmp[which(tmp$pernum==tmp[which(tmp$histid==multiple_sample[i, "histid_ct_1940"]),"momloc"]),]
      multiple_sample$mother_name_census_1940[i]=tolower(tmp2$namefrst)
    }
  } else{
    multiple_sample$father_name_census_1940[i]=NA
    multiple_sample$mother_name_census_1940[i]=NA
  }
}


#next stage string distances (soundex?) between parent names to find closest match! Then thats it, replicate for each cohort!

test=multiple_sample
library(stringdist)


load("input_data/step_1_misc_data/name_dict.rda")

all_possible_keys=list()
first_names_ita=list()
name_match_list=unique(name_match_list)
for (i in 1:length(name_match_list)){
  first_names_ita[[i]]=name_match_list[[i]][[1]]
}
first_names_ita=unlist(first_names_ita)

test$mean_parent_difference=NA
#below loop handles NAs in parent names and anglicanizations of names
for (i in 1:nrow(test)){
  value=matrix(nrow=2, ncol=3)
  if (is.na(test$father_name[i])){
    value[1,1]=0 #column 1 records distance column 2 records if NA
    value[1,2]=0 #0 means NA for this position- do not use in averages
    value[1,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[1,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$father_name_census_1940[i])|test$father_name_census_1940[i]==0){
      value[1,1]=0
      value[1,2]=0
    } else{
      value[1,2]=1
      a=stringdist(test$father_name[i], test$father_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$father_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$father_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[1,1]=min(stringdist(alt_names, test$father_name_census_1940[i], method="soundex"))
          } else{
            value[1,1]=a
          }
        } else{
          value[1,1]=a
        }
      } else{
        value[1,1]=a
      }
    }
  }
  if (is.na(test$mother_name[i])){
    value[2,1]=0 #column 1 records distance column 2 records if NA
    value[2,2]=0 #0 means NA for this position- do not use in averages
    value[2,3]=0 #0 RECORDS missing parent info from numident for column 3- no matches possible
  } else{
    value[2,3]=1 #1 RECORDS present parent info from numident for colum
    if (is.na(test$mother_name_census_1940[i])|test$mother_name_census_1940[i]==0){
      value[2,1]=0
      value[2,2]=0
    } else{
      value[2,2]=1
      a=stringdist(test$mother_name[i], test$mother_name_census_1940[i], method = "soundex")
      if (a > 0){
        if ((tolower(test$mother_name[i]) %in% tolower(first_names_ita))==TRUE){
          alt_names=unlist(name_match_list[[which(tolower(first_names_ita)==tolower(test$mother_name[i]))]][2])
          alt_names=alt_names[which(alt_names!="none")]
          n_alt_names=length(alt_names)
          if (n_alt_names>0){
            value[2,1]=min(stringdist(alt_names, test$mother_name_census_1940[i], method="soundex"))
          } else{
            value[2,1]=a
          }
        } else{
          value[2,1]=a
        }
      } else{
        value[2,1]=a
      }
    }
  }
  if (sum(value[,3])==0){
    test$mean_parent_difference[i]=0.99 #no parent info available in numident is marginally better than parent info being available and not matching anywhere
  } else{
    if (sum(value[,2])==0){
      test$mean_parent_difference[i]=0.99 #no parent info available  in censuses is marginally better than parent info being available and not matching anywhere
    } else{
      test$mean_parent_difference[i]=mean(value[which(value[,2]==1),1]) #assigning average of non-na parent name distances
    }
  }
}


test2=matrix(nrow=length(unique(test$cohort_member_id)), ncol=5)
test2[,1]=unique(test$cohort_member_id)
for (i in 1:nrow(test2)){
  tmp=test[which(test$cohort_member_id==test2[i,1]),]
  tmp$mean_parent_difference=ifelse(tmp$match_method=="fuzzy", tmp$mean_parent_difference+0.1, tmp$mean_parent_difference) #small preference for direct matches
  test2[i,2]=min(tmp$mean_parent_difference)
  test2[i,3]=nrow(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
  if (test2[i,3]>1){
    a=nrow(tmp[which(is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])
    if (a>0){
      tmp2=tmp[which(is.na(tmp$histid_ct_1940)==FALSE & tmp$mean_parent_difference==min(tmp$mean_parent_difference)),]
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp2[which(tmp2$mean_parent_difference==min(tmp2$mean_parent_difference)),])))) #with ties, decision based on more complete data, extra preference towards complete histid set
    } else{
      test2[i,4]=as.numeric(which.min(rowSums(is.na(tmp[which(tmp$mean_parent_difference==min(tmp$mean_parent_difference)),])))) #with ties, decision based on more complete data
    }
  } else{
    test2[i,4]=which.min(tmp$mean_parent_difference)
  }
  test2[i,5]=paste0(tmp[as.numeric(test2[i,4]), "individual_string"], "_", rownames(tmp[as.numeric(test2[i,4]),]))
}

#last to combine with 1 to 1 individual matches and create matched data set
test$preferred=ifelse((paste0(test$individual_string,"_", rownames(test)) %in% test2[,5])==TRUE, 1, 0)
test=test[which(test$preferred==1),]
length(unique(test$cohort_member_id))

cohort_1940=rbind.data.frame(single_sample, test[,c(1:7)])
length(unique(cohort_1940$cohort_member_id)) #PERFECT!!!!!!!!!!!!!
save(cohort_1940, file="intermediate_outputs/matched_records/cohort_1940.rda")






rm(list=ls())
gc()