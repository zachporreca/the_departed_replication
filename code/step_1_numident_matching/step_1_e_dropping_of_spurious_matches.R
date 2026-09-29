###############################################################################################################################
##################################### Step 1e: Initial dropping of spurious matches  ##########################################
###############################################################################################################################



#1930 cohort
#to 1940
load("intermediate_outputs/fuzzy_matches_1930_1940.rda")
load("intermediate_outputs/direct_matches_1930_1940.rda")
matches_1930_1940=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1930_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1930_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1930_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1930_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1930_1940[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1930_1940[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1930_1940[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1930_1940[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1930_1940[[i]]=NA
      }
    }
  }
  if (length(matches_1930_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1930_1940[[i]])[1]==4 & dim(matches_1930_1940[[i]])[2] != 4){
      if (matches_1930_1940[[i]][2,1]=="direct"|matches_1930_1940[[i]][2,1]=="fuzzy"){
        matches_1930_1940[[i]]=t(as.matrix(matches_1930_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric))[,1])

restricted_1940=as.data.frame(read.csv(file="input_data/census_restricted/census_1940.csv"))
#limiting only to matches born in italy
restricted_1940=restricted_1940[which(restricted_1940$bpl=="Italy"),]
hist_1940=unique(restricted_1940$histid)
before=matches_1930_1940
for (i in 1:length(matches_1930_1940)){
  if (length(matches_1930_1940[[i]])>1){
    if (nrow(matches_1930_1940[[i]])==1){
      if (nrow(as.matrix(matches_1930_1940[[i]][which((matches_1930_1940[[i]][,4] %in% hist_1940)==TRUE),]))==0){
        matches_1930_1940[[i]]=as.matrix(matches_1930_1940[[i]][which((matches_1930_1940[[i]][,4] %in% hist_1940)==TRUE),])
      } else{
        matches_1930_1940[[i]]=t(as.matrix(matches_1930_1940[[i]][which((matches_1930_1940[[i]][,4] %in% hist_1940)==TRUE),]))
      }
    } else{
      matches_1930_1940[[i]]=as.matrix(matches_1930_1940[[i]][which((matches_1930_1940[[i]][,4] %in% hist_1940)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1930_1940)){
  if (length(matches_1930_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1930_1940[[i]])[1]==4 & dim(matches_1930_1940[[i]])[2] != 4){
      if (matches_1930_1940[[i]][2,1]=="direct"|matches_1930_1940[[i]][2,1]=="fuzzy"){
        matches_1930_1940[[i]]=t(as.matrix(matches_1930_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1930_1940)){
  if (length(matches_1930_1940[[i]])==1){
    next
  } else{
    if (length(matches_1930_1940[[i]])==0){
      matches_1930_1940[[i]]=NA
    } else{
      if (dim(matches_1930_1940[[i]])[1]==0){
        matches_1930_1940[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1930_1940, dim), as.numeric))[,1])

save(matches_1930_1940, file="intermediate_outputs/step_1_potential_census_matches/matches_1930_1940.rda")

#1930 cohort
#to 1930
load("intermediate_outputs/fuzzy_matches_1930_1930.rda")
load("intermediate_outputs/direct_matches_1930_1930.rda")
matches_1930_1930=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1930_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1930_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1930_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1930_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1930_1930[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1930_1930[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1930_1930[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1930_1930[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1930_1930[[i]]=NA
      }
    }
  }
  if (length(matches_1930_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1930_1930[[i]])[1]==4 & dim(matches_1930_1930[[i]])[2] != 4){
      if (matches_1930_1930[[i]][2,1]=="direct"|matches_1930_1930[[i]][2,1]=="fuzzy"){
        matches_1930_1930[[i]]=t(as.matrix(matches_1930_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric))[,1])

restricted_1930=as.data.frame(read.csv(file="input_data/census_restricted/census_1930.csv"))
#limiting only to matches born in italy
restricted_1930=restricted_1930[which(restricted_1930$bpl=="Italy"),]
hist_1930=unique(restricted_1930$histid)
before=matches_1930_1930
for (i in 1:length(matches_1930_1930)){
  if (length(matches_1930_1930[[i]])>1){
    if (nrow(matches_1930_1930[[i]])==1){
      if (nrow(as.matrix(matches_1930_1930[[i]][which((matches_1930_1930[[i]][,4] %in% hist_1930)==TRUE),]))==0){
        matches_1930_1930[[i]]=as.matrix(matches_1930_1930[[i]][which((matches_1930_1930[[i]][,4] %in% hist_1930)==TRUE),])
      } else{
        matches_1930_1930[[i]]=t(as.matrix(matches_1930_1930[[i]][which((matches_1930_1930[[i]][,4] %in% hist_1930)==TRUE),]))
      }
    } else{
      matches_1930_1930[[i]]=as.matrix(matches_1930_1930[[i]][which((matches_1930_1930[[i]][,4] %in% hist_1930)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1930_1930)){
  if (length(matches_1930_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1930_1930[[i]])[1]==4 & dim(matches_1930_1930[[i]])[2] != 4){
      if (matches_1930_1930[[i]][2,1]=="direct"|matches_1930_1930[[i]][2,1]=="fuzzy"){
        matches_1930_1930[[i]]=t(as.matrix(matches_1930_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1930_1930)){
  if (length(matches_1930_1930[[i]])==1){
    next
  } else{
    if (length(matches_1930_1930[[i]])==0){
      matches_1930_1930[[i]]=NA
    } else{
      if (dim(matches_1930_1930[[i]])[1]==0){
        matches_1930_1930[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1930_1930, dim), as.numeric))[,1])

save(matches_1930_1930, file="intermediate_outputs/step_1_potential_census_matches/matches_1930_1930.rda")

#1940 cohort
#to 1940
load("intermediate_outputs/fuzzy_matches_1940_1940.rda")
load("intermediate_outputs/direct_matches_1940_1940.rda")
matches_1940_1940=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1940_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1940_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1940_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1940_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1940_1940[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1940_1940[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1940_1940[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1940_1940[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1940_1940[[i]]=NA
      }
    }
  }
  if (length(matches_1940_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1940_1940[[i]])[1]==4 & dim(matches_1940_1940[[i]])[2] != 4){
      if (matches_1940_1940[[i]][2,1]=="direct"|matches_1940_1940[[i]][2,1]=="fuzzy"){
        matches_1940_1940[[i]]=t(as.matrix(matches_1940_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric))[,1])

restricted_1940=as.data.frame(read.csv(file="input_data/census_restricted/census_1940.csv"))
#limiting only to matches born in italy
restricted_1940=restricted_1940[which(restricted_1940$bpl=="Italy"),]
hist_1940=unique(restricted_1940$histid)
before=matches_1940_1940
for (i in 1:length(matches_1940_1940)){
  if (length(matches_1940_1940[[i]])>1){
    if (nrow(matches_1940_1940[[i]])==1){
      if (nrow(as.matrix(matches_1940_1940[[i]][which((matches_1940_1940[[i]][,4] %in% hist_1940)==TRUE),]))==0){
        matches_1940_1940[[i]]=as.matrix(matches_1940_1940[[i]][which((matches_1940_1940[[i]][,4] %in% hist_1940)==TRUE),])
      } else{
        matches_1940_1940[[i]]=t(as.matrix(matches_1940_1940[[i]][which((matches_1940_1940[[i]][,4] %in% hist_1940)==TRUE),]))
      }
    } else{
      matches_1940_1940[[i]]=as.matrix(matches_1940_1940[[i]][which((matches_1940_1940[[i]][,4] %in% hist_1940)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1940_1940)){
  if (length(matches_1940_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1940_1940[[i]])[1]==4 & dim(matches_1940_1940[[i]])[2] != 4){
      if (matches_1940_1940[[i]][2,1]=="direct"|matches_1940_1940[[i]][2,1]=="fuzzy"){
        matches_1940_1940[[i]]=t(as.matrix(matches_1940_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1940_1940)){
  if (length(matches_1940_1940[[i]])==1){
    next
  } else{
    if (length(matches_1940_1940[[i]])==0){
      matches_1940_1940[[i]]=NA
    } else{
      if (dim(matches_1940_1940[[i]])[1]==0){
        matches_1940_1940[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1940_1940, dim), as.numeric))[,1])

save(matches_1940_1940, file="intermediate_outputs/step_1_potential_census_matches/matches_1940_1940.rda")


#now to 1920 cohorts
#1920 cohort
#to 1940
load("intermediate_outputs/fuzzy_matches_1920_1940.rda")
load("intermediate_outputs/direct_matches_1920_1940.rda")
matches_1920_1940=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1920_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1920_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1920_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1940[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1920_1940[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1920_1940[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1920_1940[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1920_1940[[i]]=NA
      }
    }
  }
  if (length(matches_1920_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1940[[i]])[1]==4 & dim(matches_1920_1940[[i]])[2] != 4){
      if (matches_1920_1940[[i]][2,1]=="direct"|matches_1920_1940[[i]][2,1]=="fuzzy"){
        matches_1920_1940[[i]]=t(as.matrix(matches_1920_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric))[,1])

restricted_1940=as.data.frame(read.csv(file="input_data/census_restricted/census_1940.csv"))
#limiting only to matches born in italy
restricted_1940=restricted_1940[which(restricted_1940$bpl=="Italy"),]
hist_1940=unique(restricted_1940$histid)
before=matches_1920_1940
for (i in 1:length(matches_1920_1940)){
  if (length(matches_1920_1940[[i]])>1){
    if (nrow(matches_1920_1940[[i]])==1){
      if (nrow(as.matrix(matches_1920_1940[[i]][which((matches_1920_1940[[i]][,4] %in% hist_1940)==TRUE),]))==0){
        matches_1920_1940[[i]]=as.matrix(matches_1920_1940[[i]][which((matches_1920_1940[[i]][,4] %in% hist_1940)==TRUE),])
      } else{
        matches_1920_1940[[i]]=t(as.matrix(matches_1920_1940[[i]][which((matches_1920_1940[[i]][,4] %in% hist_1940)==TRUE),]))
      }
    } else{
      matches_1920_1940[[i]]=as.matrix(matches_1920_1940[[i]][which((matches_1920_1940[[i]][,4] %in% hist_1940)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1920_1940)){
  if (length(matches_1920_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1940[[i]])[1]==4 & dim(matches_1920_1940[[i]])[2] != 4){
      if (matches_1920_1940[[i]][2,1]=="direct"|matches_1920_1940[[i]][2,1]=="fuzzy"){
        matches_1920_1940[[i]]=t(as.matrix(matches_1920_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1920_1940)){
  if (length(matches_1920_1940[[i]])==1){
    next
  } else{
    if (length(matches_1920_1940[[i]])==0){
      matches_1920_1940[[i]]=NA
    } else{
      if (dim(matches_1920_1940[[i]])[1]==0){
        matches_1920_1940[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1940, dim), as.numeric))[,1])

save(matches_1920_1940, file="intermediate_outputs/step_1_potential_census_matches/matches_1920_1940.rda")

#1920 cohort
#to 1930
load("intermediate_outputs/fuzzy_matches_1920_1930.rda")
load("intermediate_outputs/direct_matches_1920_1930.rda")
matches_1920_1930=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1920_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1920_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1920_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1930[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1920_1930[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1920_1930[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1920_1930[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1920_1930[[i]]=NA
      }
    }
  }
  if (length(matches_1920_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1930[[i]])[1]==4 & dim(matches_1920_1930[[i]])[2] != 4){
      if (matches_1920_1930[[i]][2,1]=="direct"|matches_1920_1930[[i]][2,1]=="fuzzy"){
        matches_1920_1930[[i]]=t(as.matrix(matches_1920_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric))[,1])

restricted_1930=as.data.frame(read.csv(file="input_data/census_restricted/census_1930.csv"))
#limiting only to matches born in italy
restricted_1930=restricted_1930[which(restricted_1930$bpl=="Italy"),]
hist_1930=unique(restricted_1930$histid)
before=matches_1920_1930
for (i in 1:length(matches_1920_1930)){
  if (length(matches_1920_1930[[i]])>1){
    if (nrow(matches_1920_1930[[i]])==1){
      if (nrow(as.matrix(matches_1920_1930[[i]][which((matches_1920_1930[[i]][,4] %in% hist_1930)==TRUE),]))==0){
        matches_1920_1930[[i]]=as.matrix(matches_1920_1930[[i]][which((matches_1920_1930[[i]][,4] %in% hist_1930)==TRUE),])
      } else{
        matches_1920_1930[[i]]=t(as.matrix(matches_1920_1930[[i]][which((matches_1920_1930[[i]][,4] %in% hist_1930)==TRUE),]))
      }
    } else{
      matches_1920_1930[[i]]=as.matrix(matches_1920_1930[[i]][which((matches_1920_1930[[i]][,4] %in% hist_1930)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1920_1930)){
  if (length(matches_1920_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1930[[i]])[1]==4 & dim(matches_1920_1930[[i]])[2] != 4){
      if (matches_1920_1930[[i]][2,1]=="direct"|matches_1920_1930[[i]][2,1]=="fuzzy"){
        matches_1920_1930[[i]]=t(as.matrix(matches_1920_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1920_1930)){
  if (length(matches_1920_1930[[i]])==1){
    next
  } else{
    if (length(matches_1920_1930[[i]])==0){
      matches_1920_1930[[i]]=NA
    } else{
      if (dim(matches_1920_1930[[i]])[1]==0){
        matches_1920_1930[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1930, dim), as.numeric))[,1])

save(matches_1920_1930, file="intermediate_outputs/step_1_potential_census_matches/matches_1920_1930.rda")


#1920 cohort
#to 1920
load("intermediate_outputs/fuzzy_matches_1920_1920.rda")
load("intermediate_outputs/direct_matches_1920_1920.rda")
matches_1920_1920=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1920_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1920_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1920_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1920_1920[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1920_1920[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1920_1920[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1920_1920[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1920_1920[[i]]=NA
      }
    }
  }
  if (length(matches_1920_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1920[[i]])[1]==4 & dim(matches_1920_1920[[i]])[2] != 4){
      if (matches_1920_1920[[i]][2,1]=="direct"|matches_1920_1920[[i]][2,1]=="fuzzy"){
        matches_1920_1920[[i]]=t(as.matrix(matches_1920_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric))[,1])

restricted_1920=as.data.frame(read.csv(file="input_data/census_restricted/census_1920.csv"))
#limiting only to matches born in italy
restricted_1920=restricted_1920[which(restricted_1920$bpl=="Italy"),]
hist_1920=unique(restricted_1920$histid)
before=matches_1920_1920
for (i in 1:length(matches_1920_1920)){
  if (length(matches_1920_1920[[i]])>1){
    if (nrow(matches_1920_1920[[i]])==1){
      if (nrow(as.matrix(matches_1920_1920[[i]][which((matches_1920_1920[[i]][,4] %in% hist_1920)==TRUE),]))==0){
        matches_1920_1920[[i]]=as.matrix(matches_1920_1920[[i]][which((matches_1920_1920[[i]][,4] %in% hist_1920)==TRUE),])
      } else{
        matches_1920_1920[[i]]=t(as.matrix(matches_1920_1920[[i]][which((matches_1920_1920[[i]][,4] %in% hist_1920)==TRUE),]))
      }
    } else{
      matches_1920_1920[[i]]=as.matrix(matches_1920_1920[[i]][which((matches_1920_1920[[i]][,4] %in% hist_1920)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1920_1920)){
  if (length(matches_1920_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1920_1920[[i]])[1]==4 & dim(matches_1920_1920[[i]])[2] != 4){
      if (matches_1920_1920[[i]][2,1]=="direct"|matches_1920_1920[[i]][2,1]=="fuzzy"){
        matches_1920_1920[[i]]=t(as.matrix(matches_1920_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1920_1920)){
  if (length(matches_1920_1920[[i]])==1){
    next
  } else{
    if (length(matches_1920_1920[[i]])==0){
      matches_1920_1920[[i]]=NA
    } else{
      if (dim(matches_1920_1920[[i]])[1]==0){
        matches_1920_1920[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1920_1920, dim), as.numeric))[,1])

save(matches_1920_1920, file="intermediate_outputs/step_1_potential_census_matches/matches_1920_1920.rda")


#now to 1910 cohorts
#1910 cohort
#to 1940
load("intermediate_outputs/fuzzy_matches_1910_1940.rda")
load("intermediate_outputs/direct_matches_1910_1940.rda")
matches_1910_1940=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1910_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1910_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1910_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1940[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1910_1940[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1910_1940[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1910_1940[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1910_1940[[i]]=NA
      }
    }
  }
  if (length(matches_1910_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1940[[i]])[1]==4 & dim(matches_1910_1940[[i]])[2] != 4){
      if (matches_1910_1940[[i]][2,1]=="direct"|matches_1910_1940[[i]][2,1]=="fuzzy"){
        matches_1910_1940[[i]]=t(as.matrix(matches_1910_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric))[,1])

restricted_1940=as.data.frame(read.csv(file="input_data/census_restricted/census_1940.csv"))
#limiting only to matches born in italy
restricted_1940=restricted_1940[which(restricted_1940$bpl=="Italy"),]
hist_1940=unique(restricted_1940$histid)
before=matches_1910_1940
for (i in 1:length(matches_1910_1940)){
  if (length(matches_1910_1940[[i]])>1){
    if (nrow(matches_1910_1940[[i]])==1){
      if (nrow(as.matrix(matches_1910_1940[[i]][which((matches_1910_1940[[i]][,4] %in% hist_1940)==TRUE),]))==0){
        matches_1910_1940[[i]]=as.matrix(matches_1910_1940[[i]][which((matches_1910_1940[[i]][,4] %in% hist_1940)==TRUE),])
      } else{
        matches_1910_1940[[i]]=t(as.matrix(matches_1910_1940[[i]][which((matches_1910_1940[[i]][,4] %in% hist_1940)==TRUE),]))
      }
    } else{
      matches_1910_1940[[i]]=as.matrix(matches_1910_1940[[i]][which((matches_1910_1940[[i]][,4] %in% hist_1940)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1910_1940)){
  if (length(matches_1910_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1940[[i]])[1]==4 & dim(matches_1910_1940[[i]])[2] != 4){
      if (matches_1910_1940[[i]][2,1]=="direct"|matches_1910_1940[[i]][2,1]=="fuzzy"){
        matches_1910_1940[[i]]=t(as.matrix(matches_1910_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1910_1940)){
  if (length(matches_1910_1940[[i]])==1){
    next
  } else{
    if (length(matches_1910_1940[[i]])==0){
      matches_1910_1940[[i]]=NA
    } else{
      if (dim(matches_1910_1940[[i]])[1]==0){
        matches_1910_1940[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1940, dim), as.numeric))[,1])

save(matches_1910_1940, file="intermediate_outputs/step_1_potential_census_matches/matches_1910_1940.rda")

#1910 cohort
#to 1930
load("intermediate_outputs/fuzzy_matches_1910_1930.rda")
load("intermediate_outputs/direct_matches_1910_1930.rda")
matches_1910_1930=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1910_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1910_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1910_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1930[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1910_1930[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1910_1930[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1910_1930[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1910_1930[[i]]=NA
      }
    }
  }
  if (length(matches_1910_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1930[[i]])[1]==4 & dim(matches_1910_1930[[i]])[2] != 4){
      if (matches_1910_1930[[i]][2,1]=="direct"|matches_1910_1930[[i]][2,1]=="fuzzy"){
        matches_1910_1930[[i]]=t(as.matrix(matches_1910_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric))[,1])

restricted_1930=as.data.frame(read.csv(file="input_data/census_restricted/census_1930.csv"))
#limiting only to matches born in italy
restricted_1930=restricted_1930[which(restricted_1930$bpl=="Italy"),]
hist_1930=unique(restricted_1930$histid)
before=matches_1910_1930
for (i in 1:length(matches_1910_1930)){
  if (length(matches_1910_1930[[i]])>1){
    if (nrow(matches_1910_1930[[i]])==1){
      if (nrow(as.matrix(matches_1910_1930[[i]][which((matches_1910_1930[[i]][,4] %in% hist_1930)==TRUE),]))==0){
        matches_1910_1930[[i]]=as.matrix(matches_1910_1930[[i]][which((matches_1910_1930[[i]][,4] %in% hist_1930)==TRUE),])
      } else{
        matches_1910_1930[[i]]=t(as.matrix(matches_1910_1930[[i]][which((matches_1910_1930[[i]][,4] %in% hist_1930)==TRUE),]))
      }
    } else{
      matches_1910_1930[[i]]=as.matrix(matches_1910_1930[[i]][which((matches_1910_1930[[i]][,4] %in% hist_1930)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1910_1930)){
  if (length(matches_1910_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1930[[i]])[1]==4 & dim(matches_1910_1930[[i]])[2] != 4){
      if (matches_1910_1930[[i]][2,1]=="direct"|matches_1910_1930[[i]][2,1]=="fuzzy"){
        matches_1910_1930[[i]]=t(as.matrix(matches_1910_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1910_1930)){
  if (length(matches_1910_1930[[i]])==1){
    next
  } else{
    if (length(matches_1910_1930[[i]])==0){
      matches_1910_1930[[i]]=NA
    } else{
      if (dim(matches_1910_1930[[i]])[1]==0){
        matches_1910_1930[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1930, dim), as.numeric))[,1])

save(matches_1910_1930, file="intermediate_outputs/step_1_potential_census_matches/matches_1910_1930.rda")


#1910 cohort
#to 1920
load("intermediate_outputs/fuzzy_matches_1910_1920.rda")
load("intermediate_outputs/direct_matches_1910_1920.rda")
matches_1910_1920=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1910_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1910_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1910_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1920[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1910_1920[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1910_1920[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1910_1920[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1910_1920[[i]]=NA
      }
    }
  }
  if (length(matches_1910_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1920[[i]])[1]==4 & dim(matches_1910_1920[[i]])[2] != 4){
      if (matches_1910_1920[[i]][2,1]=="direct"|matches_1910_1920[[i]][2,1]=="fuzzy"){
        matches_1910_1920[[i]]=t(as.matrix(matches_1910_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric))[,1])

restricted_1920=as.data.frame(read.csv(file="input_data/census_restricted/census_1920.csv"))
#limiting only to matches born in italy
restricted_1920=restricted_1920[which(restricted_1920$bpl=="Italy"),]
hist_1920=unique(restricted_1920$histid)
before=matches_1910_1920
for (i in 1:length(matches_1910_1920)){
  if (length(matches_1910_1920[[i]])>1){
    if (nrow(matches_1910_1920[[i]])==1){
      if (nrow(as.matrix(matches_1910_1920[[i]][which((matches_1910_1920[[i]][,4] %in% hist_1920)==TRUE),]))==0){
        matches_1910_1920[[i]]=as.matrix(matches_1910_1920[[i]][which((matches_1910_1920[[i]][,4] %in% hist_1920)==TRUE),])
      } else{
        matches_1910_1920[[i]]=t(as.matrix(matches_1910_1920[[i]][which((matches_1910_1920[[i]][,4] %in% hist_1920)==TRUE),]))
      }
    } else{
      matches_1910_1920[[i]]=as.matrix(matches_1910_1920[[i]][which((matches_1910_1920[[i]][,4] %in% hist_1920)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1910_1920)){
  if (length(matches_1910_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1920[[i]])[1]==4 & dim(matches_1910_1920[[i]])[2] != 4){
      if (matches_1910_1920[[i]][2,1]=="direct"|matches_1910_1920[[i]][2,1]=="fuzzy"){
        matches_1910_1920[[i]]=t(as.matrix(matches_1910_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1910_1920)){
  if (length(matches_1910_1920[[i]])==1){
    next
  } else{
    if (length(matches_1910_1920[[i]])==0){
      matches_1910_1920[[i]]=NA
    } else{
      if (dim(matches_1910_1920[[i]])[1]==0){
        matches_1910_1920[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1920, dim), as.numeric))[,1])

save(matches_1910_1920, file="intermediate_outputs/step_1_potential_census_matches/matches_1910_1920.rda")

#1910 cohort
#to 1910
load("intermediate_outputs/fuzzy_matches_1910_1910.rda")
load("intermediate_outputs/direct_matches_1910_1910.rda")
matches_1910_1910=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1910_1910[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1910[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1910_1910[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1910_1910[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1910_1910[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1910_1910[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1910_1910[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1910_1910[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1910_1910[[i]]=NA
      }
    }
  }
  if (length(matches_1910_1910[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1910[[i]])[1]==4 & dim(matches_1910_1910[[i]])[2] != 4){
      if (matches_1910_1910[[i]][2,1]=="direct"|matches_1910_1910[[i]][2,1]=="fuzzy"){
        matches_1910_1910[[i]]=t(as.matrix(matches_1910_1910[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric))[,1])

restricted_1910=as.data.frame(read.csv(file="input_data/census_restricted/census_1910.csv"))
#limiting only to matches born in italy
restricted_1910=restricted_1910[which(restricted_1910$bpl=="Italy"),]
hist_1910=unique(restricted_1910$histid)
before=matches_1910_1910
for (i in 1:length(matches_1910_1910)){
  if (length(matches_1910_1910[[i]])>1){
    if (nrow(matches_1910_1910[[i]])==1){
      if (nrow(as.matrix(matches_1910_1910[[i]][which((matches_1910_1910[[i]][,4] %in% hist_1910)==TRUE),]))==0){
        matches_1910_1910[[i]]=as.matrix(matches_1910_1910[[i]][which((matches_1910_1910[[i]][,4] %in% hist_1910)==TRUE),])
      } else{
        matches_1910_1910[[i]]=t(as.matrix(matches_1910_1910[[i]][which((matches_1910_1910[[i]][,4] %in% hist_1910)==TRUE),]))
      }
    } else{
      matches_1910_1910[[i]]=as.matrix(matches_1910_1910[[i]][which((matches_1910_1910[[i]][,4] %in% hist_1910)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1910_1910)){
  if (length(matches_1910_1910[[i]])==1){
    next
  } else{
    if (dim(matches_1910_1910[[i]])[1]==4 & dim(matches_1910_1910[[i]])[2] != 4){
      if (matches_1910_1910[[i]][2,1]=="direct"|matches_1910_1910[[i]][2,1]=="fuzzy"){
        matches_1910_1910[[i]]=t(as.matrix(matches_1910_1910[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1910_1910)){
  if (length(matches_1910_1910[[i]])==1){
    next
  } else{
    if (length(matches_1910_1910[[i]])==0){
      matches_1910_1910[[i]]=NA
    } else{
      if (dim(matches_1910_1910[[i]])[1]==0){
        matches_1910_1910[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1910_1910, dim), as.numeric))[,1])

save(matches_1910_1910, file="intermediate_outputs/step_1_potential_census_matches/matches_1910_1910.rda")



#now to 1900 cohorts
#1900 cohort
#to 1940
load("intermediate_outputs/fuzzy_matches_1900_1940.rda")
load("intermediate_outputs/direct_matches_1900_1940.rda")
matches_1900_1940=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1900_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1900_1940[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1900_1940[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1940[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1900_1940[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1900_1940[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1900_1940[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1900_1940[[i]]=NA
      }
    }
  }
  if (length(matches_1900_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1940[[i]])[1]==4 & dim(matches_1900_1940[[i]])[2] != 4){
      if (matches_1900_1940[[i]][2,1]=="direct"|matches_1900_1940[[i]][2,1]=="fuzzy"){
        matches_1900_1940[[i]]=t(as.matrix(matches_1900_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric))[,1])

restricted_1940=as.data.frame(read.csv(file="input_data/census_restricted/census_1940.csv"))
#limiting only to matches born in italy
restricted_1940=restricted_1940[which(restricted_1940$bpl=="Italy"),]
hist_1940=unique(restricted_1940$histid)
before=matches_1900_1940
for (i in 1:length(matches_1900_1940)){
  if (length(matches_1900_1940[[i]])>1){
    if (nrow(matches_1900_1940[[i]])==1){
      if (nrow(as.matrix(matches_1900_1940[[i]][which((matches_1900_1940[[i]][,4] %in% hist_1940)==TRUE),]))==0){
        matches_1900_1940[[i]]=as.matrix(matches_1900_1940[[i]][which((matches_1900_1940[[i]][,4] %in% hist_1940)==TRUE),])
      } else{
        matches_1900_1940[[i]]=t(as.matrix(matches_1900_1940[[i]][which((matches_1900_1940[[i]][,4] %in% hist_1940)==TRUE),]))
      }
    } else{
      matches_1900_1940[[i]]=as.matrix(matches_1900_1940[[i]][which((matches_1900_1940[[i]][,4] %in% hist_1940)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1900_1940)){
  if (length(matches_1900_1940[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1940[[i]])[1]==4 & dim(matches_1900_1940[[i]])[2] != 4){
      if (matches_1900_1940[[i]][2,1]=="direct"|matches_1900_1940[[i]][2,1]=="fuzzy"){
        matches_1900_1940[[i]]=t(as.matrix(matches_1900_1940[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1900_1940)){
  if (length(matches_1900_1940[[i]])==1){
    next
  } else{
    if (length(matches_1900_1940[[i]])==0){
      matches_1900_1940[[i]]=NA
    } else{
      if (dim(matches_1900_1940[[i]])[1]==0){
        matches_1900_1940[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1940, dim), as.numeric))[,1])

save(matches_1900_1940, file="intermediate_outputs/step_1_potential_census_matches/matches_1900_1940.rda")

#1900 cohort
#to 1930
load("intermediate_outputs/fuzzy_matches_1900_1930.rda")
load("intermediate_outputs/direct_matches_1900_1930.rda")
matches_1900_1930=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1900_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1900_1930[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1900_1930[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1930[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1900_1930[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1900_1930[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1900_1930[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1900_1930[[i]]=NA
      }
    }
  }
  if (length(matches_1900_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1930[[i]])[1]==4 & dim(matches_1900_1930[[i]])[2] != 4){
      if (matches_1900_1930[[i]][2,1]=="direct"|matches_1900_1930[[i]][2,1]=="fuzzy"){
        matches_1900_1930[[i]]=t(as.matrix(matches_1900_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric))[,1])

restricted_1930=as.data.frame(read.csv(file="input_data/census_restricted/census_1930.csv"))
#limiting only to matches born in italy
restricted_1930=restricted_1930[which(restricted_1930$bpl=="Italy"),]
hist_1930=unique(restricted_1930$histid)
before=matches_1900_1930
for (i in 1:length(matches_1900_1930)){
  if (length(matches_1900_1930[[i]])>1){
    if (nrow(matches_1900_1930[[i]])==1){
      if (nrow(as.matrix(matches_1900_1930[[i]][which((matches_1900_1930[[i]][,4] %in% hist_1930)==TRUE),]))==0){
        matches_1900_1930[[i]]=as.matrix(matches_1900_1930[[i]][which((matches_1900_1930[[i]][,4] %in% hist_1930)==TRUE),])
      } else{
        matches_1900_1930[[i]]=t(as.matrix(matches_1900_1930[[i]][which((matches_1900_1930[[i]][,4] %in% hist_1930)==TRUE),]))
      }
    } else{
      matches_1900_1930[[i]]=as.matrix(matches_1900_1930[[i]][which((matches_1900_1930[[i]][,4] %in% hist_1930)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1900_1930)){
  if (length(matches_1900_1930[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1930[[i]])[1]==4 & dim(matches_1900_1930[[i]])[2] != 4){
      if (matches_1900_1930[[i]][2,1]=="direct"|matches_1900_1930[[i]][2,1]=="fuzzy"){
        matches_1900_1930[[i]]=t(as.matrix(matches_1900_1930[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1900_1930)){
  if (length(matches_1900_1930[[i]])==1){
    next
  } else{
    if (length(matches_1900_1930[[i]])==0){
      matches_1900_1930[[i]]=NA
    } else{
      if (dim(matches_1900_1930[[i]])[1]==0){
        matches_1900_1930[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1930, dim), as.numeric))[,1])

save(matches_1900_1930, file="intermediate_outputs/step_1_potential_census_matches/matches_1900_1930.rda")


#1900 cohort
#to 1920
load("intermediate_outputs/fuzzy_matches_1900_1920.rda")
load("intermediate_outputs/direct_matches_1900_1920.rda")
matches_1900_1920=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1900_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1900_1920[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1900_1920[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1920[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1900_1920[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1900_1920[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1900_1920[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1900_1920[[i]]=NA
      }
    }
  }
  if (length(matches_1900_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1920[[i]])[1]==4 & dim(matches_1900_1920[[i]])[2] != 4){
      if (matches_1900_1920[[i]][2,1]=="direct"|matches_1900_1920[[i]][2,1]=="fuzzy"){
        matches_1900_1920[[i]]=t(as.matrix(matches_1900_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric))[,1])

restricted_1920=as.data.frame(read.csv(file="input_data/census_restricted/census_1920.csv"))
#limiting only to matches born in italy
restricted_1920=restricted_1920[which(restricted_1920$bpl=="Italy"),]
hist_1920=unique(restricted_1920$histid)
before=matches_1900_1920
for (i in 1:length(matches_1900_1920)){
  if (length(matches_1900_1920[[i]])>1){
    if (nrow(matches_1900_1920[[i]])==1){
      if (nrow(as.matrix(matches_1900_1920[[i]][which((matches_1900_1920[[i]][,4] %in% hist_1920)==TRUE),]))==0){
        matches_1900_1920[[i]]=as.matrix(matches_1900_1920[[i]][which((matches_1900_1920[[i]][,4] %in% hist_1920)==TRUE),])
      } else{
        matches_1900_1920[[i]]=t(as.matrix(matches_1900_1920[[i]][which((matches_1900_1920[[i]][,4] %in% hist_1920)==TRUE),]))
      }
    } else{
      matches_1900_1920[[i]]=as.matrix(matches_1900_1920[[i]][which((matches_1900_1920[[i]][,4] %in% hist_1920)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1900_1920)){
  if (length(matches_1900_1920[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1920[[i]])[1]==4 & dim(matches_1900_1920[[i]])[2] != 4){
      if (matches_1900_1920[[i]][2,1]=="direct"|matches_1900_1920[[i]][2,1]=="fuzzy"){
        matches_1900_1920[[i]]=t(as.matrix(matches_1900_1920[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1900_1920)){
  if (length(matches_1900_1920[[i]])==1){
    next
  } else{
    if (length(matches_1900_1920[[i]])==0){
      matches_1900_1920[[i]]=NA
    } else{
      if (dim(matches_1900_1920[[i]])[1]==0){
        matches_1900_1920[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1920, dim), as.numeric))[,1])

save(matches_1900_1920, file="intermediate_outputs/step_1_potential_census_matches/matches_1900_1920.rda")


#1900 cohort
#to 1910
load("intermediate_outputs/fuzzy_matches_1900_1910.rda")
load("intermediate_outputs/direct_matches_1900_1910.rda")
matches_1900_1910=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1900_1910[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1910[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1900_1910[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1900_1910[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1910[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1900_1910[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1900_1910[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1900_1910[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1900_1910[[i]]=NA
      }
    }
  }
  if (length(matches_1900_1910[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1910[[i]])[1]==4 & dim(matches_1900_1910[[i]])[2] != 4){
      if (matches_1900_1910[[i]][2,1]=="direct"|matches_1900_1910[[i]][2,1]=="fuzzy"){
        matches_1900_1910[[i]]=t(as.matrix(matches_1900_1910[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric))[,1])

restricted_1910=as.data.frame(read.csv(file="input_data/census_restricted/census_1910.csv"))
#limiting only to matches born in italy
restricted_1910=restricted_1910[which(restricted_1910$bpl=="Italy"),]
hist_1910=unique(restricted_1910$histid)
before=matches_1900_1910
for (i in 1:length(matches_1900_1910)){
  if (length(matches_1900_1910[[i]])>1){
    if (nrow(matches_1900_1910[[i]])==1){
      if (nrow(as.matrix(matches_1900_1910[[i]][which((matches_1900_1910[[i]][,4] %in% hist_1910)==TRUE),]))==0){
        matches_1900_1910[[i]]=as.matrix(matches_1900_1910[[i]][which((matches_1900_1910[[i]][,4] %in% hist_1910)==TRUE),])
      } else{
        matches_1900_1910[[i]]=t(as.matrix(matches_1900_1910[[i]][which((matches_1900_1910[[i]][,4] %in% hist_1910)==TRUE),]))
      }
    } else{
      matches_1900_1910[[i]]=as.matrix(matches_1900_1910[[i]][which((matches_1900_1910[[i]][,4] %in% hist_1910)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1900_1910)){
  if (length(matches_1900_1910[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1910[[i]])[1]==4 & dim(matches_1900_1910[[i]])[2] != 4){
      if (matches_1900_1910[[i]][2,1]=="direct"|matches_1900_1910[[i]][2,1]=="fuzzy"){
        matches_1900_1910[[i]]=t(as.matrix(matches_1900_1910[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1900_1910)){
  if (length(matches_1900_1910[[i]])==1){
    next
  } else{
    if (length(matches_1900_1910[[i]])==0){
      matches_1900_1910[[i]]=NA
    } else{
      if (dim(matches_1900_1910[[i]])[1]==0){
        matches_1900_1910[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1910, dim), as.numeric))[,1])

save(matches_1900_1910, file="intermediate_outputs/step_1_potential_census_matches/matches_1900_1910.rda")


#1900 cohort
#to 1900
load("intermediate_outputs/fuzzy_matches_1900_1900.rda")
load("intermediate_outputs/direct_matches_1900_1900.rda")
matches_1900_1900=list()
#combining fuzzy and direct into single object
for (i in 1:length(direct_matches)){
  if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])>1){
    if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==4){
      direct_matches[[i]][,2]="direct"
      fuzzy_matches[[i]][2]="fuzzy"
      matches_1900_1900[[i]]=rbind(as.matrix(direct_matches[[i]]), t(as.matrix(fuzzy_matches[[i]])))
    } else{
      if(length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])>4){
        direct_matches[[i]][2]="direct"
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1900[[i]]=rbind(t(as.matrix(direct_matches[[i]])), as.matrix(fuzzy_matches[[i]]))
      } else{ 
        if (length(direct_matches[[i]])==4 & length(fuzzy_matches[[i]])==4){
          direct_matches[[i]][2]="direct"
          fuzzy_matches[[i]][2]="fuzzy"
          matches_1900_1900[[i]]=rbind(t(as.matrix(direct_matches[[i]])), t(as.matrix(fuzzy_matches[[i]])))
        } else{
          direct_matches[[i]][,2]="direct"
          fuzzy_matches[[i]][,2]="fuzzy"
          matches_1900_1900[[i]]=rbind(as.matrix(direct_matches[[i]]), as.matrix(fuzzy_matches[[i]]))
        }
      }
    }
  } else {
    if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>1){
      if (length(direct_matches[[i]])==1 & length(fuzzy_matches[[i]])>4){
        fuzzy_matches[[i]][,2]="fuzzy"
        matches_1900_1900[[i]]=as.matrix(fuzzy_matches[[i]])
      } else{
        fuzzy_matches[[i]][2]="fuzzy"
        matches_1900_1900[[i]]=t(as.matrix(fuzzy_matches[[i]]))
      }
    } else{
      if (length(direct_matches[[i]])>1 & length(fuzzy_matches[[i]])==1){
        if (length(direct_matches[[i]])>4 & length(fuzzy_matches[[i]])==1){
          direct_matches[[i]][,2]="direct"
          matches_1900_1900[[i]]=as.matrix(direct_matches[[i]])
        }else{
          direct_matches[[i]][2]="direct"
          matches_1900_1900[[i]]=t(as.matrix(direct_matches[[i]]))
        }
      } else{
        matches_1900_1900[[i]]=NA
      }
    }
  }
  if (length(matches_1900_1900[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1900[[i]])[1]==4 & dim(matches_1900_1900[[i]])[2] != 4){
      if (matches_1900_1900[[i]][2,1]=="direct"|matches_1900_1900[[i]][2,1]=="fuzzy"){
        matches_1900_1900[[i]]=t(as.matrix(matches_1900_1900[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric))[,1])

restricted_1900=as.data.frame(read.csv(file="input_data/census_restricted/census_1900.csv"))
#limiting only to matches born in italy
restricted_1900=restricted_1900[which(restricted_1900$bpl=="Italy"),]
hist_1900=unique(restricted_1900$histid)
before=matches_1900_1900
for (i in 1:length(matches_1900_1900)){
  if (length(matches_1900_1900[[i]])>1){
    if (nrow(matches_1900_1900[[i]])==1){
      if (nrow(as.matrix(matches_1900_1900[[i]][which((matches_1900_1900[[i]][,4] %in% hist_1900)==TRUE),]))==0){
        matches_1900_1900[[i]]=as.matrix(matches_1900_1900[[i]][which((matches_1900_1900[[i]][,4] %in% hist_1900)==TRUE),])
      } else{
        matches_1900_1900[[i]]=t(as.matrix(matches_1900_1900[[i]][which((matches_1900_1900[[i]][,4] %in% hist_1900)==TRUE),]))
      }
    } else{
      matches_1900_1900[[i]]=as.matrix(matches_1900_1900[[i]][which((matches_1900_1900[[i]][,4] %in% hist_1900)==TRUE),])
    }
  } else{
    next
  }
}

#next loop corrects transposing and such to keep constant format of matches
for (i in 1:length(matches_1900_1900)){
  if (length(matches_1900_1900[[i]])==1){
    next
  } else{
    if (dim(matches_1900_1900[[i]])[1]==4 & dim(matches_1900_1900[[i]])[2] != 4){
      if (matches_1900_1900[[i]][2,1]=="direct"|matches_1900_1900[[i]][2,1]=="fuzzy"){
        matches_1900_1900[[i]]=t(as.matrix(matches_1900_1900[[i]]))
      } else{
        next
      }
    }
    else{
      next
    }
  }
}

#dropping emptied matches
for (i in 1:length(matches_1900_1900)){
  if (length(matches_1900_1900[[i]])==1){
    next
  } else{
    if (length(matches_1900_1900[[i]])==0){
      matches_1900_1900[[i]]=NA
    } else{
      if (dim(matches_1900_1900[[i]])[1]==0){
        matches_1900_1900[[i]]=NA
      } else{
        next
      } 
    }
  }
}
#examinign numbers of matches per observation
sort(unique(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric)))[,1])
unique(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric)))[,2]
plot(table(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric))[,1]))
table(do.call(rbind,lapply(lapply(matches_1900_1900, dim), as.numeric))[,1])

save(matches_1900_1900, file="intermediate_outputs/step_1_potential_census_matches/matches_1900_1900.rda")