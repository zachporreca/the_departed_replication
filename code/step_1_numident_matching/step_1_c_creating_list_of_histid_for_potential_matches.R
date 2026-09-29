###############################################################################################################################
############## Step 1c: generating list of histid from set of potential links for each census year ############################
###############################################################################################################################




#1940
load("intermediate_outputs/fuzzy_matches_1940_1940.rda")
fuzzy_1940_1940=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1930_1940.rda")
fuzzy_1930_1940=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1920_1940.rda")
fuzzy_1920_1940=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1910_1940.rda")
fuzzy_1910_1940=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1900_1940.rda")
fuzzy_1900_1940=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
fuzzy_matches=append(fuzzy_1900_1940, fuzzy_1910_1940)
fuzzy_matches=append(fuzzy_matches, fuzzy_1920_1940)
fuzzy_matches=append(fuzzy_matches, fuzzy_1930_1940)
fuzzy_matches=append(fuzzy_matches, fuzzy_1940_1940)
fuzzy_matches=as.data.frame(do.call(rbind, as.matrix(fuzzy_matches)))

load("intermediate_outputs/direct_matches_1940_1940.rda")
direct_1940_1940=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1930_1940.rda")
direct_1930_1940=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1920_1940.rda")
direct_1920_1940=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1910_1940.rda")
direct_1910_1940=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1900_1940.rda")
direct_1900_1940=direct_matches[which(is.na(direct_matches)==FALSE)]
direct_matches=append(direct_1900_1940, direct_1910_1940)
direct_matches=append(direct_matches, direct_1920_1940)
direct_matches=append(direct_matches, direct_1930_1940)
direct_matches=append(direct_matches, direct_1940_1940)
direct_matches=as.data.frame(do.call(rbind, as.matrix(direct_matches)))

matches_1940=rbind.data.frame(direct_matches, fuzzy_matches)
matches_1940=as.vector(matches_1940[,4])
matches_1940=unique(matches_1940)

save(matches_1940, file="intermediate_outputs/histid_1940_matches.rda")

#1930
load("intermediate_outputs/fuzzy_matches_1930_1930.rda")
fuzzy_1930_1930=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1920_1930.rda")
fuzzy_1920_1930=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1910_1930.rda")
fuzzy_1910_1930=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1900_1930.rda")
fuzzy_1900_1930=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
fuzzy_matches=append(fuzzy_1900_1930, fuzzy_1910_1930)
fuzzy_matches=append(fuzzy_matches, fuzzy_1920_1930)
fuzzy_matches=append(fuzzy_matches, fuzzy_1930_1930)
fuzzy_matches=as.data.frame(do.call(rbind, as.matrix(fuzzy_matches)))

load("intermediate_outputs/direct_matches_1930_1930.rda")
direct_1930_1930=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1920_1930.rda")
direct_1920_1930=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1910_1930.rda")
direct_1910_1930=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1900_1930.rda")
direct_1900_1930=direct_matches[which(is.na(direct_matches)==FALSE)]
direct_matches=append(direct_1900_1930, direct_1910_1930)
direct_matches=append(direct_matches, direct_1920_1930)
direct_matches=append(direct_matches, direct_1930_1930)
direct_matches=as.data.frame(do.call(rbind, as.matrix(direct_matches)))

matches_1930=rbind.data.frame(direct_matches, fuzzy_matches)
matches_1930=as.vector(matches_1930[,4])
matches_1930=unique(matches_1930)

save(matches_1930, file="intermediate_outputs/histid_1930_matches.rda")


#1920
load("intermediate_outputs/fuzzy_matches_1920_1920.rda")
fuzzy_1920_1920=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1910_1920.rda")
fuzzy_1910_1920=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1900_1920.rda")
fuzzy_1900_1920=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
fuzzy_matches=append(fuzzy_1900_1920, fuzzy_1910_1920)
fuzzy_matches=append(fuzzy_matches, fuzzy_1920_1920)
fuzzy_matches=as.data.frame(do.call(rbind, as.matrix(fuzzy_matches)))

load("intermediate_outputs/direct_matches_1920_1920.rda")
direct_1920_1920=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1910_1920.rda")
direct_1910_1920=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1900_1920.rda")
direct_1900_1920=direct_matches[which(is.na(direct_matches)==FALSE)]
direct_matches=append(direct_1900_1920, direct_1910_1920)
direct_matches=append(direct_matches, direct_1920_1920)
direct_matches=as.data.frame(do.call(rbind, as.matrix(direct_matches)))

matches_1920=rbind.data.frame(direct_matches, fuzzy_matches)
matches_1920=as.vector(matches_1920[,4])
matches_1920=unique(matches_1920)

save(matches_1920, file="intermediate_outputs/histid_1920_matches.rda")

#1910
load("intermediate_outputs/fuzzy_matches_1910_1910.rda")
fuzzy_1910_1910=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
load("intermediate_outputs/fuzzy_matches_1900_1910.rda")
fuzzy_1900_1910=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
fuzzy_matches=append(fuzzy_1900_1910, fuzzy_1910_1910)
fuzzy_matches=as.data.frame(do.call(rbind, as.matrix(fuzzy_matches)))

load("intermediate_outputs/direct_matches_1910_1910.rda")
direct_1910_1910=direct_matches[which(is.na(direct_matches)==FALSE)]
load("intermediate_outputs/direct_matches_1900_1910.rda")
direct_1900_1910=direct_matches[which(is.na(direct_matches)==FALSE)]
direct_matches=append(direct_1900_1910, direct_1910_1910)
direct_matches=as.data.frame(do.call(rbind, as.matrix(direct_matches)))

matches_1910=rbind.data.frame(direct_matches, fuzzy_matches)
matches_1910=as.vector(matches_1910[,4])
matches_1910=unique(matches_1910)

save(matches_1910, file="intermediate_outputs/histid_1910_matches.rda")

#1900
load("intermediate_outputs/fuzzy_matches_1900_1900.rda")
fuzzy_1900_1900=fuzzy_matches[which(is.na(fuzzy_matches)==FALSE)]
fuzzy_matches=fuzzy_1900_1900
fuzzy_matches=as.data.frame(do.call(rbind, as.matrix(fuzzy_matches)))

load("intermediate_outputs/direct_matches_1900_1900.rda")
direct_1900_1900=direct_matches[which(is.na(direct_matches)==FALSE)]
direct_matches=direct_1900_1900
direct_matches=as.data.frame(do.call(rbind, as.matrix(direct_matches)))

matches_1900=rbind.data.frame(direct_matches, fuzzy_matches)
matches_1900=as.vector(matches_1900[,4])
matches_1900=unique(matches_1900)

save(matches_1900, file="intermediate_outputs/histid_1900_matches.rda")