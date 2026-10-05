load("analysis_data/combined_df.rda")

library(fixest)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667
mafia_book_geo=unique(combined_df[which(combined_df$mafia_book_geo==1), "city_hex"]) #hexagons containing geocoded locations of 1959 mafia actiivty- 265
mafia_book_hex=unique(combined_df[which(combined_df$mafia_book_hex==1), "city_hex"]) #hexagons containing 1900-1920 census of locations of future 1959 mafia members- 149

####something to note: feols(fixed_mori_set~mafia_pre_geo|city, data=combined_df[which(combined_df$year==1920),])


#FIGURE 5 PANEL B- PROPORTION OF FUTURE 1959 MAFIA ACTIVITY OCCURING IN 1920-FIXED MORI AREAS
length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_geo %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.70),freq=FALSE, main="Samples Drawn from All Cities", xlab = "Proportion of Known Mafia Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)

#FIGURE 5 PANEL A- PROPORTION OF FUTURE 1959 MAFIA MEMBERS' 1900-1920 CENSUS LOCATIONS OCCURING IN 1920-FIXED MORI AREAS
length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_hex %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 10, xlim=c(0.00, 0.85), freq=FALSE, main="Samples Drawn from All Cities", xlab = "Proportion of Known Mafia in Census in Sample")
abline(v=value, col="red", lwd=2, lty=2)



#TABLE 3 (CONLEY STANDARD ERRORS COMPUTED SEPERATELY)
feols(mafia_book_hex~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),])
feols(mafia_book_hex~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),])
feols(mafia_book_hex~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians+mafia_pre_geo|city, data=combined_df[which(combined_df$year==1920),])

feols(mafia_book_geo~fixed_mori_set|city, data=combined_df[which(combined_df$year==1920),])
feols(mafia_book_geo~fixed_mori_set+any_sicilians|city, data=combined_df[which(combined_df$year==1920),])
feols(mafia_book_geo~fixed_mori_set+any_sicilians+any_cutrera_or_damiani_maps_sicilians+mafia_pre_geo|city, data=combined_df[which(combined_df$year==1920),])


#FIGURE 7
coefplot(feols(incarc_cs_per_10k~i(year,fixed_mori_set, ref=1920)+any_sicilians+ital_prop|city_hex+year, data=combined_df, cluster=~city_hex),keep="year::")
coefplot(feols(incarc_cs_per_10k~i(year,fixed_mori_set, ref=1920)+any_sicilians+ital_prop+any_cutrera_or_damiani_maps_sicilians|city_hex+year, data=combined_df, cluster=~city_hex),keep="year::")
coefplot(feols(incarc_cs_per_10k~i(year,fixed_mori_set, ref=1920)+any_sicilians+ital_prop+any_cutrera_or_damiani_maps_sicilians+mafia_pre_geo|city_hex+year, data=combined_df, cluster=~city_hex),keep="year::")

#FIGURE 9
coefplot(feols(owned~i(year,fixed_mori_set, ref=1920)+any_sicilians+ital_prop|city_hex+year, data=combined_df, cluster=~city_hex),keep="year::")
coefplot(feols(owned~i(year,fixed_mori_set, ref=1920)+any_sicilians+ital_prop|city_hex+year, data=combined_df[which(combined_df$year>1900),], cluster=~city_hex),keep="year::")



#RELEVANT REPLICATIONS OF FIGURE 5 WITH ALTERNATIVE EXPLANATION RESITRCITIONS PANEL B- PROPORTION OF FUTURE 1959 MAFIA ACTIVITY OCCURING IN 1920-FIXED MORI AREAS
    #RESTRICTIONS BASED ON PRE-1926 ORGANIZED CRIME ACTIVITY- GEOCODED LOCATIONS- REMOVE OVERLAP OF MORI AND PRE-1926 GEO FROM "MORI" POOL
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$mafia_pre_geo==0)])
length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_geo %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.60),freq=FALSE, main="Samples Drawn from All Cities, Mori Excludes Pre-1926 Italian Organized Crime Locations", xlab = "Proportion of Known 1959 Mafia Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667


  #RESTRICTIONS BASED ON PRE-1926 ORGANIZED CRIME MEMBERS CENSUS LOCATIONS- CENSUS LOCATIONS OF FUTURE MAFIOSI- REMOVE OVERLAP OF MORI AND PRE-1926 GEO FROM "MORI" POOL
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$mafia_pre_hex==0)])
length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_hex %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.90),freq=FALSE, main="Samples Drawn from All Cities, Mori Excludes Pre-1926 Italian Organized Crime Census Locations", xlab = "Proportion of 1959 Mafia Member pre-1920 Census Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667

  #RESTRICTIONS BASED ON LOCATIONS OF CUTRERA/DAMIANI IMMIGRANTS- GEOCODED LOCATIONS OF FUTURE MAFIA ACTIIVTY- REMOVE OVERLAP OF MORI AND OTHER MAPS GEO FROM "MORI" POOL
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$any_cutrera_or_damiani_maps_sicilians==0)]) 
length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_geo %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.50),freq=FALSE, main="Samples Drawn from All Cities, Mori Excludes Enclave of Other Mafia Commune", xlab = "Proportion of Known 1959 Mafia Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667

  #RESTRICTIONS BASED ON LOCATIONS OF CUTRERA/DAMIANI IMMIGRANTS- CENSUS LOCAITONS OF FUTURE MAFIOSI- REMOVE OVERLAP OF MORI AND OTHER MAPS GEO FROM "MORI" POOL
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$any_cutrera_or_damiani_maps_sicilians==0)]) #fixed mori set (1900-1920) locations- 667
length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_hex %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.50),freq=FALSE, main="Samples Drawn from All Cities, Mori Excludes Enclaves of Other Mafia Commune", xlab = "Proportion of 1959 Mafia Member pre-1920 Census Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667

  #RESTRICTIONS BASED ON LOCATIONS OF CUTRERA/DAMIANI IMMIGRANTS- GEOCODED LOCATIONS OF FUTURE MAFIA ACTIIVTY- REMOVE OVERLAP OF MORI AND OTHER MAPS GEO FROM "MORI" POOL AND RESTRICT PLACEBO DONOR POOL TO ONLY OTHER MAP AREAS
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$any_cutrera_or_damiani_maps_sicilians==0)])
length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920 & combined_df$any_cutrera_or_damiani_maps_sicilians==1),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_geo %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_geo[which((mafia_book_geo %in% mori)==TRUE)])/length(mafia_book_geo)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.50),freq=FALSE, main="Samples Drawn from All Cities, Limited to Enclaves of Other Mafia Commune Which Mori Excludes", xlab = "Proportion of 1959 Known Mafia Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667


  #RESTRICTIONS BASED ON LOCATIONS OF CUTRERA/DAMIANI IMMIGRANTS- CENSUS LOCAITONS OF FUTURE MAFIOSI- REMOVE OVERLAP OF MORI AND OTHER MAPS GEO FROM "MORI" POOL AND RESTRICT PLACEBO DONOR POOL TO ONLY OTHER MAP AREAS
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1 & combined_df$any_cutrera_or_damiani_maps_sicilians==0)])
length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
bs_vec=vector(mode="numeric", length=1000)
for (i in 1:length(bs_vec)){
  bs_sample=sample(unique(combined_df[which(combined_df$year==1920 & combined_df$any_cutrera_or_damiani_maps_sicilians==1),"city_hex"]), size=length(mori), replace = FALSE)
  bs_vec[i]=length(bs_sample[which((mafia_book_hex %in% bs_sample)==TRUE)])/length(bs_sample)
}
value=length(mafia_book_hex[which((mafia_book_hex %in% mori)==TRUE)])/length(mafia_book_hex)
2*(1-pnorm(abs((value-mean(bs_vec))/sd(bs_vec))))
hist(bs_vec, breaks = 20, xlim=c(0.00, 0.50),freq=FALSE, main="Samples Drawn from All Cities, Limited to Enclaves of Other Mafia Commune Which Mori Excludes", xlab = "Proportion of Known Mafia Locations in Sample")
abline(v=value, col="red", lwd=2, lty=2)
mori=unique(combined_df$city_hex[which(combined_df$fixed_mori_set==1)]) #fixed mori set (1900-1920) locations- 667
