############################################################################################################
###################### ANALYSIS SCRIPT 4: THIS SCRIPT REPLICATES CHICAGO HOMICIDE RESULTS ##################
######################                    FIGURES AND TABLES FOR ALL VIOLENCE RELATED     ##################
######################                    OUTCOMES                                       ###################
############################################################################################################

library(synthdid)
library(fixest)



load("analysis_data/clean_geocoded_chicago_murders_with_neighbors.rda")

##########################################################################
####################### SYNTH DID PLOTS ##################################
##########################################################################

#Full time frame neighbors
setup=panel.matrices(
  mat[which(mat$fixed_mori_set==0),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 25,
  treated.last = TRUE
)
tau.hat_neighbors = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='jackknife'))
print(se_neighbors)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='placebo'))
print(se_neighbors)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='bootstrap'))
print(se_neighbors)
plot(tau.hat_neighbors, overlay=1)
weights=synthdid_controls(tau.hat_neighbors, sort.by = 1, mass = 0.9, weight.type = "omega")
mat$weights=weights[match(mat$hex_id, rownames(weights))]
mat$weights=ifelse(mat$mori_neighbor==1, 1, mat$weights)
mat$weights=ifelse(is.na(mat$weights)==TRUE, 0, mat$weights)

#reuduced time frame mori 1900-1930
setup=panel.matrices(
  mat[which(mat$mori_neighbor==0 & mat$year>1899 & mat$year<1931),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 17,
  treated.last = TRUE
)
tau.hat= synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='jackknife'))
print(se)
se = sqrt(vcov(tau.hat, method='placebo'))
print(se)
se = sqrt(vcov(tau.hat, method='bootstrap'))
print(se)
plot(tau.hat, overlay=1)

#reduced time frame neighbors 1900-1930
setup=panel.matrices(
  mat[which(mat$fixed_mori_set==0 & mat$year>1899 & mat$year<1931),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 25,
  treated.last = TRUE
)
tau.hat_neighbors = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='jackknife'))
print(se_neighbors)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='placebo'))
print(se_neighbors)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='bootstrap'))
plot(tau.hat_neighbors, overlay=1)

#full time frame mori
setup=panel.matrices(
  mat[which(mat$mori_neighbor==0),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 17,
  treated.last = TRUE
)
tau.hat= synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='jackknife'))
print(se)
se = sqrt(vcov(tau.hat, method='placebo'))
print(se)
se = sqrt(vcov(tau.hat, method='bootstrap'))
print(se)
plot(tau.hat, overlay=1)
weights2=synthdid_controls(tau.hat, sort.by = 1, mass = 0.9, weight.type = "omega")
mat$weights2=weights2[match(mat$hex_id, rownames(weights2))]
mat$weights2=ifelse(mat$any_mori_2==1, 1, mat$weights2)
mat$weights2=ifelse(is.na(mat$weights2)==TRUE, 0, mat$weights2)

#reduced time frame neighbors- 1920-1930
setup=panel.matrices(
  mat[which(mat$fixed_mori_set==0& mat$year>1919 & mat$year<1931),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 25,
  treated.last = TRUE
)
tau.hat_neighbors = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='jackknife'))
print(se_neighbors)
se_neighbors= sqrt(vcov(tau.hat_neighbors, method='placebo'))
print(se_neighbors)
se_neighbors = sqrt(vcov(tau.hat_neighbors, method='bootstrap'))
print(se_neighbors)
plot(tau.hat_neighbors, overlay=1)

#reduced time frame mori 1920-1930
setup=panel.matrices(
  mat[which(mat$mori_neighbor==0 & mat$year>1919 & mat$year<1931),],
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 17,
  treated.last = TRUE
)
tau.hat= synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='jackknife'))
print(se)
se = sqrt(vcov(tau.hat, method='placebo'))
print(se)
se= sqrt(vcov(tau.hat, method='bootstrap'))
print(se)
plot(tau.hat, overlay=1)



##########################################################################
#####################  TWFE REGRESSIONS ##################################
##########################################################################
feols(total_homicides~neighbors_post_raid|hex_id+year, data=mat[which(mat$any_mori_2==0 & mat$year>=1919),], weights = ~weights) #level 1 and level 2 neighors
feols(total_homicides~neighbors_post_raid|hex_id+year, data=mat[which(mat$fixed_mori_set==0  & mat$year>=1919),]) #level 1 and level 2 neighors
feols(total_homicides~post_treat_raids|hex_id+year, data=mat[which(mat$mori_neighbor==0 & mat$year>=1919),], weights=~weights2)
feols(total_homicides~post_treat_raids|hex_id+year, data=mat[which(mat$mori_neighbor==0 & mat$year>=1919),])

