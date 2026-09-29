############################################################################################
#################   STEP 2A: CREATION OF HEXGRIDS FOR THE SAMPLE CITIES     ################
############################################################################################

#######################################################################
########### THIS CODE PROCEEDS CITY BY CITY      ######################
###########     LOADING ENUMERATION DISTRICT     ######################
###########     SHAPEFILES PROVIDED BY URBAN     ######################
###########     TRANSITION PROJECT AT BROWN      ######################
###########     UNIVERSITY                       ######################
#######################################################################

#######################################################################
########### THIS CODE ALSO GENERATES 1920        ######################
###########     INTERSECTION FILES. OTHER        ######################
###########     YEARS ARE GENERATED IN A         ######################
###########     LATER STAGE. 1920 IS A           ######################
###########     CONSTANT HEXGRID YEAR.           ######################
#######################################################################

#Chicago
library(sf)
Chicago_1920=st_read("input_data/step_2_enumeration_districts/1920/Illinois/Chicago")
Chicago_1920=st_make_valid(Chicago_1920)
Chicago_1920$area=as.numeric(st_area(Chicago_1920))

#repeated observations, keeping max size rep of ED
Chicago_1920=st_drop_geometry(Chicago_1920)
ind=vector(mode="character", length = length(unique(Chicago_1920[which(Chicago_1920$ED %in% (names(table(Chicago_1920$ED)[which(as.numeric(table(Chicago_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Chicago_1920[which(Chicago_1920$ED %in% (names(table(Chicago_1920$ED)[which(as.numeric(table(Chicago_1920$ED))>1)]))),"ED"]))){
  tmp=Chicago_1920[which(Chicago_1920$ED %in% unique(Chicago_1920[which(Chicago_1920$ED %in% (names(table(Chicago_1920$ED)[which(as.numeric(table(Chicago_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Chicago_1920=st_read("input_data/step_2_enumeration_districts/1920/Illinois/Chicago")
map_area=sum(st_area(Chicago_1920))
Chicago_1920=st_make_valid(Chicago_1920)
Chicago_1920$area=as.numeric(st_area(Chicago_1920))
Chicago_1920=Chicago_1920[which((row.names(Chicago_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Chicago_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Chicago_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Chicago_1920$geometry)
Chicago_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Chicago_hexgrid$hex_id=seq(1:nrow(Chicago_hexgrid))
hex_area=sum(st_area(Chicago_hexgrid))


#starting to intersect map and hex grid
Chicago_1920=st_make_valid(Chicago_1920)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
intersections_Chicago_1920=st_intersection(Chicago_1920, Chicago_hexgrid)
intersections_Chicago_1920$area_intersect=as.numeric(st_area(intersections_Chicago_1920))
Chicago_1920=st_drop_geometry(Chicago_1920)
intersections_Chicago_1920$ED_area=as.numeric(Chicago_1920[match(intersections_Chicago_1920$ED, Chicago_1920$ED), "area"])
intersections_Chicago_1920$proportion_intersected=intersections_Chicago_1920$area_intersect/intersections_Chicago_1920$ED_area

sum(intersections_Chicago_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Chicago_1920, file="intermediate_outputs/step_2_intersections/intersections_Chicago_1920.rda")
save(Chicago_hexgrid, file="intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")

#Baltimore
library(sf)
Baltimore_1920=st_read("input_data/step_2_enumeration_districts/1920/Maryland/Baltimore")
Baltimore_1920=st_make_valid(Baltimore_1920)
Baltimore_1920$area=as.numeric(st_area(Baltimore_1920))

#repeated observations, keeping max size rep of ED
Baltimore_1920=st_drop_geometry(Baltimore_1920)
ind=vector(mode="character", length = length(unique(Baltimore_1920[which(Baltimore_1920$ED %in% (names(table(Baltimore_1920$ED)[which(as.numeric(table(Baltimore_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Baltimore_1920[which(Baltimore_1920$ED %in% (names(table(Baltimore_1920$ED)[which(as.numeric(table(Baltimore_1920$ED))>1)]))),"ED"]))){
  tmp=Baltimore_1920[which(Baltimore_1920$ED %in% unique(Baltimore_1920[which(Baltimore_1920$ED %in% (names(table(Baltimore_1920$ED)[which(as.numeric(table(Baltimore_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Baltimore_1920=st_read("input_data/step_2_enumeration_districts/1920/Maryland/Baltimore")
map_area=sum(st_area(Baltimore_1920))
Baltimore_1920=st_make_valid(Baltimore_1920)
Baltimore_1920$area=as.numeric(st_area(Baltimore_1920))
Baltimore_1920=Baltimore_1920[which((row.names(Baltimore_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Baltimore_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Baltimore_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Baltimore_1920$geometry)
Baltimore_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Baltimore_hexgrid$hex_id=seq(1:nrow(Baltimore_hexgrid))
hex_area=sum(st_area(Baltimore_hexgrid))


#starting to intersect map and hex grid
Baltimore_1920=st_make_valid(Baltimore_1920)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
intersections_Baltimore_1920=st_intersection(Baltimore_1920, Baltimore_hexgrid)
intersections_Baltimore_1920$area_intersect=as.numeric(st_area(intersections_Baltimore_1920))
Baltimore_1920=st_drop_geometry(Baltimore_1920)
intersections_Baltimore_1920$ED_area=as.numeric(Baltimore_1920[match(intersections_Baltimore_1920$ED, Baltimore_1920$ED), "area"])
intersections_Baltimore_1920$proportion_intersected=intersections_Baltimore_1920$area_intersect/intersections_Baltimore_1920$ED_area

sum(intersections_Baltimore_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Baltimore_1920, file="intermediate_outputs/step_2_intersections/intersections_Baltimore_1920.rda")
save(Baltimore_hexgrid, file="intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")

#Boston
library(sf)
Boston_1920=st_read("input_data/step_2_enumeration_districts/1920/Massachusetts/Boston")
Boston_1920=st_make_valid(Boston_1920)
Boston_1920$area=as.numeric(st_area(Boston_1920))

#repeated observations, keeping max size rep of ED
Boston_1920=st_drop_geometry(Boston_1920)
ind=vector(mode="character", length = length(unique(Boston_1920[which(Boston_1920$ED %in% (names(table(Boston_1920$ED)[which(as.numeric(table(Boston_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Boston_1920[which(Boston_1920$ED %in% (names(table(Boston_1920$ED)[which(as.numeric(table(Boston_1920$ED))>1)]))),"ED"]))){
  tmp=Boston_1920[which(Boston_1920$ED %in% unique(Boston_1920[which(Boston_1920$ED %in% (names(table(Boston_1920$ED)[which(as.numeric(table(Boston_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Boston_1920=st_read("input_data/step_2_enumeration_districts/1920/Massachusetts/Boston")
map_area=sum(st_area(Boston_1920))
Boston_1920=st_make_valid(Boston_1920)
Boston_1920$area=as.numeric(st_area(Boston_1920))
Boston_1920=Boston_1920[which((row.names(Boston_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Boston_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Boston_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Boston_1920$geometry)
Boston_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Boston_hexgrid$hex_id=seq(1:nrow(Boston_hexgrid))
hex_area=sum(st_area(Boston_hexgrid))


#starting to intersect map and hex grid
Boston_1920=st_make_valid(Boston_1920)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
intersections_Boston_1920=st_intersection(Boston_1920, Boston_hexgrid)
intersections_Boston_1920$area_intersect=as.numeric(st_area(intersections_Boston_1920))
Boston_1920=st_drop_geometry(Boston_1920)
intersections_Boston_1920$ED_area=as.numeric(Boston_1920[match(intersections_Boston_1920$ED, Boston_1920$ED), "area"])
intersections_Boston_1920$proportion_intersected=intersections_Boston_1920$area_intersect/intersections_Boston_1920$ED_area

sum(intersections_Boston_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Boston_1920, file="intermediate_outputs/step_2_intersections/intersections_Boston_1920.rda")
save(Boston_hexgrid, file="intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")

#Cleveland
library(sf)
Cleveland_1920=st_read("input_data/step_2_enumeration_districts/1920/Ohio/Cleveland")
Cleveland_1920=st_make_valid(Cleveland_1920)
Cleveland_1920$area=as.numeric(st_area(Cleveland_1920))

#repeated observations, keeping max size rep of ED
Cleveland_1920=st_drop_geometry(Cleveland_1920)
ind=vector(mode="character", length = length(unique(Cleveland_1920[which(Cleveland_1920$ED %in% (names(table(Cleveland_1920$ED)[which(as.numeric(table(Cleveland_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cleveland_1920[which(Cleveland_1920$ED %in% (names(table(Cleveland_1920$ED)[which(as.numeric(table(Cleveland_1920$ED))>1)]))),"ED"]))){
  tmp=Cleveland_1920[which(Cleveland_1920$ED %in% unique(Cleveland_1920[which(Cleveland_1920$ED %in% (names(table(Cleveland_1920$ED)[which(as.numeric(table(Cleveland_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cleveland_1920=st_read("input_data/step_2_enumeration_districts/1920/Ohio/Cleveland")
map_area=sum(st_area(Cleveland_1920))
Cleveland_1920=st_make_valid(Cleveland_1920)
Cleveland_1920$area=as.numeric(st_area(Cleveland_1920))
Cleveland_1920=Cleveland_1920[which((row.names(Cleveland_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Cleveland_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Cleveland_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cleveland_1920$geometry)
Cleveland_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Cleveland_hexgrid$hex_id=seq(1:nrow(Cleveland_hexgrid))
hex_area=sum(st_area(Cleveland_hexgrid))


#starting to intersect map and hex grid
Cleveland_1920=st_make_valid(Cleveland_1920)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
intersections_Cleveland_1920=st_intersection(Cleveland_1920, Cleveland_hexgrid)
intersections_Cleveland_1920$area_intersect=as.numeric(st_area(intersections_Cleveland_1920))
Cleveland_1920=st_drop_geometry(Cleveland_1920)
intersections_Cleveland_1920$ED_area=as.numeric(Cleveland_1920[match(intersections_Cleveland_1920$ED, Cleveland_1920$ED), "area"])
intersections_Cleveland_1920$proportion_intersected=intersections_Cleveland_1920$area_intersect/intersections_Cleveland_1920$ED_area

sum(intersections_Cleveland_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cleveland_1920, file="intermediate_outputs/step_2_intersections/intersections_Cleveland_1920.rda")
save(Cleveland_hexgrid, file="intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")

#Cincinnati
library(sf)
Cincinnati_1920=st_read("input_data/step_2_enumeration_districts/1920/Ohio/Cincinnati")
Cincinnati_1920=st_make_valid(Cincinnati_1920)
Cincinnati_1920$area=as.numeric(st_area(Cincinnati_1920))

#repeated observations, keeping max size rep of ED
Cincinnati_1920=st_drop_geometry(Cincinnati_1920)
ind=vector(mode="character", length = length(unique(Cincinnati_1920[which(Cincinnati_1920$ED %in% (names(table(Cincinnati_1920$ED)[which(as.numeric(table(Cincinnati_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cincinnati_1920[which(Cincinnati_1920$ED %in% (names(table(Cincinnati_1920$ED)[which(as.numeric(table(Cincinnati_1920$ED))>1)]))),"ED"]))){
  tmp=Cincinnati_1920[which(Cincinnati_1920$ED %in% unique(Cincinnati_1920[which(Cincinnati_1920$ED %in% (names(table(Cincinnati_1920$ED)[which(as.numeric(table(Cincinnati_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cincinnati_1920=st_read("input_data/step_2_enumeration_districts/1920/Ohio/Cincinnati")
map_area=sum(st_area(Cincinnati_1920))
Cincinnati_1920=st_make_valid(Cincinnati_1920)
Cincinnati_1920$area=as.numeric(st_area(Cincinnati_1920))
Cincinnati_1920=Cincinnati_1920[which((row.names(Cincinnati_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Cincinnati_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Cincinnati_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cincinnati_1920$geometry)
Cincinnati_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Cincinnati_hexgrid$hex_id=seq(1:nrow(Cincinnati_hexgrid))
hex_area=sum(st_area(Cincinnati_hexgrid))


#starting to intersect map and hex grid
Cincinnati_1920=st_make_valid(Cincinnati_1920)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
intersections_Cincinnati_1920=st_intersection(Cincinnati_1920, Cincinnati_hexgrid)
intersections_Cincinnati_1920$area_intersect=as.numeric(st_area(intersections_Cincinnati_1920))
Cincinnati_1920=st_drop_geometry(Cincinnati_1920)
intersections_Cincinnati_1920$ED_area=as.numeric(Cincinnati_1920[match(intersections_Cincinnati_1920$ED, Cincinnati_1920$ED), "area"])
intersections_Cincinnati_1920$proportion_intersected=intersections_Cincinnati_1920$area_intersect/intersections_Cincinnati_1920$ED_area

sum(intersections_Cincinnati_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cincinnati_1920, file="intermediate_outputs/step_2_intersections/intersections_Cincinnati_1920.rda")
save(Cincinnati_hexgrid, file="intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")

#Brooklyn
library(sf)
Brooklyn_1920=st_read("input_data/step_2_enumeration_districts/1920/New York/Brooklyn")
Brooklyn_1920=st_make_valid(Brooklyn_1920)
Brooklyn_1920$area=as.numeric(st_area(Brooklyn_1920))

#repeated observations, keeping max size rep of ED
Brooklyn_1920=st_drop_geometry(Brooklyn_1920)
ind=vector(mode="character", length = length(unique(Brooklyn_1920[which(Brooklyn_1920$ED %in% (names(table(Brooklyn_1920$ED)[which(as.numeric(table(Brooklyn_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Brooklyn_1920[which(Brooklyn_1920$ED %in% (names(table(Brooklyn_1920$ED)[which(as.numeric(table(Brooklyn_1920$ED))>1)]))),"ED"]))){
  tmp=Brooklyn_1920[which(Brooklyn_1920$ED %in% unique(Brooklyn_1920[which(Brooklyn_1920$ED %in% (names(table(Brooklyn_1920$ED)[which(as.numeric(table(Brooklyn_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Brooklyn_1920=st_read("input_data/step_2_enumeration_districts/1920/New York/Brooklyn")
map_area=sum(st_area(Brooklyn_1920))
Brooklyn_1920=st_make_valid(Brooklyn_1920)
Brooklyn_1920$area=as.numeric(st_area(Brooklyn_1920))
Brooklyn_1920=Brooklyn_1920[which((row.names(Brooklyn_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Brooklyn_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Brooklyn_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Brooklyn_1920$geometry)
Brooklyn_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Brooklyn_hexgrid$hex_id=seq(1:nrow(Brooklyn_hexgrid))
hex_area=sum(st_area(Brooklyn_hexgrid))


#starting to intersect map and hex grid
Brooklyn_1920=st_make_valid(Brooklyn_1920)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
intersections_Brooklyn_1920=st_intersection(Brooklyn_1920, Brooklyn_hexgrid)
intersections_Brooklyn_1920$area_intersect=as.numeric(st_area(intersections_Brooklyn_1920))
Brooklyn_1920=st_drop_geometry(Brooklyn_1920)
intersections_Brooklyn_1920$ED_area=as.numeric(Brooklyn_1920[match(intersections_Brooklyn_1920$ED, Brooklyn_1920$ED), "area"])
intersections_Brooklyn_1920$proportion_intersected=intersections_Brooklyn_1920$area_intersect/intersections_Brooklyn_1920$ED_area

sum(intersections_Brooklyn_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Brooklyn_1920, file="intermediate_outputs/step_2_intersections/intersections_Brooklyn_1920.rda")
save(Brooklyn_hexgrid, file="intermediate_outputs/step_2_hexgrids/Brooklyn_hexgrid.rda")

#Manhattan
library(sf)
Manhattan_1920=st_read("input_data/step_2_enumeration_districts/1920/New York/Manhattan")
Manhattan_1920=st_make_valid(Manhattan_1920)
Manhattan_1920$area=as.numeric(st_area(Manhattan_1920))

#repeated observations, keeping max size rep of ED
Manhattan_1920=st_drop_geometry(Manhattan_1920)
ind=vector(mode="character", length = length(unique(Manhattan_1920[which(Manhattan_1920$ED %in% (names(table(Manhattan_1920$ED)[which(as.numeric(table(Manhattan_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Manhattan_1920[which(Manhattan_1920$ED %in% (names(table(Manhattan_1920$ED)[which(as.numeric(table(Manhattan_1920$ED))>1)]))),"ED"]))){
  tmp=Manhattan_1920[which(Manhattan_1920$ED %in% unique(Manhattan_1920[which(Manhattan_1920$ED %in% (names(table(Manhattan_1920$ED)[which(as.numeric(table(Manhattan_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Manhattan_1920=st_read("input_data/step_2_enumeration_districts/1920/New York/Manhattan")
map_area=sum(st_area(Manhattan_1920))
Manhattan_1920=st_make_valid(Manhattan_1920)
Manhattan_1920$area=as.numeric(st_area(Manhattan_1920))
Manhattan_1920=Manhattan_1920[which((row.names(Manhattan_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Manhattan_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Manhattan_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Manhattan_1920$geometry)
Manhattan_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Manhattan_hexgrid$hex_id=seq(1:nrow(Manhattan_hexgrid))
hex_area=sum(st_area(Manhattan_hexgrid))


#starting to intersect map and hex grid
Manhattan_1920=st_make_valid(Manhattan_1920)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
intersections_Manhattan_1920=st_intersection(Manhattan_1920, Manhattan_hexgrid)
intersections_Manhattan_1920$area_intersect=as.numeric(st_area(intersections_Manhattan_1920))
Manhattan_1920=st_drop_geometry(Manhattan_1920)
intersections_Manhattan_1920$ED_area=as.numeric(Manhattan_1920[match(intersections_Manhattan_1920$ED, Manhattan_1920$ED), "area"])
intersections_Manhattan_1920$proportion_intersected=intersections_Manhattan_1920$area_intersect/intersections_Manhattan_1920$ED_area

sum(intersections_Manhattan_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Manhattan_1920, file="intermediate_outputs/step_2_intersections/intersections_Manhattan_1920.rda")
save(Manhattan_hexgrid, file="intermediate_outputs/step_2_hexgrids/Manhattan_hexgrid.rda")

#Philadelphia
library(sf)
Philadelphia_1920=st_read("input_data/step_2_enumeration_districts/1920/Pennsylvania/Philadelphia")
Philadelphia_1920=st_make_valid(Philadelphia_1920)
Philadelphia_1920$area=as.numeric(st_area(Philadelphia_1920))

#repeated observations, keeping max size rep of ED
Philadelphia_1920=st_drop_geometry(Philadelphia_1920)
ind=vector(mode="character", length = length(unique(Philadelphia_1920[which(Philadelphia_1920$ED %in% (names(table(Philadelphia_1920$ED)[which(as.numeric(table(Philadelphia_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Philadelphia_1920[which(Philadelphia_1920$ED %in% (names(table(Philadelphia_1920$ED)[which(as.numeric(table(Philadelphia_1920$ED))>1)]))),"ED"]))){
  tmp=Philadelphia_1920[which(Philadelphia_1920$ED %in% unique(Philadelphia_1920[which(Philadelphia_1920$ED %in% (names(table(Philadelphia_1920$ED)[which(as.numeric(table(Philadelphia_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Philadelphia_1920=st_read("input_data/step_2_enumeration_districts/1920/Pennsylvania/Philadelphia")
map_area=sum(st_area(Philadelphia_1920))
Philadelphia_1920=st_make_valid(Philadelphia_1920)
Philadelphia_1920$area=as.numeric(st_area(Philadelphia_1920))
Philadelphia_1920=Philadelphia_1920[which((row.names(Philadelphia_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Philadelphia_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Philadelphia_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Philadelphia_1920$geometry)
Philadelphia_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Philadelphia_hexgrid$hex_id=seq(1:nrow(Philadelphia_hexgrid))
hex_area=sum(st_area(Philadelphia_hexgrid))


#starting to intersect map and hex grid
Philadelphia_1920=st_make_valid(Philadelphia_1920)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
intersections_Philadelphia_1920=st_intersection(Philadelphia_1920, Philadelphia_hexgrid)
intersections_Philadelphia_1920$area_intersect=as.numeric(st_area(intersections_Philadelphia_1920))
Philadelphia_1920=st_drop_geometry(Philadelphia_1920)
intersections_Philadelphia_1920$ED_area=as.numeric(Philadelphia_1920[match(intersections_Philadelphia_1920$ED, Philadelphia_1920$ED), "area"])
intersections_Philadelphia_1920$proportion_intersected=intersections_Philadelphia_1920$area_intersect/intersections_Philadelphia_1920$ED_area

sum(intersections_Philadelphia_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Philadelphia_1920, file="intermediate_outputs/step_2_intersections/intersections_Philadelphia_1920.rda")
save(Philadelphia_hexgrid, file="intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")

#Pittsburgh
library(sf)
Pittsburgh_1920=st_read("input_data/step_2_enumeration_districts/1920/Pennsylvania/Pittsburgh")
Pittsburgh_1920=st_make_valid(Pittsburgh_1920)
Pittsburgh_1920$area=as.numeric(st_area(Pittsburgh_1920))

#repeated observations, keeping max size rep of ED
Pittsburgh_1920=st_drop_geometry(Pittsburgh_1920)
ind=vector(mode="character", length = length(unique(Pittsburgh_1920[which(Pittsburgh_1920$ED %in% (names(table(Pittsburgh_1920$ED)[which(as.numeric(table(Pittsburgh_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Pittsburgh_1920[which(Pittsburgh_1920$ED %in% (names(table(Pittsburgh_1920$ED)[which(as.numeric(table(Pittsburgh_1920$ED))>1)]))),"ED"]))){
  tmp=Pittsburgh_1920[which(Pittsburgh_1920$ED %in% unique(Pittsburgh_1920[which(Pittsburgh_1920$ED %in% (names(table(Pittsburgh_1920$ED)[which(as.numeric(table(Pittsburgh_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Pittsburgh_1920=st_read("input_data/step_2_enumeration_districts/1920/Pennsylvania/Pittsburgh")
map_area=sum(st_area(Pittsburgh_1920))
Pittsburgh_1920=st_make_valid(Pittsburgh_1920)
Pittsburgh_1920$area=as.numeric(st_area(Pittsburgh_1920))
Pittsburgh_1920=Pittsburgh_1920[which((row.names(Pittsburgh_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Pittsburgh_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Pittsburgh_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Pittsburgh_1920$geometry)
Pittsburgh_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Pittsburgh_hexgrid$hex_id=seq(1:nrow(Pittsburgh_hexgrid))
hex_area=sum(st_area(Pittsburgh_hexgrid))


#starting to intersect map and hex grid
Pittsburgh_1920=st_make_valid(Pittsburgh_1920)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
intersections_Pittsburgh_1920=st_intersection(Pittsburgh_1920, Pittsburgh_hexgrid)
intersections_Pittsburgh_1920$area_intersect=as.numeric(st_area(intersections_Pittsburgh_1920))
Pittsburgh_1920=st_drop_geometry(Pittsburgh_1920)
intersections_Pittsburgh_1920$ED_area=as.numeric(Pittsburgh_1920[match(intersections_Pittsburgh_1920$ED, Pittsburgh_1920$ED), "area"])
intersections_Pittsburgh_1920$proportion_intersected=intersections_Pittsburgh_1920$area_intersect/intersections_Pittsburgh_1920$ED_area

sum(intersections_Pittsburgh_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Pittsburgh_1920, file="intermediate_outputs/step_2_intersections/intersections_Pittsburgh_1920.rda")
save(Pittsburgh_hexgrid, file="intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")

#StLouis
library(sf)
StLouis_1920=st_read("input_data/step_2_enumeration_districts/1920/Missouri/St. Louis")
StLouis_1920=st_make_valid(StLouis_1920)
StLouis_1920$area=as.numeric(st_area(StLouis_1920))

#repeated observations, keeping max size rep of ED
StLouis_1920=st_drop_geometry(StLouis_1920)
ind=vector(mode="character", length = length(unique(StLouis_1920[which(StLouis_1920$ED %in% (names(table(StLouis_1920$ED)[which(as.numeric(table(StLouis_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(StLouis_1920[which(StLouis_1920$ED %in% (names(table(StLouis_1920$ED)[which(as.numeric(table(StLouis_1920$ED))>1)]))),"ED"]))){
  tmp=StLouis_1920[which(StLouis_1920$ED %in% unique(StLouis_1920[which(StLouis_1920$ED %in% (names(table(StLouis_1920$ED)[which(as.numeric(table(StLouis_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
StLouis_1920=st_read("input_data/step_2_enumeration_districts/1920/Missouri/St. Louis")
map_area=sum(st_area(StLouis_1920))
StLouis_1920=st_make_valid(StLouis_1920)
StLouis_1920$area=as.numeric(st_area(StLouis_1920))
StLouis_1920=StLouis_1920[which((row.names(StLouis_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(StLouis_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(StLouis_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, StLouis_1920$geometry)
StLouis_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
StLouis_hexgrid$hex_id=seq(1:nrow(StLouis_hexgrid))
hex_area=sum(st_area(StLouis_hexgrid))


#starting to intersect map and hex grid
StLouis_1920=st_make_valid(StLouis_1920)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
intersections_StLouis_1920=st_intersection(StLouis_1920, StLouis_hexgrid)
intersections_StLouis_1920$area_intersect=as.numeric(st_area(intersections_StLouis_1920))
StLouis_1920=st_drop_geometry(StLouis_1920)
intersections_StLouis_1920$ED_area=as.numeric(StLouis_1920[match(intersections_StLouis_1920$ED, StLouis_1920$ED), "area"])
intersections_StLouis_1920$proportion_intersected=intersections_StLouis_1920$area_intersect/intersections_StLouis_1920$ED_area

sum(intersections_StLouis_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_StLouis_1920, file="intermediate_outputs/step_2_intersections/intersections_StLouis_1920.rda")
save(StLouis_hexgrid, file="intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")

#Detroit
library(sf)
Detroit_1920=st_read("input_data/step_2_enumeration_districts/1920/Michigan/Detroit")
StLouis_1920=st_read("input_data/step_2_enumeration_districts/1920/Missouri/St. Louis") ### CORRECTING CRS ISSUE IN RAW DATA
Detroit_1920=st_transform(Detroit_1920, crs=st_crs(StLouis_1920))
Detroit_1920=st_make_valid(Detroit_1920)
Detroit_1920$area=as.numeric(st_area(Detroit_1920))

#repeated observations, keeping max size rep of ED
Detroit_1920=st_drop_geometry(Detroit_1920)
ind=vector(mode="character", length = length(unique(Detroit_1920[which(Detroit_1920$ED %in% (names(table(Detroit_1920$ED)[which(as.numeric(table(Detroit_1920$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Detroit_1920[which(Detroit_1920$ED %in% (names(table(Detroit_1920$ED)[which(as.numeric(table(Detroit_1920$ED))>1)]))),"ED"]))){
  tmp=Detroit_1920[which(Detroit_1920$ED %in% unique(Detroit_1920[which(Detroit_1920$ED %in% (names(table(Detroit_1920$ED)[which(as.numeric(table(Detroit_1920$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Detroit_1920=st_read("input_data/step_2_enumeration_districts/1920/Michigan/Detroit")
Detroit_1920=st_transform(Detroit_1920, crs=st_crs(StLouis_1920))
Detroit_1920=st_make_valid(Detroit_1920)
map_area=sum(st_area(Detroit_1920))
Detroit_1920=st_make_valid(Detroit_1920)
Detroit_1920$area=as.numeric(st_area(Detroit_1920))
Detroit_1920=Detroit_1920[which((row.names(Detroit_1920) %in% ind)==FALSE),]

#area of all ed polygons in map
areas_ed=st_area(Detroit_1920)
mean(areas_ed)
median(areas_ed)

#or can do 800 as in the paper
cellsize=800

hex_grid <- st_make_grid(Detroit_1920, cellsize = cellsize, square = FALSE, what = "polygons") #cellsize is radius of hexagon
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1920$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
Detroit_hexgrid$hex_id=seq(1:nrow(Detroit_hexgrid))
hex_area=sum(st_area(Detroit_hexgrid))


#starting to intersect map and hex grid
Detroit_1920=st_make_valid(Detroit_1920)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
intersections_Detroit_1920=st_intersection(Detroit_1920, Detroit_hexgrid)
intersections_Detroit_1920$area_intersect=as.numeric(st_area(intersections_Detroit_1920))
Detroit_1920=st_drop_geometry(Detroit_1920)
intersections_Detroit_1920$ED_area=as.numeric(Detroit_1920[match(intersections_Detroit_1920$ED, Detroit_1920$ED), "area"])
intersections_Detroit_1920$proportion_intersected=intersections_Detroit_1920$area_intersect/intersections_Detroit_1920$ED_area

sum(intersections_Detroit_1920$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Detroit_1920, file="intermediate_outputs/step_2_intersections/intersections_Detroit_1920.rda")
save(Detroit_hexgrid, file="intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")