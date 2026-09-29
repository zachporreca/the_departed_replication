############################################################################################
##########   STEP 2B: CREATING INTERSECTION OBJECTS FOR 1900-1940 (EX 1920)     ############
############################################################################################


#1930
#Chicago
library(sf)
Chicago_1930=st_read("input_data/step_2_enumeration_districts/1930/Illinois/Chicago")
Chicago_1930=st_make_valid(Chicago_1930)
Chicago_1930$area=as.numeric(st_area(Chicago_1930))

#repeated observations, keeping max size rep of ED
Chicago_1930=st_drop_geometry(Chicago_1930)
ind=vector(mode="character", length = length(unique(Chicago_1930[which(Chicago_1930$ED %in% (names(table(Chicago_1930$ED)[which(as.numeric(table(Chicago_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Chicago_1930[which(Chicago_1930$ED %in% (names(table(Chicago_1930$ED)[which(as.numeric(table(Chicago_1930$ED))>1)]))),"ED"]))){
  tmp=Chicago_1930[which(Chicago_1930$ED %in% unique(Chicago_1930[which(Chicago_1930$ED %in% (names(table(Chicago_1930$ED)[which(as.numeric(table(Chicago_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Chicago_1930=st_read("input_data/step_2_enumeration_districts/1930/Illinois/Chicago")
map_area=sum(st_area(Chicago_1930))
Chicago_1930=st_make_valid(Chicago_1930)
Chicago_1930$area=as.numeric(st_area(Chicago_1930))
Chicago_1930=Chicago_1930[which((row.names(Chicago_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
hex_grid=Chicago_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Chicago_1930$geometry)
Chicago_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Chicago_hexgrid))


#starting to intersect map and hex grid
Chicago_1930=st_make_valid(Chicago_1930)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
intersections_Chicago_1930=st_intersection(Chicago_1930, Chicago_hexgrid)
intersections_Chicago_1930$area_intersect=as.numeric(st_area(intersections_Chicago_1930))
Chicago_1930=st_drop_geometry(Chicago_1930)
intersections_Chicago_1930$ED_area=as.numeric(Chicago_1930[match(intersections_Chicago_1930$ED, Chicago_1930$ED), "area"])
intersections_Chicago_1930$proportion_intersected=intersections_Chicago_1930$area_intersect/intersections_Chicago_1930$ED_area

sum(intersections_Chicago_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Chicago_1930, file="intermediate_outputs/step_2_intersections/intersections_Chicago_1930.rda")


#Baltimore
library(sf)
Baltimore_1930=st_read("input_data/step_2_enumeration_districts/1930/Maryland/Baltimore")
Baltimore_1930=st_make_valid(Baltimore_1930)
Baltimore_1930$area=as.numeric(st_area(Baltimore_1930))

#repeated observations, keeping max size rep of ED
Baltimore_1930=st_drop_geometry(Baltimore_1930)
ind=vector(mode="character", length = length(unique(Baltimore_1930[which(Baltimore_1930$ED %in% (names(table(Baltimore_1930$ED)[which(as.numeric(table(Baltimore_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Baltimore_1930[which(Baltimore_1930$ED %in% (names(table(Baltimore_1930$ED)[which(as.numeric(table(Baltimore_1930$ED))>1)]))),"ED"]))){
  tmp=Baltimore_1930[which(Baltimore_1930$ED %in% unique(Baltimore_1930[which(Baltimore_1930$ED %in% (names(table(Baltimore_1930$ED)[which(as.numeric(table(Baltimore_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Baltimore_1930=st_read("input_data/step_2_enumeration_districts/1930/Maryland/Baltimore")
map_area=sum(st_area(Baltimore_1930))
Baltimore_1930=st_make_valid(Baltimore_1930)
Baltimore_1930$area=as.numeric(st_area(Baltimore_1930))
Baltimore_1930=Baltimore_1930[which((row.names(Baltimore_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")
hex_grid=Baltimore_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Baltimore_1930$geometry)
Baltimore_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Baltimore_hexgrid))


#starting to intersect map and hex grid
Baltimore_1930=st_make_valid(Baltimore_1930)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
intersections_Baltimore_1930=st_intersection(Baltimore_1930, Baltimore_hexgrid)
intersections_Baltimore_1930$area_intersect=as.numeric(st_area(intersections_Baltimore_1930))
Baltimore_1930=st_drop_geometry(Baltimore_1930)
intersections_Baltimore_1930$ED_area=as.numeric(Baltimore_1930[match(intersections_Baltimore_1930$ED, Baltimore_1930$ED), "area"])
intersections_Baltimore_1930$proportion_intersected=intersections_Baltimore_1930$area_intersect/intersections_Baltimore_1930$ED_area

sum(intersections_Baltimore_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Baltimore_1930, file="intermediate_outputs/step_2_intersections/intersections_Baltimore_1930.rda")


#Boston
library(sf)
Boston_1930=st_read("input_data/step_2_enumeration_districts/1930/Massachusetts/Boston")
Boston_1930=st_make_valid(Boston_1930)
Boston_1930$area=as.numeric(st_area(Boston_1930))

#repeated observations, keeping max size rep of ED
Boston_1930=st_drop_geometry(Boston_1930)
ind=vector(mode="character", length = length(unique(Boston_1930[which(Boston_1930$ED %in% (names(table(Boston_1930$ED)[which(as.numeric(table(Boston_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Boston_1930[which(Boston_1930$ED %in% (names(table(Boston_1930$ED)[which(as.numeric(table(Boston_1930$ED))>1)]))),"ED"]))){
  tmp=Boston_1930[which(Boston_1930$ED %in% unique(Boston_1930[which(Boston_1930$ED %in% (names(table(Boston_1930$ED)[which(as.numeric(table(Boston_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Boston_1930=st_read("input_data/step_2_enumeration_districts/1930/Massachusetts/Boston")
map_area=sum(st_area(Boston_1930))
Boston_1930=st_make_valid(Boston_1930)
Boston_1930$area=as.numeric(st_area(Boston_1930))
Boston_1930=Boston_1930[which((row.names(Boston_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")
hex_grid=Boston_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Boston_1930$geometry)
Boston_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Boston_hexgrid))


#starting to intersect map and hex grid
Boston_1930=st_make_valid(Boston_1930)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
intersections_Boston_1930=st_intersection(Boston_1930, Boston_hexgrid)
intersections_Boston_1930$area_intersect=as.numeric(st_area(intersections_Boston_1930))
Boston_1930=st_drop_geometry(Boston_1930)
intersections_Boston_1930$ED_area=as.numeric(Boston_1930[match(intersections_Boston_1930$ED, Boston_1930$ED), "area"])
intersections_Boston_1930$proportion_intersected=intersections_Boston_1930$area_intersect/intersections_Boston_1930$ED_area

sum(intersections_Boston_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Boston_1930, file="intermediate_outputs/step_2_intersections/intersections_Boston_1930.rda")


#Cleveland
library(sf)
Cleveland_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cleveland")
Cleveland_1930=st_make_valid(Cleveland_1930)
Cleveland_1930$area=as.numeric(st_area(Cleveland_1930))

#repeated observations, keeping max size rep of ED
Cleveland_1930=st_drop_geometry(Cleveland_1930)
ind=vector(mode="character", length = length(unique(Cleveland_1930[which(Cleveland_1930$ED %in% (names(table(Cleveland_1930$ED)[which(as.numeric(table(Cleveland_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cleveland_1930[which(Cleveland_1930$ED %in% (names(table(Cleveland_1930$ED)[which(as.numeric(table(Cleveland_1930$ED))>1)]))),"ED"]))){
  tmp=Cleveland_1930[which(Cleveland_1930$ED %in% unique(Cleveland_1930[which(Cleveland_1930$ED %in% (names(table(Cleveland_1930$ED)[which(as.numeric(table(Cleveland_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cleveland_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cleveland")
map_area=sum(st_area(Cleveland_1930))
Cleveland_1930=st_make_valid(Cleveland_1930)
Cleveland_1930$area=as.numeric(st_area(Cleveland_1930))
Cleveland_1930=Cleveland_1930[which((row.names(Cleveland_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")
hex_grid=Cleveland_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cleveland_1930$geometry)
Cleveland_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cleveland_hexgrid))


#starting to intersect map and hex grid
Cleveland_1930=st_make_valid(Cleveland_1930)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
intersections_Cleveland_1930=st_intersection(Cleveland_1930, Cleveland_hexgrid)
intersections_Cleveland_1930$area_intersect=as.numeric(st_area(intersections_Cleveland_1930))
Cleveland_1930=st_drop_geometry(Cleveland_1930)
intersections_Cleveland_1930$ED_area=as.numeric(Cleveland_1930[match(intersections_Cleveland_1930$ED, Cleveland_1930$ED), "area"])
intersections_Cleveland_1930$proportion_intersected=intersections_Cleveland_1930$area_intersect/intersections_Cleveland_1930$ED_area

sum(intersections_Cleveland_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cleveland_1930, file="intermediate_outputs/step_2_intersections/intersections_Cleveland_1930.rda")


#Cincinnati
library(sf)
Cincinnati_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cincinnati")
Cincinnati_1930=st_make_valid(Cincinnati_1930)
Cincinnati_1930$area=as.numeric(st_area(Cincinnati_1930))

#repeated observations, keeping max size rep of ED
Cincinnati_1930=st_drop_geometry(Cincinnati_1930)
ind=vector(mode="character", length = length(unique(Cincinnati_1930[which(Cincinnati_1930$ED %in% (names(table(Cincinnati_1930$ED)[which(as.numeric(table(Cincinnati_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cincinnati_1930[which(Cincinnati_1930$ED %in% (names(table(Cincinnati_1930$ED)[which(as.numeric(table(Cincinnati_1930$ED))>1)]))),"ED"]))){
  tmp=Cincinnati_1930[which(Cincinnati_1930$ED %in% unique(Cincinnati_1930[which(Cincinnati_1930$ED %in% (names(table(Cincinnati_1930$ED)[which(as.numeric(table(Cincinnati_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cincinnati_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cincinnati")
map_area=sum(st_area(Cincinnati_1930))
Cincinnati_1930=st_make_valid(Cincinnati_1930)
Cincinnati_1930$area=as.numeric(st_area(Cincinnati_1930))
Cincinnati_1930=Cincinnati_1930[which((row.names(Cincinnati_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")
hex_grid=Cincinnati_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cincinnati_1930$geometry)
Cincinnati_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cincinnati_hexgrid))


#starting to intersect map and hex grid
Cincinnati_1930=st_make_valid(Cincinnati_1930)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
intersections_Cincinnati_1930=st_intersection(Cincinnati_1930, Cincinnati_hexgrid)
intersections_Cincinnati_1930$area_intersect=as.numeric(st_area(intersections_Cincinnati_1930))
Cincinnati_1930=st_drop_geometry(Cincinnati_1930)
intersections_Cincinnati_1930$ED_area=as.numeric(Cincinnati_1930[match(intersections_Cincinnati_1930$ED, Cincinnati_1930$ED), "area"])
intersections_Cincinnati_1930$proportion_intersected=intersections_Cincinnati_1930$area_intersect/intersections_Cincinnati_1930$ED_area

sum(intersections_Cincinnati_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cincinnati_1930, file="intermediate_outputs/step_2_intersections/intersections_Cincinnati_1930.rda")


#Brooklyn
library(sf)
Brooklyn_1930=st_read("input_data/step_2_enumeration_districts/1930/New York/Brooklyn")
Brooklyn_1930=st_make_valid(Brooklyn_1930)
Brooklyn_1930$area=as.numeric(st_area(Brooklyn_1930))

#repeated observations, keeping max size rep of ED
Brooklyn_1930=st_drop_geometry(Brooklyn_1930)
ind=vector(mode="character", length = length(unique(Brooklyn_1930[which(Brooklyn_1930$ED %in% (names(table(Brooklyn_1930$ED)[which(as.numeric(table(Brooklyn_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Brooklyn_1930[which(Brooklyn_1930$ED %in% (names(table(Brooklyn_1930$ED)[which(as.numeric(table(Brooklyn_1930$ED))>1)]))),"ED"]))){
  tmp=Brooklyn_1930[which(Brooklyn_1930$ED %in% unique(Brooklyn_1930[which(Brooklyn_1930$ED %in% (names(table(Brooklyn_1930$ED)[which(as.numeric(table(Brooklyn_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Brooklyn_1930=st_read("input_data/step_2_enumeration_districts/1930/New York/Brooklyn")
map_area=sum(st_area(Brooklyn_1930))
Brooklyn_1930=st_make_valid(Brooklyn_1930)
Brooklyn_1930$area=as.numeric(st_area(Brooklyn_1930))
Brooklyn_1930=Brooklyn_1930[which((row.names(Brooklyn_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Brooklyn_hexgrid.rda")
hex_grid=Brooklyn_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Brooklyn_1930$geometry)
Brooklyn_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Brooklyn_hexgrid))


#starting to intersect map and hex grid
Brooklyn_1930=st_make_valid(Brooklyn_1930)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
intersections_Brooklyn_1930=st_intersection(Brooklyn_1930, Brooklyn_hexgrid)
intersections_Brooklyn_1930$area_intersect=as.numeric(st_area(intersections_Brooklyn_1930))
Brooklyn_1930=st_drop_geometry(Brooklyn_1930)
intersections_Brooklyn_1930$ED_area=as.numeric(Brooklyn_1930[match(intersections_Brooklyn_1930$ED, Brooklyn_1930$ED), "area"])
intersections_Brooklyn_1930$proportion_intersected=intersections_Brooklyn_1930$area_intersect/intersections_Brooklyn_1930$ED_area

sum(intersections_Brooklyn_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Brooklyn_1930, file="intermediate_outputs/step_2_intersections/intersections_Brooklyn_1930.rda")


#Manhattan
library(sf)
Manhattan_1930=st_read("input_data/step_2_enumeration_districts/1930/New York/Manhattan")
Manhattan_1930=st_make_valid(Manhattan_1930)
Manhattan_1930$area=as.numeric(st_area(Manhattan_1930))

#repeated observations, keeping max size rep of ED
Manhattan_1930=st_drop_geometry(Manhattan_1930)
ind=vector(mode="character", length = length(unique(Manhattan_1930[which(Manhattan_1930$ED %in% (names(table(Manhattan_1930$ED)[which(as.numeric(table(Manhattan_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Manhattan_1930[which(Manhattan_1930$ED %in% (names(table(Manhattan_1930$ED)[which(as.numeric(table(Manhattan_1930$ED))>1)]))),"ED"]))){
  tmp=Manhattan_1930[which(Manhattan_1930$ED %in% unique(Manhattan_1930[which(Manhattan_1930$ED %in% (names(table(Manhattan_1930$ED)[which(as.numeric(table(Manhattan_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Manhattan_1930=st_read("input_data/step_2_enumeration_districts/1930/New York/Manhattan")
map_area=sum(st_area(Manhattan_1930))
Manhattan_1930=st_make_valid(Manhattan_1930)
Manhattan_1930$area=as.numeric(st_area(Manhattan_1930))
Manhattan_1930=Manhattan_1930[which((row.names(Manhattan_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Manhattan_hexgrid.rda")
hex_grid=Manhattan_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Manhattan_1930$geometry)
Manhattan_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Manhattan_hexgrid))


#starting to intersect map and hex grid
Manhattan_1930=st_make_valid(Manhattan_1930)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
intersections_Manhattan_1930=st_intersection(Manhattan_1930, Manhattan_hexgrid)
intersections_Manhattan_1930$area_intersect=as.numeric(st_area(intersections_Manhattan_1930))
Manhattan_1930=st_drop_geometry(Manhattan_1930)
intersections_Manhattan_1930$ED_area=as.numeric(Manhattan_1930[match(intersections_Manhattan_1930$ED, Manhattan_1930$ED), "area"])
intersections_Manhattan_1930$proportion_intersected=intersections_Manhattan_1930$area_intersect/intersections_Manhattan_1930$ED_area

sum(intersections_Manhattan_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Manhattan_1930, file="intermediate_outputs/step_2_intersections/intersections_Manhattan_1930.rda")


#Philadelphia
library(sf)
Philadelphia_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Philadelphia")
Philadelphia_1930=st_make_valid(Philadelphia_1930)
Philadelphia_1930$area=as.numeric(st_area(Philadelphia_1930))

#repeated observations, keeping max size rep of ED
Philadelphia_1930=st_drop_geometry(Philadelphia_1930)
ind=vector(mode="character", length = length(unique(Philadelphia_1930[which(Philadelphia_1930$ED %in% (names(table(Philadelphia_1930$ED)[which(as.numeric(table(Philadelphia_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Philadelphia_1930[which(Philadelphia_1930$ED %in% (names(table(Philadelphia_1930$ED)[which(as.numeric(table(Philadelphia_1930$ED))>1)]))),"ED"]))){
  tmp=Philadelphia_1930[which(Philadelphia_1930$ED %in% unique(Philadelphia_1930[which(Philadelphia_1930$ED %in% (names(table(Philadelphia_1930$ED)[which(as.numeric(table(Philadelphia_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Philadelphia_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Philadelphia")
map_area=sum(st_area(Philadelphia_1930))
Philadelphia_1930=st_make_valid(Philadelphia_1930)
Philadelphia_1930$area=as.numeric(st_area(Philadelphia_1930))
Philadelphia_1930=Philadelphia_1930[which((row.names(Philadelphia_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")
hex_grid=Philadelphia_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Philadelphia_1930$geometry)
Philadelphia_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Philadelphia_hexgrid))


#starting to intersect map and hex grid
Philadelphia_1930=st_make_valid(Philadelphia_1930)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
intersections_Philadelphia_1930=st_intersection(Philadelphia_1930, Philadelphia_hexgrid)
intersections_Philadelphia_1930$area_intersect=as.numeric(st_area(intersections_Philadelphia_1930))
Philadelphia_1930=st_drop_geometry(Philadelphia_1930)
intersections_Philadelphia_1930$ED_area=as.numeric(Philadelphia_1930[match(intersections_Philadelphia_1930$ED, Philadelphia_1930$ED), "area"])
intersections_Philadelphia_1930$proportion_intersected=intersections_Philadelphia_1930$area_intersect/intersections_Philadelphia_1930$ED_area

sum(intersections_Philadelphia_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Philadelphia_1930, file="intermediate_outputs/step_2_intersections/intersections_Philadelphia_1930.rda")

#Pittsburgh
library(sf)
Pittsburgh_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Pittsburgh")
Pittsburgh_1930=st_make_valid(Pittsburgh_1930)
Pittsburgh_1930$area=as.numeric(st_area(Pittsburgh_1930))

#repeated observations, keeping max size rep of ED
Pittsburgh_1930=st_drop_geometry(Pittsburgh_1930)
ind=vector(mode="character", length = length(unique(Pittsburgh_1930[which(Pittsburgh_1930$ED %in% (names(table(Pittsburgh_1930$ED)[which(as.numeric(table(Pittsburgh_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Pittsburgh_1930[which(Pittsburgh_1930$ED %in% (names(table(Pittsburgh_1930$ED)[which(as.numeric(table(Pittsburgh_1930$ED))>1)]))),"ED"]))){
  tmp=Pittsburgh_1930[which(Pittsburgh_1930$ED %in% unique(Pittsburgh_1930[which(Pittsburgh_1930$ED %in% (names(table(Pittsburgh_1930$ED)[which(as.numeric(table(Pittsburgh_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Pittsburgh_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Pittsburgh")
map_area=sum(st_area(Pittsburgh_1930))
Pittsburgh_1930=st_make_valid(Pittsburgh_1930)
Pittsburgh_1930$area=as.numeric(st_area(Pittsburgh_1930))
Pittsburgh_1930=Pittsburgh_1930[which((row.names(Pittsburgh_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")
hex_grid=Pittsburgh_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Pittsburgh_1930$geometry)
Pittsburgh_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Pittsburgh_hexgrid))


#starting to intersect map and hex grid
Pittsburgh_1930=st_make_valid(Pittsburgh_1930)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
intersections_Pittsburgh_1930=st_intersection(Pittsburgh_1930, Pittsburgh_hexgrid)
intersections_Pittsburgh_1930$area_intersect=as.numeric(st_area(intersections_Pittsburgh_1930))
Pittsburgh_1930=st_drop_geometry(Pittsburgh_1930)
intersections_Pittsburgh_1930$ED_area=as.numeric(Pittsburgh_1930[match(intersections_Pittsburgh_1930$ED, Pittsburgh_1930$ED), "area"])
intersections_Pittsburgh_1930$proportion_intersected=intersections_Pittsburgh_1930$area_intersect/intersections_Pittsburgh_1930$ED_area

sum(intersections_Pittsburgh_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Pittsburgh_1930, file="intermediate_outputs/step_2_intersections/intersections_Pittsburgh_1930.rda")


#StLouis
library(sf)
StLouis_1930=st_read("input_data/step_2_enumeration_districts/1930/Missouri/St. Louis")
StLouis_1930=st_make_valid(StLouis_1930)
StLouis_1930$area=as.numeric(st_area(StLouis_1930))

#repeated observations, keeping max size rep of ED
StLouis_1930=st_drop_geometry(StLouis_1930)
ind=vector(mode="character", length = length(unique(StLouis_1930[which(StLouis_1930$ED %in% (names(table(StLouis_1930$ED)[which(as.numeric(table(StLouis_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(StLouis_1930[which(StLouis_1930$ED %in% (names(table(StLouis_1930$ED)[which(as.numeric(table(StLouis_1930$ED))>1)]))),"ED"]))){
  tmp=StLouis_1930[which(StLouis_1930$ED %in% unique(StLouis_1930[which(StLouis_1930$ED %in% (names(table(StLouis_1930$ED)[which(as.numeric(table(StLouis_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
StLouis_1930=st_read("input_data/step_2_enumeration_districts/1930/Missouri/St. Louis")
map_area=sum(st_area(StLouis_1930))
StLouis_1930=st_make_valid(StLouis_1930)
StLouis_1930$area=as.numeric(st_area(StLouis_1930))
StLouis_1930=StLouis_1930[which((row.names(StLouis_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")
hex_grid=StLouis_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, StLouis_1930$geometry)
StLouis_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(StLouis_hexgrid))


#starting to intersect map and hex grid
StLouis_1930=st_make_valid(StLouis_1930)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
intersections_StLouis_1930=st_intersection(StLouis_1930, StLouis_hexgrid)
intersections_StLouis_1930$area_intersect=as.numeric(st_area(intersections_StLouis_1930))
StLouis_1930=st_drop_geometry(StLouis_1930)
intersections_StLouis_1930$ED_area=as.numeric(StLouis_1930[match(intersections_StLouis_1930$ED, StLouis_1930$ED), "area"])
intersections_StLouis_1930$proportion_intersected=intersections_StLouis_1930$area_intersect/intersections_StLouis_1930$ED_area

sum(intersections_StLouis_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_StLouis_1930, file="intermediate_outputs/step_2_intersections/intersections_StLouis_1930.rda")


#Detroit
library(sf)
Detroit_1930=st_read("input_data/step_2_enumeration_districts/1930/Michigan/Detroit")
StLouis_1930=st_read("input_data/step_2_enumeration_districts/1930/Missouri/St. Louis")
Detroit_1930=st_transform(Detroit_1930, crs=st_crs(StLouis_1930))
Detroit_1930=st_make_valid(Detroit_1930)
Detroit_1930$area=as.numeric(st_area(Detroit_1930))

#repeated observations, keeping max size rep of ED
Detroit_1930=st_drop_geometry(Detroit_1930)
ind=vector(mode="character", length = length(unique(Detroit_1930[which(Detroit_1930$ED %in% (names(table(Detroit_1930$ED)[which(as.numeric(table(Detroit_1930$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Detroit_1930[which(Detroit_1930$ED %in% (names(table(Detroit_1930$ED)[which(as.numeric(table(Detroit_1930$ED))>1)]))),"ED"]))){
  tmp=Detroit_1930[which(Detroit_1930$ED %in% unique(Detroit_1930[which(Detroit_1930$ED %in% (names(table(Detroit_1930$ED)[which(as.numeric(table(Detroit_1930$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Detroit_1930=st_read("input_data/step_2_enumeration_districts/1930/Michigan/Detroit")
Detroit_1930=st_transform(Detroit_1930, crs=st_crs(StLouis_1930))
Detroit_1930=st_make_valid(Detroit_1930)
map_area=sum(st_area(Detroit_1930))
Detroit_1930=st_make_valid(Detroit_1930)
Detroit_1930$area=as.numeric(st_area(Detroit_1930))
Detroit_1930=Detroit_1930[which((row.names(Detroit_1930) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
hex_grid=Detroit_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1930$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Detroit_hexgrid))


#starting to intersect map and hex grid
Detroit_1930=st_make_valid(Detroit_1930)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
intersections_Detroit_1930=st_intersection(Detroit_1930, Detroit_hexgrid)
intersections_Detroit_1930$area_intersect=as.numeric(st_area(intersections_Detroit_1930))
Detroit_1930=st_drop_geometry(Detroit_1930)
intersections_Detroit_1930$ED_area=as.numeric(Detroit_1930[match(intersections_Detroit_1930$ED, Detroit_1930$ED), "area"])
intersections_Detroit_1930$proportion_intersected=intersections_Detroit_1930$area_intersect/intersections_Detroit_1930$ED_area

sum(intersections_Detroit_1930$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Detroit_1930, file="intermediate_outputs/step_2_intersections/intersections_Detroit_1930.rda")


#1910
#Chicago
library(sf)
Chicago_1910=st_read("input_data/step_2_enumeration_districts/1910/Illinois/Chicago")
Chicago_1910=st_make_valid(Chicago_1910)
Chicago_1910$area=as.numeric(st_area(Chicago_1910))

#repeated observations, keeping max size rep of ED
Chicago_1910=st_drop_geometry(Chicago_1910)
ind=vector(mode="character", length = length(unique(Chicago_1910[which(Chicago_1910$ED %in% (names(table(Chicago_1910$ED)[which(as.numeric(table(Chicago_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Chicago_1910[which(Chicago_1910$ED %in% (names(table(Chicago_1910$ED)[which(as.numeric(table(Chicago_1910$ED))>1)]))),"ED"]))){
  tmp=Chicago_1910[which(Chicago_1910$ED %in% unique(Chicago_1910[which(Chicago_1910$ED %in% (names(table(Chicago_1910$ED)[which(as.numeric(table(Chicago_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Chicago_1910=st_read("input_data/step_2_enumeration_districts/1910/Illinois/Chicago")
map_area=sum(st_area(Chicago_1910))
Chicago_1910=st_make_valid(Chicago_1910)
Chicago_1910$area=as.numeric(st_area(Chicago_1910))
Chicago_1910=Chicago_1910[which((row.names(Chicago_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
hex_grid=Chicago_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Chicago_1910$geometry)
Chicago_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Chicago_hexgrid))


#starting to intersect map and hex grid
Chicago_1910=st_make_valid(Chicago_1910)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
intersections_Chicago_1910=st_intersection(Chicago_1910, Chicago_hexgrid)
intersections_Chicago_1910$area_intersect=as.numeric(st_area(intersections_Chicago_1910))
Chicago_1910=st_drop_geometry(Chicago_1910)
intersections_Chicago_1910$ED_area=as.numeric(Chicago_1910[match(intersections_Chicago_1910$ED, Chicago_1910$ED), "area"])
intersections_Chicago_1910$proportion_intersected=intersections_Chicago_1910$area_intersect/intersections_Chicago_1910$ED_area

sum(intersections_Chicago_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Chicago_1910, file="intermediate_outputs/step_2_intersections/intersections_Chicago_1910.rda")


#Baltimore
library(sf)
Baltimore_1910=st_read("input_data/step_2_enumeration_districts/1910/Maryland/Baltimore")
Baltimore_1910=st_make_valid(Baltimore_1910)
Baltimore_1910$area=as.numeric(st_area(Baltimore_1910))

#repeated observations, keeping max size rep of ED
Baltimore_1910=st_drop_geometry(Baltimore_1910)
ind=vector(mode="character", length = length(unique(Baltimore_1910[which(Baltimore_1910$ED %in% (names(table(Baltimore_1910$ED)[which(as.numeric(table(Baltimore_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Baltimore_1910[which(Baltimore_1910$ED %in% (names(table(Baltimore_1910$ED)[which(as.numeric(table(Baltimore_1910$ED))>1)]))),"ED"]))){
  tmp=Baltimore_1910[which(Baltimore_1910$ED %in% unique(Baltimore_1910[which(Baltimore_1910$ED %in% (names(table(Baltimore_1910$ED)[which(as.numeric(table(Baltimore_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Baltimore_1910=st_read("input_data/step_2_enumeration_districts/1910/Maryland/Baltimore")
map_area=sum(st_area(Baltimore_1910))
Baltimore_1910=st_make_valid(Baltimore_1910)
Baltimore_1910$area=as.numeric(st_area(Baltimore_1910))
Baltimore_1910=Baltimore_1910[which((row.names(Baltimore_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")
hex_grid=Baltimore_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Baltimore_1910$geometry)
Baltimore_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Baltimore_hexgrid))


#starting to intersect map and hex grid
Baltimore_1910=st_make_valid(Baltimore_1910)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
intersections_Baltimore_1910=st_intersection(Baltimore_1910, Baltimore_hexgrid)
intersections_Baltimore_1910$area_intersect=as.numeric(st_area(intersections_Baltimore_1910))
Baltimore_1910=st_drop_geometry(Baltimore_1910)
intersections_Baltimore_1910$ED_area=as.numeric(Baltimore_1910[match(intersections_Baltimore_1910$ED, Baltimore_1910$ED), "area"])
intersections_Baltimore_1910$proportion_intersected=intersections_Baltimore_1910$area_intersect/intersections_Baltimore_1910$ED_area

sum(intersections_Baltimore_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Baltimore_1910, file="intermediate_outputs/step_2_intersections/intersections_Baltimore_1910.rda")


#Boston
library(sf)
Boston_1910=st_read("input_data/step_2_enumeration_districts/1910/Massachusetts/Boston")
Boston_1910=st_make_valid(Boston_1910)
Boston_1910$area=as.numeric(st_area(Boston_1910))

#repeated observations, keeping max size rep of ED
Boston_1910=st_drop_geometry(Boston_1910)
ind=vector(mode="character", length = length(unique(Boston_1910[which(Boston_1910$ED %in% (names(table(Boston_1910$ED)[which(as.numeric(table(Boston_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Boston_1910[which(Boston_1910$ED %in% (names(table(Boston_1910$ED)[which(as.numeric(table(Boston_1910$ED))>1)]))),"ED"]))){
  tmp=Boston_1910[which(Boston_1910$ED %in% unique(Boston_1910[which(Boston_1910$ED %in% (names(table(Boston_1910$ED)[which(as.numeric(table(Boston_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Boston_1910=st_read("input_data/step_2_enumeration_districts/1910/Massachusetts/Boston")
map_area=sum(st_area(Boston_1910))
Boston_1910=st_make_valid(Boston_1910)
Boston_1910$area=as.numeric(st_area(Boston_1910))
Boston_1910=Boston_1910[which((row.names(Boston_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")
hex_grid=Boston_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Boston_1910$geometry)
Boston_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Boston_hexgrid))


#starting to intersect map and hex grid
Boston_1910=st_make_valid(Boston_1910)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
intersections_Boston_1910=st_intersection(Boston_1910, Boston_hexgrid)
intersections_Boston_1910$area_intersect=as.numeric(st_area(intersections_Boston_1910))
Boston_1910=st_drop_geometry(Boston_1910)
intersections_Boston_1910$ED_area=as.numeric(Boston_1910[match(intersections_Boston_1910$ED, Boston_1910$ED), "area"])
intersections_Boston_1910$proportion_intersected=intersections_Boston_1910$area_intersect/intersections_Boston_1910$ED_area

sum(intersections_Boston_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Boston_1910, file="intermediate_outputs/step_2_intersections/intersections_Boston_1910.rda")


#Cleveland
library(sf)
Cleveland_1910=st_read("input_data/step_2_enumeration_districts/1910/Ohio/Cleveland")
Cleveland_1910=st_make_valid(Cleveland_1910)
Cleveland_1910$area=as.numeric(st_area(Cleveland_1910))

#repeated observations, keeping max size rep of ED
Cleveland_1910=st_drop_geometry(Cleveland_1910)
ind=vector(mode="character", length = length(unique(Cleveland_1910[which(Cleveland_1910$ED %in% (names(table(Cleveland_1910$ED)[which(as.numeric(table(Cleveland_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cleveland_1910[which(Cleveland_1910$ED %in% (names(table(Cleveland_1910$ED)[which(as.numeric(table(Cleveland_1910$ED))>1)]))),"ED"]))){
  tmp=Cleveland_1910[which(Cleveland_1910$ED %in% unique(Cleveland_1910[which(Cleveland_1910$ED %in% (names(table(Cleveland_1910$ED)[which(as.numeric(table(Cleveland_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cleveland_1910=st_read("input_data/step_2_enumeration_districts/1910/Ohio/Cleveland")
map_area=sum(st_area(Cleveland_1910))
Cleveland_1910=st_make_valid(Cleveland_1910)
Cleveland_1910$area=as.numeric(st_area(Cleveland_1910))
Cleveland_1910=Cleveland_1910[which((row.names(Cleveland_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")
hex_grid=Cleveland_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cleveland_1910$geometry)
Cleveland_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cleveland_hexgrid))


#starting to intersect map and hex grid
Cleveland_1910=st_make_valid(Cleveland_1910)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
intersections_Cleveland_1910=st_intersection(Cleveland_1910, Cleveland_hexgrid)
intersections_Cleveland_1910$area_intersect=as.numeric(st_area(intersections_Cleveland_1910))
Cleveland_1910=st_drop_geometry(Cleveland_1910)
intersections_Cleveland_1910$ED_area=as.numeric(Cleveland_1910[match(intersections_Cleveland_1910$ED, Cleveland_1910$ED), "area"])
intersections_Cleveland_1910$proportion_intersected=intersections_Cleveland_1910$area_intersect/intersections_Cleveland_1910$ED_area

sum(intersections_Cleveland_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cleveland_1910, file="intermediate_outputs/step_2_intersections/intersections_Cleveland_1910.rda")


#Cincinnati
library(sf)
Cincinnati_1910=st_read("input_data/step_2_enumeration_districts/1910/Ohio/Cincinnati")
Cincinnati_1910=st_make_valid(Cincinnati_1910)
Cincinnati_1910$area=as.numeric(st_area(Cincinnati_1910))

#repeated observations, keeping max size rep of ED
Cincinnati_1910=st_drop_geometry(Cincinnati_1910)
ind=vector(mode="character", length = length(unique(Cincinnati_1910[which(Cincinnati_1910$ED %in% (names(table(Cincinnati_1910$ED)[which(as.numeric(table(Cincinnati_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cincinnati_1910[which(Cincinnati_1910$ED %in% (names(table(Cincinnati_1910$ED)[which(as.numeric(table(Cincinnati_1910$ED))>1)]))),"ED"]))){
  tmp=Cincinnati_1910[which(Cincinnati_1910$ED %in% unique(Cincinnati_1910[which(Cincinnati_1910$ED %in% (names(table(Cincinnati_1910$ED)[which(as.numeric(table(Cincinnati_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cincinnati_1910=st_read("input_data/step_2_enumeration_districts/1910/Ohio/Cincinnati")
map_area=sum(st_area(Cincinnati_1910))
Cincinnati_1910=st_make_valid(Cincinnati_1910)
Cincinnati_1910$area=as.numeric(st_area(Cincinnati_1910))
Cincinnati_1910=Cincinnati_1910[which((row.names(Cincinnati_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")
hex_grid=Cincinnati_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cincinnati_1910$geometry)
Cincinnati_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cincinnati_hexgrid))


#starting to intersect map and hex grid
Cincinnati_1910=st_make_valid(Cincinnati_1910)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
intersections_Cincinnati_1910=st_intersection(Cincinnati_1910, Cincinnati_hexgrid)
intersections_Cincinnati_1910$area_intersect=as.numeric(st_area(intersections_Cincinnati_1910))
Cincinnati_1910=st_drop_geometry(Cincinnati_1910)
intersections_Cincinnati_1910$ED_area=as.numeric(Cincinnati_1910[match(intersections_Cincinnati_1910$ED, Cincinnati_1910$ED), "area"])
intersections_Cincinnati_1910$proportion_intersected=intersections_Cincinnati_1910$area_intersect/intersections_Cincinnati_1910$ED_area

sum(intersections_Cincinnati_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cincinnati_1910, file="intermediate_outputs/step_2_intersections/intersections_Cincinnati_1910.rda")


#Brooklyn
library(sf)
Brooklyn_1910=st_read("input_data/step_2_enumeration_districts/1910/New York/Brooklyn")
Brooklyn_1910=st_make_valid(Brooklyn_1910)
Brooklyn_1910$area=as.numeric(st_area(Brooklyn_1910))

#repeated observations, keeping max size rep of ED
Brooklyn_1910=st_drop_geometry(Brooklyn_1910)
ind=vector(mode="character", length = length(unique(Brooklyn_1910[which(Brooklyn_1910$ED %in% (names(table(Brooklyn_1910$ED)[which(as.numeric(table(Brooklyn_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Brooklyn_1910[which(Brooklyn_1910$ED %in% (names(table(Brooklyn_1910$ED)[which(as.numeric(table(Brooklyn_1910$ED))>1)]))),"ED"]))){
  tmp=Brooklyn_1910[which(Brooklyn_1910$ED %in% unique(Brooklyn_1910[which(Brooklyn_1910$ED %in% (names(table(Brooklyn_1910$ED)[which(as.numeric(table(Brooklyn_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Brooklyn_1910=st_read("input_data/step_2_enumeration_districts/1910/New York/Brooklyn")
map_area=sum(st_area(Brooklyn_1910))
Brooklyn_1910=st_make_valid(Brooklyn_1910)
Brooklyn_1910$area=as.numeric(st_area(Brooklyn_1910))
Brooklyn_1910=Brooklyn_1910[which((row.names(Brooklyn_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Brooklyn_hexgrid.rda")
hex_grid=Brooklyn_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Brooklyn_1910$geometry)
Brooklyn_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Brooklyn_hexgrid))


#starting to intersect map and hex grid
Brooklyn_1910=st_make_valid(Brooklyn_1910)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
intersections_Brooklyn_1910=st_intersection(Brooklyn_1910, Brooklyn_hexgrid)
intersections_Brooklyn_1910$area_intersect=as.numeric(st_area(intersections_Brooklyn_1910))
Brooklyn_1910=st_drop_geometry(Brooklyn_1910)
intersections_Brooklyn_1910$ED_area=as.numeric(Brooklyn_1910[match(intersections_Brooklyn_1910$ED, Brooklyn_1910$ED), "area"])
intersections_Brooklyn_1910$proportion_intersected=intersections_Brooklyn_1910$area_intersect/intersections_Brooklyn_1910$ED_area

sum(intersections_Brooklyn_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Brooklyn_1910, file="intermediate_outputs/step_2_intersections/intersections_Brooklyn_1910.rda")


#Manhattan
library(sf)
Manhattan_1910=st_read("input_data/step_2_enumeration_districts/1910/New York/Manhattan")
Manhattan_1910=st_make_valid(Manhattan_1910)
Manhattan_1910$area=as.numeric(st_area(Manhattan_1910))

#repeated observations, keeping max size rep of ED
Manhattan_1910=st_drop_geometry(Manhattan_1910)
ind=vector(mode="character", length = length(unique(Manhattan_1910[which(Manhattan_1910$ED %in% (names(table(Manhattan_1910$ED)[which(as.numeric(table(Manhattan_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Manhattan_1910[which(Manhattan_1910$ED %in% (names(table(Manhattan_1910$ED)[which(as.numeric(table(Manhattan_1910$ED))>1)]))),"ED"]))){
  tmp=Manhattan_1910[which(Manhattan_1910$ED %in% unique(Manhattan_1910[which(Manhattan_1910$ED %in% (names(table(Manhattan_1910$ED)[which(as.numeric(table(Manhattan_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Manhattan_1910=st_read("input_data/step_2_enumeration_districts/1910/New York/Manhattan")
map_area=sum(st_area(Manhattan_1910))
Manhattan_1910=st_make_valid(Manhattan_1910)
Manhattan_1910$area=as.numeric(st_area(Manhattan_1910))
Manhattan_1910=Manhattan_1910[which((row.names(Manhattan_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Manhattan_hexgrid.rda")
hex_grid=Manhattan_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Manhattan_1910$geometry)
Manhattan_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Manhattan_hexgrid))


#starting to intersect map and hex grid
Manhattan_1910=st_make_valid(Manhattan_1910)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
intersections_Manhattan_1910=st_intersection(Manhattan_1910, Manhattan_hexgrid)
intersections_Manhattan_1910$area_intersect=as.numeric(st_area(intersections_Manhattan_1910))
Manhattan_1910=st_drop_geometry(Manhattan_1910)
intersections_Manhattan_1910$ED_area=as.numeric(Manhattan_1910[match(intersections_Manhattan_1910$ED, Manhattan_1910$ED), "area"])
intersections_Manhattan_1910$proportion_intersected=intersections_Manhattan_1910$area_intersect/intersections_Manhattan_1910$ED_area

sum(intersections_Manhattan_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Manhattan_1910, file="intermediate_outputs/step_2_intersections/intersections_Manhattan_1910.rda")


#Philadelphia
library(sf)
Philadelphia_1910=st_read("input_data/step_2_enumeration_districts/1910/Pennsylvania/Philadelphia")
Philadelphia_1910=st_make_valid(Philadelphia_1910)
Philadelphia_1910$area=as.numeric(st_area(Philadelphia_1910))

#repeated observations, keeping max size rep of ED
Philadelphia_1910=st_drop_geometry(Philadelphia_1910)
ind=vector(mode="character", length = length(unique(Philadelphia_1910[which(Philadelphia_1910$ED %in% (names(table(Philadelphia_1910$ED)[which(as.numeric(table(Philadelphia_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Philadelphia_1910[which(Philadelphia_1910$ED %in% (names(table(Philadelphia_1910$ED)[which(as.numeric(table(Philadelphia_1910$ED))>1)]))),"ED"]))){
  tmp=Philadelphia_1910[which(Philadelphia_1910$ED %in% unique(Philadelphia_1910[which(Philadelphia_1910$ED %in% (names(table(Philadelphia_1910$ED)[which(as.numeric(table(Philadelphia_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Philadelphia_1910=st_read("input_data/step_2_enumeration_districts/1910/Pennsylvania/Philadelphia")
map_area=sum(st_area(Philadelphia_1910))
Philadelphia_1910=st_make_valid(Philadelphia_1910)
Philadelphia_1910$area=as.numeric(st_area(Philadelphia_1910))
Philadelphia_1910=Philadelphia_1910[which((row.names(Philadelphia_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")
hex_grid=Philadelphia_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Philadelphia_1910$geometry)
Philadelphia_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Philadelphia_hexgrid))


#starting to intersect map and hex grid
Philadelphia_1910=st_make_valid(Philadelphia_1910)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
intersections_Philadelphia_1910=st_intersection(Philadelphia_1910, Philadelphia_hexgrid)
intersections_Philadelphia_1910$area_intersect=as.numeric(st_area(intersections_Philadelphia_1910))
Philadelphia_1910=st_drop_geometry(Philadelphia_1910)
intersections_Philadelphia_1910$ED_area=as.numeric(Philadelphia_1910[match(intersections_Philadelphia_1910$ED, Philadelphia_1910$ED), "area"])
intersections_Philadelphia_1910$proportion_intersected=intersections_Philadelphia_1910$area_intersect/intersections_Philadelphia_1910$ED_area

sum(intersections_Philadelphia_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Philadelphia_1910, file="intermediate_outputs/step_2_intersections/intersections_Philadelphia_1910.rda")

#Pittsburgh
library(sf)
Pittsburgh_1910=st_read("input_data/step_2_enumeration_districts/1910/Pennsylvania/Pittsburgh")
Pittsburgh_1910=st_make_valid(Pittsburgh_1910)
Pittsburgh_1910$area=as.numeric(st_area(Pittsburgh_1910))

#repeated observations, keeping max size rep of ED
Pittsburgh_1910=st_drop_geometry(Pittsburgh_1910)
ind=vector(mode="character", length = length(unique(Pittsburgh_1910[which(Pittsburgh_1910$ED %in% (names(table(Pittsburgh_1910$ED)[which(as.numeric(table(Pittsburgh_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Pittsburgh_1910[which(Pittsburgh_1910$ED %in% (names(table(Pittsburgh_1910$ED)[which(as.numeric(table(Pittsburgh_1910$ED))>1)]))),"ED"]))){
  tmp=Pittsburgh_1910[which(Pittsburgh_1910$ED %in% unique(Pittsburgh_1910[which(Pittsburgh_1910$ED %in% (names(table(Pittsburgh_1910$ED)[which(as.numeric(table(Pittsburgh_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Pittsburgh_1910=st_read("input_data/step_2_enumeration_districts/1910/Pennsylvania/Pittsburgh")
map_area=sum(st_area(Pittsburgh_1910))
Pittsburgh_1910=st_make_valid(Pittsburgh_1910)
Pittsburgh_1910$area=as.numeric(st_area(Pittsburgh_1910))
Pittsburgh_1910=Pittsburgh_1910[which((row.names(Pittsburgh_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")
hex_grid=Pittsburgh_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Pittsburgh_1910$geometry)
Pittsburgh_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Pittsburgh_hexgrid))


#starting to intersect map and hex grid
Pittsburgh_1910=st_make_valid(Pittsburgh_1910)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
intersections_Pittsburgh_1910=st_intersection(Pittsburgh_1910, Pittsburgh_hexgrid)
intersections_Pittsburgh_1910$area_intersect=as.numeric(st_area(intersections_Pittsburgh_1910))
Pittsburgh_1910=st_drop_geometry(Pittsburgh_1910)
intersections_Pittsburgh_1910$ED_area=as.numeric(Pittsburgh_1910[match(intersections_Pittsburgh_1910$ED, Pittsburgh_1910$ED), "area"])
intersections_Pittsburgh_1910$proportion_intersected=intersections_Pittsburgh_1910$area_intersect/intersections_Pittsburgh_1910$ED_area

sum(intersections_Pittsburgh_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Pittsburgh_1910, file="intermediate_outputs/step_2_intersections/intersections_Pittsburgh_1910.rda")


#StLouis
library(sf)
StLouis_1910=st_read("input_data/step_2_enumeration_districts/1910/Missouri/")
StLouis_1910=st_make_valid(StLouis_1910)
StLouis_1910$area=as.numeric(st_area(StLouis_1910))

#repeated observations, keeping max size rep of ED
StLouis_1910=st_drop_geometry(StLouis_1910)
ind=vector(mode="character", length = length(unique(StLouis_1910[which(StLouis_1910$ED %in% (names(table(StLouis_1910$ED)[which(as.numeric(table(StLouis_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(StLouis_1910[which(StLouis_1910$ED %in% (names(table(StLouis_1910$ED)[which(as.numeric(table(StLouis_1910$ED))>1)]))),"ED"]))){
  tmp=StLouis_1910[which(StLouis_1910$ED %in% unique(StLouis_1910[which(StLouis_1910$ED %in% (names(table(StLouis_1910$ED)[which(as.numeric(table(StLouis_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
StLouis_1910=st_read("input_data/step_2_enumeration_districts/1910/Missouri/")
map_area=sum(st_area(StLouis_1910))
StLouis_1910=st_make_valid(StLouis_1910)
StLouis_1910$area=as.numeric(st_area(StLouis_1910))
StLouis_1910=StLouis_1910[which((row.names(StLouis_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")
hex_grid=StLouis_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, StLouis_1910$geometry)
StLouis_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(StLouis_hexgrid))


#starting to intersect map and hex grid
StLouis_1910=st_make_valid(StLouis_1910)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
intersections_StLouis_1910=st_intersection(StLouis_1910, StLouis_hexgrid)
intersections_StLouis_1910$area_intersect=as.numeric(st_area(intersections_StLouis_1910))
StLouis_1910=st_drop_geometry(StLouis_1910)
intersections_StLouis_1910$ED_area=as.numeric(StLouis_1910[match(intersections_StLouis_1910$ED, StLouis_1910$ED), "area"])
intersections_StLouis_1910$proportion_intersected=intersections_StLouis_1910$area_intersect/intersections_StLouis_1910$ED_area

sum(intersections_StLouis_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_StLouis_1910, file="intermediate_outputs/step_2_intersections/intersections_StLouis_1910.rda")


#Detroit
library(sf)
Detroit_1910=st_read("input_data/step_2_enumeration_districts/1910/Michigan/Detroit")
StLouis_1910=st_read("input_data/step_2_enumeration_districts/1910/Missouri/")
Detroit_1910=st_transform(Detroit_1910, crs=st_crs(StLouis_1910))
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_1910$area=as.numeric(st_area(Detroit_1910))

#repeated observations, keeping max size rep of ED
Detroit_1910=st_drop_geometry(Detroit_1910)
ind=vector(mode="character", length = length(unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"]))){
  tmp=Detroit_1910[which(Detroit_1910$ED %in% unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Detroit_1910=st_read("input_data/step_2_enumeration_districts/1910/Michigan/Detroit")
Detroit_1910=st_transform(Detroit_1910, crs=st_crs(StLouis_1910))
Detroit_1910=st_make_valid(Detroit_1910)
map_area=sum(st_area(Detroit_1910))
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_1910$area=as.numeric(st_area(Detroit_1910))
Detroit_1910=Detroit_1910[which((row.names(Detroit_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
hex_grid=Detroit_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1910$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Detroit_hexgrid))


#starting to intersect map and hex grid
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
intersections_Detroit_1910=st_intersection(Detroit_1910, Detroit_hexgrid)
intersections_Detroit_1910$area_intersect=as.numeric(st_area(intersections_Detroit_1910))
Detroit_1910=st_drop_geometry(Detroit_1910)
intersections_Detroit_1910$ED_area=as.numeric(Detroit_1910[match(intersections_Detroit_1910$ED, Detroit_1910$ED), "area"])
intersections_Detroit_1910$proportion_intersected=intersections_Detroit_1910$area_intersect/intersections_Detroit_1910$ED_area

sum(intersections_Detroit_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Detroit_1910, file="intermediate_outputs/step_2_intersections/intersections_Detroit_1910.rda")


#1900
#Chicago
library(sf)
Chicago_1900=st_read("input_data/step_2_enumeration_districts/1900/Illinois/Chicago")
Chicago_1900=st_make_valid(Chicago_1900)
Chicago_1900$area=as.numeric(st_area(Chicago_1900))

#repeated observations, keeping max size rep of ED
Chicago_1900=st_drop_geometry(Chicago_1900)
ind=vector(mode="character", length = length(unique(Chicago_1900[which(Chicago_1900$ED %in% (names(table(Chicago_1900$ED)[which(as.numeric(table(Chicago_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Chicago_1900[which(Chicago_1900$ED %in% (names(table(Chicago_1900$ED)[which(as.numeric(table(Chicago_1900$ED))>1)]))),"ED"]))){
  tmp=Chicago_1900[which(Chicago_1900$ED %in% unique(Chicago_1900[which(Chicago_1900$ED %in% (names(table(Chicago_1900$ED)[which(as.numeric(table(Chicago_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Chicago_1900=st_read("input_data/step_2_enumeration_districts/1900/Illinois/Chicago")
map_area=sum(st_area(Chicago_1900))
Chicago_1900=st_make_valid(Chicago_1900)
Chicago_1900$area=as.numeric(st_area(Chicago_1900))
Chicago_1900=Chicago_1900[which((row.names(Chicago_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
hex_grid=Chicago_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Chicago_1900$geometry)
Chicago_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Chicago_hexgrid))


#starting to intersect map and hex grid
Chicago_1900=st_make_valid(Chicago_1900)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
intersections_Chicago_1900=st_intersection(Chicago_1900, Chicago_hexgrid)
intersections_Chicago_1900$area_intersect=as.numeric(st_area(intersections_Chicago_1900))
Chicago_1900=st_drop_geometry(Chicago_1900)
intersections_Chicago_1900$ED_area=as.numeric(Chicago_1900[match(intersections_Chicago_1900$ED, Chicago_1900$ED), "area"])
intersections_Chicago_1900$proportion_intersected=intersections_Chicago_1900$area_intersect/intersections_Chicago_1900$ED_area

sum(intersections_Chicago_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Chicago_1900, file="intermediate_outputs/step_2_intersections/intersections_Chicago_1900.rda")


#Baltimore
library(sf)
Baltimore_1900=st_read("input_data/step_2_enumeration_districts/1900/Maryland/Baltimore")
Baltimore_1900=st_make_valid(Baltimore_1900)
Baltimore_1900$area=as.numeric(st_area(Baltimore_1900))

#repeated observations, keeping max size rep of ED
Baltimore_1900=st_drop_geometry(Baltimore_1900)
ind=vector(mode="character", length = length(unique(Baltimore_1900[which(Baltimore_1900$ED %in% (names(table(Baltimore_1900$ED)[which(as.numeric(table(Baltimore_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Baltimore_1900[which(Baltimore_1900$ED %in% (names(table(Baltimore_1900$ED)[which(as.numeric(table(Baltimore_1900$ED))>1)]))),"ED"]))){
  tmp=Baltimore_1900[which(Baltimore_1900$ED %in% unique(Baltimore_1900[which(Baltimore_1900$ED %in% (names(table(Baltimore_1900$ED)[which(as.numeric(table(Baltimore_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Baltimore_1900=st_read("input_data/step_2_enumeration_districts/1900/Maryland/Baltimore")
map_area=sum(st_area(Baltimore_1900))
Baltimore_1900=st_make_valid(Baltimore_1900)
Baltimore_1900$area=as.numeric(st_area(Baltimore_1900))
Baltimore_1900=Baltimore_1900[which((row.names(Baltimore_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")
hex_grid=Baltimore_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Baltimore_1900$geometry)
Baltimore_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Baltimore_hexgrid))


#starting to intersect map and hex grid
Baltimore_1900=st_make_valid(Baltimore_1900)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
intersections_Baltimore_1900=st_intersection(Baltimore_1900, Baltimore_hexgrid)
intersections_Baltimore_1900$area_intersect=as.numeric(st_area(intersections_Baltimore_1900))
Baltimore_1900=st_drop_geometry(Baltimore_1900)
intersections_Baltimore_1900$ED_area=as.numeric(Baltimore_1900[match(intersections_Baltimore_1900$ED, Baltimore_1900$ED), "area"])
intersections_Baltimore_1900$proportion_intersected=intersections_Baltimore_1900$area_intersect/intersections_Baltimore_1900$ED_area

sum(intersections_Baltimore_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Baltimore_1900, file="intermediate_outputs/step_2_intersections/intersections_Baltimore_1900.rda")


#Boston
library(sf)
Boston_1900=st_read("input_data/step_2_enumeration_districts/1900/Massachusetts/Boston")
Boston_1900=st_make_valid(Boston_1900)
Boston_1900$area=as.numeric(st_area(Boston_1900))

#repeated observations, keeping max size rep of ED
Boston_1900=st_drop_geometry(Boston_1900)
ind=vector(mode="character", length = length(unique(Boston_1900[which(Boston_1900$ED %in% (names(table(Boston_1900$ED)[which(as.numeric(table(Boston_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Boston_1900[which(Boston_1900$ED %in% (names(table(Boston_1900$ED)[which(as.numeric(table(Boston_1900$ED))>1)]))),"ED"]))){
  tmp=Boston_1900[which(Boston_1900$ED %in% unique(Boston_1900[which(Boston_1900$ED %in% (names(table(Boston_1900$ED)[which(as.numeric(table(Boston_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Boston_1900=st_read("input_data/step_2_enumeration_districts/1900/Massachusetts/Boston")
map_area=sum(st_area(Boston_1900))
Boston_1900=st_make_valid(Boston_1900)
Boston_1900$area=as.numeric(st_area(Boston_1900))
Boston_1900=Boston_1900[which((row.names(Boston_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")
hex_grid=Boston_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Boston_1900$geometry)
Boston_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Boston_hexgrid))


#starting to intersect map and hex grid
Boston_1900=st_make_valid(Boston_1900)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
intersections_Boston_1900=st_intersection(Boston_1900, Boston_hexgrid)
intersections_Boston_1900$area_intersect=as.numeric(st_area(intersections_Boston_1900))
Boston_1900=st_drop_geometry(Boston_1900)
intersections_Boston_1900$ED_area=as.numeric(Boston_1900[match(intersections_Boston_1900$ED, Boston_1900$ED), "area"])
intersections_Boston_1900$proportion_intersected=intersections_Boston_1900$area_intersect/intersections_Boston_1900$ED_area

sum(intersections_Boston_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Boston_1900, file="intermediate_outputs/step_2_intersections/intersections_Boston_1900.rda")


#Cleveland
library(sf)
Cleveland_1900=st_read("input_data/step_2_enumeration_districts/1900/Ohio/Cleveland")
Cleveland_1900=st_make_valid(Cleveland_1900)
Cleveland_1900$area=as.numeric(st_area(Cleveland_1900))

#repeated observations, keeping max size rep of ED
Cleveland_1900=st_drop_geometry(Cleveland_1900)
ind=vector(mode="character", length = length(unique(Cleveland_1900[which(Cleveland_1900$ED %in% (names(table(Cleveland_1900$ED)[which(as.numeric(table(Cleveland_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cleveland_1900[which(Cleveland_1900$ED %in% (names(table(Cleveland_1900$ED)[which(as.numeric(table(Cleveland_1900$ED))>1)]))),"ED"]))){
  tmp=Cleveland_1900[which(Cleveland_1900$ED %in% unique(Cleveland_1900[which(Cleveland_1900$ED %in% (names(table(Cleveland_1900$ED)[which(as.numeric(table(Cleveland_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cleveland_1900=st_read("input_data/step_2_enumeration_districts/1900/Ohio/Cleveland")
map_area=sum(st_area(Cleveland_1900))
Cleveland_1900=st_make_valid(Cleveland_1900)
Cleveland_1900$area=as.numeric(st_area(Cleveland_1900))
Cleveland_1900=Cleveland_1900[which((row.names(Cleveland_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")
hex_grid=Cleveland_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cleveland_1900$geometry)
Cleveland_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cleveland_hexgrid))


#starting to intersect map and hex grid
Cleveland_1900=st_make_valid(Cleveland_1900)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
intersections_Cleveland_1900=st_intersection(Cleveland_1900, Cleveland_hexgrid)
intersections_Cleveland_1900$area_intersect=as.numeric(st_area(intersections_Cleveland_1900))
Cleveland_1900=st_drop_geometry(Cleveland_1900)
intersections_Cleveland_1900$ED_area=as.numeric(Cleveland_1900[match(intersections_Cleveland_1900$ED, Cleveland_1900$ED), "area"])
intersections_Cleveland_1900$proportion_intersected=intersections_Cleveland_1900$area_intersect/intersections_Cleveland_1900$ED_area

sum(intersections_Cleveland_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cleveland_1900, file="intermediate_outputs/step_2_intersections/intersections_Cleveland_1900.rda")


#Cincinnati
library(sf)
Cincinnati_1900=st_read("input_data/step_2_enumeration_districts/1900/Ohio/Cincinnati")
Cincinnati_1900=st_make_valid(Cincinnati_1900)
Cincinnati_1900$area=as.numeric(st_area(Cincinnati_1900))

#repeated observations, keeping max size rep of ED
Cincinnati_1900=st_drop_geometry(Cincinnati_1900)
ind=vector(mode="character", length = length(unique(Cincinnati_1900[which(Cincinnati_1900$ED %in% (names(table(Cincinnati_1900$ED)[which(as.numeric(table(Cincinnati_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cincinnati_1900[which(Cincinnati_1900$ED %in% (names(table(Cincinnati_1900$ED)[which(as.numeric(table(Cincinnati_1900$ED))>1)]))),"ED"]))){
  tmp=Cincinnati_1900[which(Cincinnati_1900$ED %in% unique(Cincinnati_1900[which(Cincinnati_1900$ED %in% (names(table(Cincinnati_1900$ED)[which(as.numeric(table(Cincinnati_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cincinnati_1900=st_read("input_data/step_2_enumeration_districts/1900/Ohio/Cincinnati")
map_area=sum(st_area(Cincinnati_1900))
Cincinnati_1900=st_make_valid(Cincinnati_1900)
Cincinnati_1900$area=as.numeric(st_area(Cincinnati_1900))
Cincinnati_1900=Cincinnati_1900[which((row.names(Cincinnati_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")
hex_grid=Cincinnati_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cincinnati_1900$geometry)
Cincinnati_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cincinnati_hexgrid))


#starting to intersect map and hex grid
Cincinnati_1900=st_make_valid(Cincinnati_1900)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
intersections_Cincinnati_1900=st_intersection(Cincinnati_1900, Cincinnati_hexgrid)
intersections_Cincinnati_1900$area_intersect=as.numeric(st_area(intersections_Cincinnati_1900))
Cincinnati_1900=st_drop_geometry(Cincinnati_1900)
intersections_Cincinnati_1900$ED_area=as.numeric(Cincinnati_1900[match(intersections_Cincinnati_1900$ED, Cincinnati_1900$ED), "area"])
intersections_Cincinnati_1900$proportion_intersected=intersections_Cincinnati_1900$area_intersect/intersections_Cincinnati_1900$ED_area

sum(intersections_Cincinnati_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cincinnati_1900, file="intermediate_outputs/step_2_intersections/intersections_Cincinnati_1900.rda")


#Brooklyn
library(sf)
Brooklyn_1900=st_read("input_data/step_2_enumeration_districts/1900/New York/Brooklyn")
Brooklyn_1900=st_make_valid(Brooklyn_1900)
Brooklyn_1900$area=as.numeric(st_area(Brooklyn_1900))

#repeated observations, keeping max size rep of ED
Brooklyn_1900=st_drop_geometry(Brooklyn_1900)
ind=vector(mode="character", length = length(unique(Brooklyn_1900[which(Brooklyn_1900$ED %in% (names(table(Brooklyn_1900$ED)[which(as.numeric(table(Brooklyn_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Brooklyn_1900[which(Brooklyn_1900$ED %in% (names(table(Brooklyn_1900$ED)[which(as.numeric(table(Brooklyn_1900$ED))>1)]))),"ED"]))){
  tmp=Brooklyn_1900[which(Brooklyn_1900$ED %in% unique(Brooklyn_1900[which(Brooklyn_1900$ED %in% (names(table(Brooklyn_1900$ED)[which(as.numeric(table(Brooklyn_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Brooklyn_1900=st_read("input_data/step_2_enumeration_districts/1900/New York/Brooklyn")
map_area=sum(st_area(Brooklyn_1900))
Brooklyn_1900=st_make_valid(Brooklyn_1900)
Brooklyn_1900$area=as.numeric(st_area(Brooklyn_1900))
Brooklyn_1900=Brooklyn_1900[which((row.names(Brooklyn_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Brooklyn_hexgrid.rda")
hex_grid=Brooklyn_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Brooklyn_1900$geometry)
Brooklyn_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Brooklyn_hexgrid))


#starting to intersect map and hex grid
Brooklyn_1900=st_make_valid(Brooklyn_1900)
Brooklyn_hexgrid=st_make_valid(Brooklyn_hexgrid)
intersections_Brooklyn_1900=st_intersection(Brooklyn_1900, Brooklyn_hexgrid)
intersections_Brooklyn_1900$area_intersect=as.numeric(st_area(intersections_Brooklyn_1900))
Brooklyn_1900=st_drop_geometry(Brooklyn_1900)
intersections_Brooklyn_1900$ED_area=as.numeric(Brooklyn_1900[match(intersections_Brooklyn_1900$ED, Brooklyn_1900$ED), "area"])
intersections_Brooklyn_1900$proportion_intersected=intersections_Brooklyn_1900$area_intersect/intersections_Brooklyn_1900$ED_area

sum(intersections_Brooklyn_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Brooklyn_1900, file="intermediate_outputs/step_2_intersections/intersections_Brooklyn_1900.rda")


#Manhattan
library(sf)
Manhattan_1900=st_read("input_data/step_2_enumeration_districts/1900/New York/Manhattan")
Manhattan_1900=st_make_valid(Manhattan_1900)
Manhattan_1900$area=as.numeric(st_area(Manhattan_1900))

#repeated observations, keeping max size rep of ED
Manhattan_1900=st_drop_geometry(Manhattan_1900)
ind=vector(mode="character", length = length(unique(Manhattan_1900[which(Manhattan_1900$ED %in% (names(table(Manhattan_1900$ED)[which(as.numeric(table(Manhattan_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Manhattan_1900[which(Manhattan_1900$ED %in% (names(table(Manhattan_1900$ED)[which(as.numeric(table(Manhattan_1900$ED))>1)]))),"ED"]))){
  tmp=Manhattan_1900[which(Manhattan_1900$ED %in% unique(Manhattan_1900[which(Manhattan_1900$ED %in% (names(table(Manhattan_1900$ED)[which(as.numeric(table(Manhattan_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Manhattan_1900=st_read("input_data/step_2_enumeration_districts/1900/New York/Manhattan")
map_area=sum(st_area(Manhattan_1900))
Manhattan_1900=st_make_valid(Manhattan_1900)
Manhattan_1900$area=as.numeric(st_area(Manhattan_1900))
Manhattan_1900=Manhattan_1900[which((row.names(Manhattan_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Manhattan_hexgrid.rda")
hex_grid=Manhattan_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Manhattan_1900$geometry)
Manhattan_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Manhattan_hexgrid))


#starting to intersect map and hex grid
Manhattan_1900=st_make_valid(Manhattan_1900)
Manhattan_hexgrid=st_make_valid(Manhattan_hexgrid)
intersections_Manhattan_1900=st_intersection(Manhattan_1900, Manhattan_hexgrid)
intersections_Manhattan_1900$area_intersect=as.numeric(st_area(intersections_Manhattan_1900))
Manhattan_1900=st_drop_geometry(Manhattan_1900)
intersections_Manhattan_1900$ED_area=as.numeric(Manhattan_1900[match(intersections_Manhattan_1900$ED, Manhattan_1900$ED), "area"])
intersections_Manhattan_1900$proportion_intersected=intersections_Manhattan_1900$area_intersect/intersections_Manhattan_1900$ED_area

sum(intersections_Manhattan_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Manhattan_1900, file="intermediate_outputs/step_2_intersections/intersections_Manhattan_1900.rda")


#Philadelphia
library(sf)
Philadelphia_1900=st_read("input_data/step_2_enumeration_districts/1900/Pennsylvania/Philadelphia")
Philadelphia_1900=st_make_valid(Philadelphia_1900)
Philadelphia_1900$area=as.numeric(st_area(Philadelphia_1900))

#repeated observations, keeping max size rep of ED
Philadelphia_1900=st_drop_geometry(Philadelphia_1900)
ind=vector(mode="character", length = length(unique(Philadelphia_1900[which(Philadelphia_1900$ED %in% (names(table(Philadelphia_1900$ED)[which(as.numeric(table(Philadelphia_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Philadelphia_1900[which(Philadelphia_1900$ED %in% (names(table(Philadelphia_1900$ED)[which(as.numeric(table(Philadelphia_1900$ED))>1)]))),"ED"]))){
  tmp=Philadelphia_1900[which(Philadelphia_1900$ED %in% unique(Philadelphia_1900[which(Philadelphia_1900$ED %in% (names(table(Philadelphia_1900$ED)[which(as.numeric(table(Philadelphia_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Philadelphia_1900=st_read("input_data/step_2_enumeration_districts/1900/Pennsylvania/Philadelphia")
map_area=sum(st_area(Philadelphia_1900))
Philadelphia_1900=st_make_valid(Philadelphia_1900)
Philadelphia_1900$area=as.numeric(st_area(Philadelphia_1900))
Philadelphia_1900=Philadelphia_1900[which((row.names(Philadelphia_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")
hex_grid=Philadelphia_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Philadelphia_1900$geometry)
Philadelphia_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Philadelphia_hexgrid))


#starting to intersect map and hex grid
Philadelphia_1900=st_make_valid(Philadelphia_1900)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
intersections_Philadelphia_1900=st_intersection(Philadelphia_1900, Philadelphia_hexgrid)
intersections_Philadelphia_1900$area_intersect=as.numeric(st_area(intersections_Philadelphia_1900))
Philadelphia_1900=st_drop_geometry(Philadelphia_1900)
intersections_Philadelphia_1900$ED_area=as.numeric(Philadelphia_1900[match(intersections_Philadelphia_1900$ED, Philadelphia_1900$ED), "area"])
intersections_Philadelphia_1900$proportion_intersected=intersections_Philadelphia_1900$area_intersect/intersections_Philadelphia_1900$ED_area

sum(intersections_Philadelphia_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Philadelphia_1900, file="intermediate_outputs/step_2_intersections/intersections_Philadelphia_1900.rda")

#Pittsburgh
library(sf)
Pittsburgh_1900=st_read("input_data/step_2_enumeration_districts/1900/Pennsylvania/Pittsburgh")
Pittsburgh_1900=st_make_valid(Pittsburgh_1900)
Pittsburgh_1900$area=as.numeric(st_area(Pittsburgh_1900))

#repeated observations, keeping max size rep of ED
Pittsburgh_1900=st_drop_geometry(Pittsburgh_1900)
ind=vector(mode="character", length = length(unique(Pittsburgh_1900[which(Pittsburgh_1900$ED %in% (names(table(Pittsburgh_1900$ED)[which(as.numeric(table(Pittsburgh_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Pittsburgh_1900[which(Pittsburgh_1900$ED %in% (names(table(Pittsburgh_1900$ED)[which(as.numeric(table(Pittsburgh_1900$ED))>1)]))),"ED"]))){
  tmp=Pittsburgh_1900[which(Pittsburgh_1900$ED %in% unique(Pittsburgh_1900[which(Pittsburgh_1900$ED %in% (names(table(Pittsburgh_1900$ED)[which(as.numeric(table(Pittsburgh_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Pittsburgh_1900=st_read("input_data/step_2_enumeration_districts/1900/Pennsylvania/Pittsburgh")
map_area=sum(st_area(Pittsburgh_1900))
Pittsburgh_1900=st_make_valid(Pittsburgh_1900)
Pittsburgh_1900$area=as.numeric(st_area(Pittsburgh_1900))
Pittsburgh_1900=Pittsburgh_1900[which((row.names(Pittsburgh_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")
hex_grid=Pittsburgh_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Pittsburgh_1900$geometry)
Pittsburgh_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Pittsburgh_hexgrid))


#starting to intersect map and hex grid
Pittsburgh_1900=st_make_valid(Pittsburgh_1900)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
intersections_Pittsburgh_1900=st_intersection(Pittsburgh_1900, Pittsburgh_hexgrid)
intersections_Pittsburgh_1900$area_intersect=as.numeric(st_area(intersections_Pittsburgh_1900))
Pittsburgh_1900=st_drop_geometry(Pittsburgh_1900)
intersections_Pittsburgh_1900$ED_area=as.numeric(Pittsburgh_1900[match(intersections_Pittsburgh_1900$ED, Pittsburgh_1900$ED), "area"])
intersections_Pittsburgh_1900$proportion_intersected=intersections_Pittsburgh_1900$area_intersect/intersections_Pittsburgh_1900$ED_area

sum(intersections_Pittsburgh_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Pittsburgh_1900, file="intermediate_outputs/step_2_intersections/intersections_Pittsburgh_1900.rda")


#StLouis
library(sf)
StLouis_1900=st_read("input_data/step_2_enumeration_districts/1900/Missouri/St. Louis")
StLouis_1900=st_make_valid(StLouis_1900)
StLouis_1900$area=as.numeric(st_area(StLouis_1900))

#repeated observations, keeping max size rep of ED
StLouis_1900=st_drop_geometry(StLouis_1900)
ind=vector(mode="character", length = length(unique(StLouis_1900[which(StLouis_1900$ED %in% (names(table(StLouis_1900$ED)[which(as.numeric(table(StLouis_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(StLouis_1900[which(StLouis_1900$ED %in% (names(table(StLouis_1900$ED)[which(as.numeric(table(StLouis_1900$ED))>1)]))),"ED"]))){
  tmp=StLouis_1900[which(StLouis_1900$ED %in% unique(StLouis_1900[which(StLouis_1900$ED %in% (names(table(StLouis_1900$ED)[which(as.numeric(table(StLouis_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
StLouis_1900=st_read("input_data/step_2_enumeration_districts/1900/Missouri/St. Louis")
map_area=sum(st_area(StLouis_1900))
StLouis_1900=st_make_valid(StLouis_1900)
StLouis_1900$area=as.numeric(st_area(StLouis_1900))
StLouis_1900=StLouis_1900[which((row.names(StLouis_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")
hex_grid=StLouis_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, StLouis_1900$geometry)
StLouis_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(StLouis_hexgrid))


#starting to intersect map and hex grid
StLouis_1900=st_make_valid(StLouis_1900)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
intersections_StLouis_1900=st_intersection(StLouis_1900, StLouis_hexgrid)
intersections_StLouis_1900$area_intersect=as.numeric(st_area(intersections_StLouis_1900))
StLouis_1900=st_drop_geometry(StLouis_1900)
intersections_StLouis_1900$ED_area=as.numeric(StLouis_1900[match(intersections_StLouis_1900$ED, StLouis_1900$ED), "area"])
intersections_StLouis_1900$proportion_intersected=intersections_StLouis_1900$area_intersect/intersections_StLouis_1900$ED_area

sum(intersections_StLouis_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_StLouis_1900, file="intermediate_outputs/step_2_intersections/intersections_StLouis_1900.rda")


#Detroit
library(sf)
Detroit_1900=st_read("input_data/step_2_enumeration_districts/1900/Michigan/Detroit")
StLouis_1900=st_read("input_data/step_2_enumeration_districts/1900/Missouri/St. Louis")
Detroit_1900=st_transform(Detroit_1900, crs=st_crs(StLouis_1900))
Detroit_1900=st_make_valid(Detroit_1900)
Detroit_1900$area=as.numeric(st_area(Detroit_1900))

#repeated observations, keeping max size rep of ED
Detroit_1900=st_drop_geometry(Detroit_1900)
ind=vector(mode="character", length = length(unique(Detroit_1900[which(Detroit_1900$ED %in% (names(table(Detroit_1900$ED)[which(as.numeric(table(Detroit_1900$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Detroit_1900[which(Detroit_1900$ED %in% (names(table(Detroit_1900$ED)[which(as.numeric(table(Detroit_1900$ED))>1)]))),"ED"]))){
  tmp=Detroit_1900[which(Detroit_1900$ED %in% unique(Detroit_1900[which(Detroit_1900$ED %in% (names(table(Detroit_1900$ED)[which(as.numeric(table(Detroit_1900$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Detroit_1900=st_read("input_data/step_2_enumeration_districts/1900/Michigan/Detroit")
Detroit_1900=st_transform(Detroit_1900, crs=st_crs(StLouis_1900))
Detroit_1900=st_make_valid(Detroit_1900)
map_area=sum(st_area(Detroit_1900))
Detroit_1900=st_make_valid(Detroit_1900)
Detroit_1900$area=as.numeric(st_area(Detroit_1900))
Detroit_1900=Detroit_1900[which((row.names(Detroit_1900) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
hex_grid=Detroit_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1900$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Detroit_hexgrid))


#starting to intersect map and hex grid
Detroit_1900=st_make_valid(Detroit_1900)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
intersections_Detroit_1900=st_intersection(Detroit_1900, Detroit_hexgrid)
intersections_Detroit_1900$area_intersect=as.numeric(st_area(intersections_Detroit_1900))
Detroit_1900=st_drop_geometry(Detroit_1900)
intersections_Detroit_1900$ED_area=as.numeric(Detroit_1900[match(intersections_Detroit_1900$ED, Detroit_1900$ED), "area"])
intersections_Detroit_1900$proportion_intersected=intersections_Detroit_1900$area_intersect/intersections_Detroit_1900$ED_area

sum(intersections_Detroit_1900$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Detroit_1900, file="intermediate_outputs/step_2_intersections/intersections_Detroit_1900.rda")

#Detroit
library(sf)
Detroit_1910=st_read("input_data/step_2_enumeration_districts/1910/Michigan/Detroit")
StLouis_1910=st_read("input_data/step_2_enumeration_districts/1910/Missouri/")
Detroit_1910=st_transform(Detroit_1910, crs=st_crs(StLouis_1910))
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_1910$area=as.numeric(st_area(Detroit_1910))

#repeated observations, keeping max size rep of ED
Detroit_1910=st_drop_geometry(Detroit_1910)
ind=vector(mode="character", length = length(unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"]))){
  tmp=Detroit_1910[which(Detroit_1910$ED %in% unique(Detroit_1910[which(Detroit_1910$ED %in% (names(table(Detroit_1910$ED)[which(as.numeric(table(Detroit_1910$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Detroit_1910=st_read("input_data/step_2_enumeration_districts/1910/Michigan/Detroit")
Detroit_1910=st_transform(Detroit_1910, crs=st_crs(StLouis_1910))
Detroit_1910=st_make_valid(Detroit_1910)
map_area=sum(st_area(Detroit_1910))
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_1910$area=as.numeric(st_area(Detroit_1910))
Detroit_1910=Detroit_1910[which((row.names(Detroit_1910) %in% ind)==FALSE),]

load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
hex_grid=Detroit_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1910$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Detroit_hexgrid))


#starting to intersect map and hex grid
Detroit_1910=st_make_valid(Detroit_1910)
Detroit_hexgrid=st_make_valid(Detroit_hexgrid)
intersections_Detroit_1910=st_intersection(Detroit_1910, Detroit_hexgrid)
intersections_Detroit_1910$area_intersect=as.numeric(st_area(intersections_Detroit_1910))
Detroit_1910=st_drop_geometry(Detroit_1910)
intersections_Detroit_1910$ED_area=as.numeric(Detroit_1910[match(intersections_Detroit_1910$ED, Detroit_1910$ED), "area"])
intersections_Detroit_1910$proportion_intersected=intersections_Detroit_1910$area_intersect/intersections_Detroit_1910$ED_area

sum(intersections_Detroit_1910$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Detroit_1910, file="intermediate_outputs/step_2_intersections/intersections_Detroit_1910.rda")

#Detroit
library(sf)
Detroit_1920=st_read("input_data/step_2_enumeration_districts/1920/Michigan/Detroit")
StLouis_1920=st_read("input_data/step_2_enumeration_districts/1920/Missouri/St. Louis")
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

load("intermediate_outputs/step_2_hexgrids/Detroit_hexgrid.rda")
hex_grid=Detroit_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Detroit_1920$geometry)
Detroit_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
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


#1940
#Chicago
library(sf)
Chicago_1940=st_read("input_data/step_2_enumeration_districts/1940/ChicagoIL40", layer="ChicagoIL_ed40_aggr")
Chicago_1930=st_read("input_data/step_2_enumeration_districts/1930/Illinois/Chicago")
Chicago_1940=st_transform(Chicago_1940, crs=st_crs(Chicago_1930))
Chicago_1940=st_make_valid(Chicago_1940)
Chicago_1940$area=as.numeric(st_area(Chicago_1940))

#repeated observations, keeping max size rep of ED
Chicago_1940=st_drop_geometry(Chicago_1940)
ind=vector(mode="character", length = length(unique(Chicago_1940[which(Chicago_1940$ED %in% (names(table(Chicago_1940$ED)[which(as.numeric(table(Chicago_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Chicago_1940[which(Chicago_1940$ED %in% (names(table(Chicago_1940$ED)[which(as.numeric(table(Chicago_1940$ED))>1)]))),"ED"]))){
  tmp=Chicago_1940[which(Chicago_1940$ED %in% unique(Chicago_1940[which(Chicago_1940$ED %in% (names(table(Chicago_1940$ED)[which(as.numeric(table(Chicago_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Chicago_1940=st_read("input_data/step_2_enumeration_districts/1940/ChicagoIL40", layer="ChicagoIL_ed40_aggr")
Chicago_1940=st_transform(Chicago_1940, crs=st_crs(Chicago_1930))
Chicago_1940=st_make_valid(Chicago_1940)
map_area=sum(st_area(Chicago_1940))
Chicago_1940=st_make_valid(Chicago_1940)
Chicago_1940$area=as.numeric(st_area(Chicago_1940))
Chicago_1940=Chicago_1940[which((row.names(Chicago_1940) %in% ind)==FALSE),]
Chicago_1940$ED=Chicago_1940$ed

median_area <- median(Chicago_1940$area, na.rm = TRUE)
Chicago_1940_split <- split(Chicago_1940, Chicago_1940$ED)
Chicago_1940_filtered <- do.call(rbind, lapply(Chicago_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Chicago_1940_filtered$ED))  # Should return 0
Chicago_1940=Chicago_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
hex_grid=Chicago_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Chicago_1940$geometry)
Chicago_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Chicago_hexgrid))


#starting to intersect map and hex grid
Chicago_1940=st_make_valid(Chicago_1940)
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)
intersections_Chicago_1940=st_intersection(Chicago_1940, Chicago_hexgrid)
intersections_Chicago_1940$area_intersect=as.numeric(st_area(intersections_Chicago_1940))
Chicago_1940=st_drop_geometry(Chicago_1940)
intersections_Chicago_1940$ED_area=as.numeric(Chicago_1940[match(intersections_Chicago_1940$ED, Chicago_1940$ED), "area"])
intersections_Chicago_1940$proportion_intersected=intersections_Chicago_1940$area_intersect/intersections_Chicago_1940$ED_area

sum(intersections_Chicago_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Chicago_1940, file="intermediate_outputs/step_2_intersections/intersections_Chicago_1940.rda")

#Boston
library(sf)
Boston_1940=st_read("input_data/step_2_enumeration_districts/1940/BostonMA40", layer="BostonMA_ed40_aggr")
Boston_1930=st_read("input_data/step_2_enumeration_districts/1930/Massachusetts/Boston")
Boston_1940=st_transform(Boston_1940, crs=st_crs(Boston_1930))
Boston_1940=st_make_valid(Boston_1940)
Boston_1940$area=as.numeric(st_area(Boston_1940))

#repeated observations, keeping max size rep of ED
Boston_1940=st_drop_geometry(Boston_1940)
ind=vector(mode="character", length = length(unique(Boston_1940[which(Boston_1940$ED %in% (names(table(Boston_1940$ED)[which(as.numeric(table(Boston_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Boston_1940[which(Boston_1940$ED %in% (names(table(Boston_1940$ED)[which(as.numeric(table(Boston_1940$ED))>1)]))),"ED"]))){
  tmp=Boston_1940[which(Boston_1940$ED %in% unique(Boston_1940[which(Boston_1940$ED %in% (names(table(Boston_1940$ED)[which(as.numeric(table(Boston_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Boston_1940=st_read("input_data/step_2_enumeration_districts/1940/BostonMA40", layer="BostonMA_ed40_aggr")
Boston_1940=st_transform(Boston_1940, crs=st_crs(Boston_1930))
Boston_1940=st_make_valid(Boston_1940)
map_area=sum(st_area(Boston_1940))
Boston_1940=st_make_valid(Boston_1940)
Boston_1940$area=as.numeric(st_area(Boston_1940))
Boston_1940=Boston_1940[which((row.names(Boston_1940) %in% ind)==FALSE),]
Boston_1940$ED=Boston_1940$ed

median_area <- median(Boston_1940$area, na.rm = TRUE)
Boston_1940_split <- split(Boston_1940, Boston_1940$ED)
Boston_1940_filtered <- do.call(rbind, lapply(Boston_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Boston_1940_filtered$ED))  # Should return 0
Boston_1940=Boston_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Boston_hexgrid.rda")
hex_grid=Boston_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Boston_1940$geometry)
Boston_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Boston_hexgrid))


#starting to intersect map and hex grid
Boston_1940=st_make_valid(Boston_1940)
Boston_hexgrid=st_make_valid(Boston_hexgrid)
intersections_Boston_1940=st_intersection(Boston_1940, Boston_hexgrid)
intersections_Boston_1940$area_intersect=as.numeric(st_area(intersections_Boston_1940))
Boston_1940=st_drop_geometry(Boston_1940)
intersections_Boston_1940$ED_area=as.numeric(Boston_1940[match(intersections_Boston_1940$ED, Boston_1940$ED), "area"])
intersections_Boston_1940$proportion_intersected=intersections_Boston_1940$area_intersect/intersections_Boston_1940$ED_area

sum(intersections_Boston_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Boston_1940, file="intermediate_outputs/step_2_intersections/intersections_Boston_1940.rda")

#Baltimore
library(sf)
Baltimore_1940=st_read("input_data/step_2_enumeration_districts/1940/BaltimoreMD40", layer="BaltimoreMD_ed40_aggr")
Baltimore_1930=st_read("input_data/step_2_enumeration_districts/1930/Maryland/Baltimore")
Baltimore_1940=st_transform(Baltimore_1940, crs=st_crs(Baltimore_1930))
Baltimore_1940=st_make_valid(Baltimore_1940)
Baltimore_1940$area=as.numeric(st_area(Baltimore_1940))

#repeated observations, keeping max size rep of ED
Baltimore_1940=st_drop_geometry(Baltimore_1940)
ind=vector(mode="character", length = length(unique(Baltimore_1940[which(Baltimore_1940$ED %in% (names(table(Baltimore_1940$ED)[which(as.numeric(table(Baltimore_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Baltimore_1940[which(Baltimore_1940$ED %in% (names(table(Baltimore_1940$ED)[which(as.numeric(table(Baltimore_1940$ED))>1)]))),"ED"]))){
  tmp=Baltimore_1940[which(Baltimore_1940$ED %in% unique(Baltimore_1940[which(Baltimore_1940$ED %in% (names(table(Baltimore_1940$ED)[which(as.numeric(table(Baltimore_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Baltimore_1940=st_read("input_data/step_2_enumeration_districts/1940/BaltimoreMD40", layer="BaltimoreMD_ed40_aggr")
Baltimore_1940=st_transform(Baltimore_1940, crs=st_crs(Baltimore_1930))
Baltimore_1940=st_make_valid(Baltimore_1940)
map_area=sum(st_area(Baltimore_1940))
Baltimore_1940=st_make_valid(Baltimore_1940)
Baltimore_1940$area=as.numeric(st_area(Baltimore_1940))
Baltimore_1940=Baltimore_1940[which((row.names(Baltimore_1940) %in% ind)==FALSE),]
Baltimore_1940$ED=Baltimore_1940$ed

median_area <- median(Baltimore_1940$area, na.rm = TRUE)
Baltimore_1940_split <- split(Baltimore_1940, Baltimore_1940$ED)
Baltimore_1940_filtered <- do.call(rbind, lapply(Baltimore_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Baltimore_1940_filtered$ED))  # Should return 0
Baltimore_1940=Baltimore_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Baltimore_hexgrid.rda")
hex_grid=Baltimore_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Baltimore_1940$geometry)
Baltimore_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Baltimore_hexgrid))


#starting to intersect map and hex grid
Baltimore_1940=st_make_valid(Baltimore_1940)
Baltimore_hexgrid=st_make_valid(Baltimore_hexgrid)
intersections_Baltimore_1940=st_intersection(Baltimore_1940, Baltimore_hexgrid)
intersections_Baltimore_1940$area_intersect=as.numeric(st_area(intersections_Baltimore_1940))
Baltimore_1940=st_drop_geometry(Baltimore_1940)
intersections_Baltimore_1940$ED_area=as.numeric(Baltimore_1940[match(intersections_Baltimore_1940$ED, Baltimore_1940$ED), "area"])
intersections_Baltimore_1940$proportion_intersected=intersections_Baltimore_1940$area_intersect/intersections_Baltimore_1940$ED_area

sum(intersections_Baltimore_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Baltimore_1940, file="intermediate_outputs/step_2_intersections/intersections_Baltimore_1940.rda")


#Cincinnati
library(sf)
Cincinnati_1940=st_read("input_data/step_2_enumeration_districts/1940/CincinnatiOH40", layer="CincinnatiOH_ed40_aggr")
Cincinnati_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cincinnati")
Cincinnati_1940=st_transform(Cincinnati_1940, crs=st_crs(Cincinnati_1930))
Cincinnati_1940=st_make_valid(Cincinnati_1940)
Cincinnati_1940$area=as.numeric(st_area(Cincinnati_1940))

#repeated observations, keeping max size rep of ED
Cincinnati_1940=st_drop_geometry(Cincinnati_1940)
ind=vector(mode="character", length = length(unique(Cincinnati_1940[which(Cincinnati_1940$ED %in% (names(table(Cincinnati_1940$ED)[which(as.numeric(table(Cincinnati_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cincinnati_1940[which(Cincinnati_1940$ED %in% (names(table(Cincinnati_1940$ED)[which(as.numeric(table(Cincinnati_1940$ED))>1)]))),"ED"]))){
  tmp=Cincinnati_1940[which(Cincinnati_1940$ED %in% unique(Cincinnati_1940[which(Cincinnati_1940$ED %in% (names(table(Cincinnati_1940$ED)[which(as.numeric(table(Cincinnati_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cincinnati_1940=st_read("input_data/step_2_enumeration_districts/1940/CincinnatiOH40", layer="CincinnatiOH_ed40_aggr")
Cincinnati_1940=st_transform(Cincinnati_1940, crs=st_crs(Cincinnati_1930))
Cincinnati_1940=st_make_valid(Cincinnati_1940)
map_area=sum(st_area(Cincinnati_1940))
Cincinnati_1940=st_make_valid(Cincinnati_1940)
Cincinnati_1940$area=as.numeric(st_area(Cincinnati_1940))
Cincinnati_1940=Cincinnati_1940[which((row.names(Cincinnati_1940) %in% ind)==FALSE),]
Cincinnati_1940$ED=Cincinnati_1940$ed

median_area <- median(Cincinnati_1940$area, na.rm = TRUE)
Cincinnati_1940_split <- split(Cincinnati_1940, Cincinnati_1940$ED)
Cincinnati_1940_filtered <- do.call(rbind, lapply(Cincinnati_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Cincinnati_1940_filtered$ED))  # Should return 0
Cincinnati_1940=Cincinnati_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Cincinnati_hexgrid.rda")
hex_grid=Cincinnati_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cincinnati_1940$geometry)
Cincinnati_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cincinnati_hexgrid))


#starting to intersect map and hex grid
Cincinnati_1940=st_make_valid(Cincinnati_1940)
Cincinnati_hexgrid=st_make_valid(Cincinnati_hexgrid)
intersections_Cincinnati_1940=st_intersection(Cincinnati_1940, Cincinnati_hexgrid)
intersections_Cincinnati_1940$area_intersect=as.numeric(st_area(intersections_Cincinnati_1940))
Cincinnati_1940=st_drop_geometry(Cincinnati_1940)
intersections_Cincinnati_1940$ED_area=as.numeric(Cincinnati_1940[match(intersections_Cincinnati_1940$ED, Cincinnati_1940$ED), "area"])
intersections_Cincinnati_1940$proportion_intersected=intersections_Cincinnati_1940$area_intersect/intersections_Cincinnati_1940$ED_area

sum(intersections_Cincinnati_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cincinnati_1940, file="intermediate_outputs/step_2_intersections/intersections_Cincinnati_1940.rda")

#Philadelphia
library(sf)
Philadelphia_1940=st_read("input_data/step_2_enumeration_districts/1940/PhiladelphiaPA40", layer="PhiladelphiaPA_ed40_aggr")
Philadelphia_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Philadelphia")
Philadelphia_1940=st_transform(Philadelphia_1940, crs=st_crs(Philadelphia_1930))
Philadelphia_1940=st_make_valid(Philadelphia_1940)
Philadelphia_1940$area=as.numeric(st_area(Philadelphia_1940))

#repeated observations, keeping max size rep of ED
Philadelphia_1940=st_drop_geometry(Philadelphia_1940)
ind=vector(mode="character", length = length(unique(Philadelphia_1940[which(Philadelphia_1940$ED %in% (names(table(Philadelphia_1940$ED)[which(as.numeric(table(Philadelphia_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Philadelphia_1940[which(Philadelphia_1940$ED %in% (names(table(Philadelphia_1940$ED)[which(as.numeric(table(Philadelphia_1940$ED))>1)]))),"ED"]))){
  tmp=Philadelphia_1940[which(Philadelphia_1940$ED %in% unique(Philadelphia_1940[which(Philadelphia_1940$ED %in% (names(table(Philadelphia_1940$ED)[which(as.numeric(table(Philadelphia_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Philadelphia_1940=st_read("input_data/step_2_enumeration_districts/1940/PhiladelphiaPA40", layer="PhiladelphiaPA_ed40_aggr")
Philadelphia_1940=st_transform(Philadelphia_1940, crs=st_crs(Philadelphia_1930))
Philadelphia_1940=st_make_valid(Philadelphia_1940)
map_area=sum(st_area(Philadelphia_1940))
Philadelphia_1940=st_make_valid(Philadelphia_1940)
Philadelphia_1940$area=as.numeric(st_area(Philadelphia_1940))
Philadelphia_1940=Philadelphia_1940[which((row.names(Philadelphia_1940) %in% ind)==FALSE),]
Philadelphia_1940$ED=Philadelphia_1940$ed

median_area <- median(Philadelphia_1940$area, na.rm = TRUE)
Philadelphia_1940_split <- split(Philadelphia_1940, Philadelphia_1940$ED)
Philadelphia_1940_filtered <- do.call(rbind, lapply(Philadelphia_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Philadelphia_1940_filtered$ED))  # Should return 0
Philadelphia_1940=Philadelphia_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Philadelphia_hexgrid.rda")
hex_grid=Philadelphia_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Philadelphia_1940$geometry)
Philadelphia_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Philadelphia_hexgrid))


#starting to intersect map and hex grid
Philadelphia_1940=st_make_valid(Philadelphia_1940)
Philadelphia_hexgrid=st_make_valid(Philadelphia_hexgrid)
intersections_Philadelphia_1940=st_intersection(Philadelphia_1940, Philadelphia_hexgrid)
intersections_Philadelphia_1940$area_intersect=as.numeric(st_area(intersections_Philadelphia_1940))
Philadelphia_1940=st_drop_geometry(Philadelphia_1940)
intersections_Philadelphia_1940$ED_area=as.numeric(Philadelphia_1940[match(intersections_Philadelphia_1940$ED, Philadelphia_1940$ED), "area"])
intersections_Philadelphia_1940$proportion_intersected=intersections_Philadelphia_1940$area_intersect/intersections_Philadelphia_1940$ED_area

sum(intersections_Philadelphia_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Philadelphia_1940, file="intermediate_outputs/step_2_intersections/intersections_Philadelphia_1940.rda")

#StLouis
library(sf)
StLouis_1940=st_read("input_data/step_2_enumeration_districts/1940/StLouisMO40", layer="StLouisMO_ed40_aggr")
StLouis_1930=st_read("input_data/step_2_enumeration_districts/1930/Missouri/St. Louis")
StLouis_1940=st_transform(StLouis_1940, crs=st_crs(StLouis_1930))
StLouis_1940=st_make_valid(StLouis_1940)
StLouis_1940$area=as.numeric(st_area(StLouis_1940))

#repeated observations, keeping max size rep of ED
StLouis_1940=st_drop_geometry(StLouis_1940)
ind=vector(mode="character", length = length(unique(StLouis_1940[which(StLouis_1940$ED %in% (names(table(StLouis_1940$ED)[which(as.numeric(table(StLouis_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(StLouis_1940[which(StLouis_1940$ED %in% (names(table(StLouis_1940$ED)[which(as.numeric(table(StLouis_1940$ED))>1)]))),"ED"]))){
  tmp=StLouis_1940[which(StLouis_1940$ED %in% unique(StLouis_1940[which(StLouis_1940$ED %in% (names(table(StLouis_1940$ED)[which(as.numeric(table(StLouis_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
StLouis_1940=st_read("input_data/step_2_enumeration_districts/1940/StLouisMO40", layer="StLouisMO_ed40_aggr")
StLouis_1940=st_transform(StLouis_1940, crs=st_crs(StLouis_1930))
StLouis_1940=st_make_valid(StLouis_1940)
map_area=sum(st_area(StLouis_1940))
StLouis_1940=st_make_valid(StLouis_1940)
StLouis_1940$area=as.numeric(st_area(StLouis_1940))
StLouis_1940=StLouis_1940[which((row.names(StLouis_1940) %in% ind)==FALSE),]
StLouis_1940$ED=StLouis_1940$ed


load("intermediate_outputs/step_2_hexgrids/StLouis_hexgrid.rda")
hex_grid=StLouis_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, StLouis_1940$geometry)
StLouis_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(StLouis_hexgrid))

median_area <- median(StLouis_1940$area, na.rm = TRUE)
StLouis_1940_split <- split(StLouis_1940, StLouis_1940$ED)
StLouis_1940_filtered <- do.call(rbind, lapply(StLouis_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(StLouis_1940_filtered$ED))  # Should return 0
StLouis_1940=StLouis_1940_filtered

#starting to intersect map and hex grid
StLouis_1940=st_make_valid(StLouis_1940)
StLouis_hexgrid=st_make_valid(StLouis_hexgrid)
intersections_StLouis_1940=st_intersection(StLouis_1940, StLouis_hexgrid)
intersections_StLouis_1940$area_intersect=as.numeric(st_area(intersections_StLouis_1940))
StLouis_1940=st_drop_geometry(StLouis_1940)
intersections_StLouis_1940$ED_area=as.numeric(StLouis_1940[match(intersections_StLouis_1940$ED, StLouis_1940$ED), "area"])
intersections_StLouis_1940$proportion_intersected=intersections_StLouis_1940$area_intersect/intersections_StLouis_1940$ED_area

sum(intersections_StLouis_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_StLouis_1940, file="intermediate_outputs/step_2_intersections/intersections_StLouis_1940.rda")

#PROBLEM ONES
#Pittsburgh
library(sf)
Pittsburgh_1940=st_read("input_data/step_2_enumeration_districts/1940/PittsburghPA40", layer="PittsburghPA_ed40_aggr")
Pittsburgh_1930=st_read("input_data/step_2_enumeration_districts/1930/Pennsylvania/Pittsburgh")
Pittsburgh_1940=st_transform(Pittsburgh_1940, crs=st_crs(Pittsburgh_1930))
Pittsburgh_1940=st_make_valid(Pittsburgh_1940)
Pittsburgh_1940$area=as.numeric(st_area(Pittsburgh_1940))

#repeated observations, keeping max size rep of ED
Pittsburgh_1940=st_drop_geometry(Pittsburgh_1940)
ind=vector(mode="character", length = length(unique(Pittsburgh_1940[which(Pittsburgh_1940$ED %in% (names(table(Pittsburgh_1940$ED)[which(as.numeric(table(Pittsburgh_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Pittsburgh_1940[which(Pittsburgh_1940$ED %in% (names(table(Pittsburgh_1940$ED)[which(as.numeric(table(Pittsburgh_1940$ED))>1)]))),"ED"]))){
  tmp=Pittsburgh_1940[which(Pittsburgh_1940$ED %in% unique(Pittsburgh_1940[which(Pittsburgh_1940$ED %in% (names(table(Pittsburgh_1940$ED)[which(as.numeric(table(Pittsburgh_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Pittsburgh_1940=st_read("input_data/step_2_enumeration_districts/1940/PittsburghPA40", layer="PittsburghPA_ed40_aggr")
Pittsburgh_1940=st_transform(Pittsburgh_1940, crs=st_crs(Pittsburgh_1930))
Pittsburgh_1940=st_make_valid(Pittsburgh_1940)
map_area=sum(st_area(Pittsburgh_1940))
Pittsburgh_1940=st_make_valid(Pittsburgh_1940)
Pittsburgh_1940$area=as.numeric(st_area(Pittsburgh_1940))
Pittsburgh_1940=Pittsburgh_1940[which((row.names(Pittsburgh_1940) %in% ind)==FALSE),]
Pittsburgh_1940$ED=Pittsburgh_1940$ed

median_area <- median(Pittsburgh_1940$area, na.rm = TRUE)
Pittsburgh_1940_split <- split(Pittsburgh_1940, Pittsburgh_1940$ED)
Pittsburgh_1940_filtered <- do.call(rbind, lapply(Pittsburgh_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Pittsburgh_1940_filtered$ED))  # Should return 0
Pittsburgh_1940=Pittsburgh_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Pittsburgh_hexgrid.rda")
hex_grid=Pittsburgh_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Pittsburgh_1940$geometry)
Pittsburgh_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Pittsburgh_hexgrid))


#starting to intersect map and hex grid
Pittsburgh_1940=st_make_valid(Pittsburgh_1940)
Pittsburgh_hexgrid=st_make_valid(Pittsburgh_hexgrid)
intersections_Pittsburgh_1940=st_intersection(Pittsburgh_1940, Pittsburgh_hexgrid)
intersections_Pittsburgh_1940$area_intersect=as.numeric(st_area(intersections_Pittsburgh_1940))
Pittsburgh_1940=st_drop_geometry(Pittsburgh_1940)
intersections_Pittsburgh_1940$ED_area=as.numeric(Pittsburgh_1940[match(intersections_Pittsburgh_1940$ED, Pittsburgh_1940$ED), "area"])
intersections_Pittsburgh_1940$proportion_intersected=intersections_Pittsburgh_1940$area_intersect/intersections_Pittsburgh_1940$ED_area

sum(intersections_Pittsburgh_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Pittsburgh_1940, file="intermediate_outputs/step_2_intersections/intersections_Pittsburgh_1940.rda")



#Cleveland
library(sf)
Cleveland_1940=st_read("input_data/step_2_enumeration_districts/1940/ClevelandOH40", layer="ClevelandOH_ed40_aggr")
Cleveland_1930=st_read("input_data/step_2_enumeration_districts/1930/Ohio/Cleveland")
Cleveland_1940=st_transform(Cleveland_1940, crs=st_crs(Cleveland_1930))
Cleveland_1940=st_make_valid(Cleveland_1940)
Cleveland_1940$area=as.numeric(st_area(Cleveland_1940))

#repeated observations, keeping max size rep of ED
Cleveland_1940=st_drop_geometry(Cleveland_1940)
ind=vector(mode="character", length = length(unique(Cleveland_1940[which(Cleveland_1940$ED %in% (names(table(Cleveland_1940$ED)[which(as.numeric(table(Cleveland_1940$ED))>1)]))),"ED"])))
for (i in 1:length(unique(Cleveland_1940[which(Cleveland_1940$ED %in% (names(table(Cleveland_1940$ED)[which(as.numeric(table(Cleveland_1940$ED))>1)]))),"ED"]))){
  tmp=Cleveland_1940[which(Cleveland_1940$ED %in% unique(Cleveland_1940[which(Cleveland_1940$ED %in% (names(table(Cleveland_1940$ED)[which(as.numeric(table(Cleveland_1940$ED))>1)]))),"ED"])[i]),]
  ind[i]=ifelse(nrow(tmp)>0, row.names(tmp[which(tmp$area==min(tmp$area)),]), 99999999)
}
Cleveland_1940=st_read("input_data/step_2_enumeration_districts/1940/ClevelandOH40", layer="ClevelandOH_ed40_aggr")
Cleveland_1940=st_transform(Cleveland_1940, crs=st_crs(Cleveland_1930))
Cleveland_1940=st_make_valid(Cleveland_1940)
map_area=sum(st_area(Cleveland_1940))
Cleveland_1940=st_make_valid(Cleveland_1940)
Cleveland_1940$area=as.numeric(st_area(Cleveland_1940))
Cleveland_1940=Cleveland_1940[which((row.names(Cleveland_1940) %in% ind)==FALSE),]
Cleveland_1940$ED=Cleveland_1940$ed

median_area <- median(Cleveland_1940$area, na.rm = TRUE)
Cleveland_1940_split <- split(Cleveland_1940, Cleveland_1940$ED)
Cleveland_1940_filtered <- do.call(rbind, lapply(Cleveland_1940_split, function(subset) {
  subset[which.min(abs(subset$area - median_area)), , drop = FALSE]
}))
sum(duplicated(Cleveland_1940_filtered$ED))  # Should return 0
Cleveland_1940=Cleveland_1940_filtered

load("intermediate_outputs/step_2_hexgrids/Cleveland_hexgrid.rda")
hex_grid=Cleveland_hexgrid
hex_grid=st_as_sf(hex_grid)
hex_grid=st_make_valid(hex_grid)

#limiting hexgrid to area of city (interesecting)
indicies=st_intersects(hex_grid$x, Cleveland_1940$geometry)
Cleveland_hexgrid=hex_grid[unlist(lapply(indicies, length))>0,]
hex_area=sum(st_area(Cleveland_hexgrid))


#starting to intersect map and hex grid
Cleveland_1940=st_make_valid(Cleveland_1940)
Cleveland_hexgrid=st_make_valid(Cleveland_hexgrid)
intersections_Cleveland_1940=st_intersection(Cleveland_1940, Cleveland_hexgrid)
intersections_Cleveland_1940$area_intersect=as.numeric(st_area(intersections_Cleveland_1940))
Cleveland_1940=st_drop_geometry(Cleveland_1940)
intersections_Cleveland_1940$ED_area=as.numeric(Cleveland_1940[match(intersections_Cleveland_1940$ED, Cleveland_1940$ED), "area"])
intersections_Cleveland_1940$proportion_intersected=intersections_Cleveland_1940$area_intersect/intersections_Cleveland_1940$ED_area

sum(intersections_Cleveland_1940$proportion_intersected) #should equal number rows of enumberation district

save(intersections_Cleveland_1940, file="intermediate_outputs/step_2_intersections/intersections_Cleveland_1940.rda")

