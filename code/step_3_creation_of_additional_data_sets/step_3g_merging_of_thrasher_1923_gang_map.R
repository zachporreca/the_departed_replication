###########################################################################################################
######## STEP 3G: MERGING OF THRASHER (1923-26) CHICAGO GANG MAP INTO THE COMBINED PANEL ##################
########          RUN AFTER STEP 3F, WITH combined_df IN MEMORY (OR LOADED FROM analysis_data)    #########
###########################################################################################################

#######################################################################
###########   Source: F. M. Thrasher, "Chicago's Gangland"    #########
###########   (1923-26), Library of Congress g4104c.ct003854. #########
###########   1,158 gang symbols digitized from the LOC       #########
###########   master TIFF; 6 enclosures the map labels        #########
###########   ITALIAN traced by hand. Chicago only: all       #########
###########   other cities receive NA, not zero.              #########
#######################################################################

library(sf)

thrasher=read.csv("external_data/thrasher_1923/thrasher_1923_gang_symbols.csv")
italian_areas=st_read("external_data/thrasher_1923/thrasher_1923_italian_areas.geojson", quiet=TRUE)

load("intermediate_outputs/step_2_hexgrids/Chicago_hexgrid.rda")
Chicago_hexgrid=st_make_valid(Chicago_hexgrid)

#points to the hexgrid CRS, assign hex_id by containment
thrasher_sf=st_as_sf(thrasher, coords=c("longitude","latitude"), crs=4326, remove=FALSE)
thrasher_sf=st_transform(thrasher_sf, crs=st_crs(Chicago_hexgrid))
ind=st_intersects(thrasher_sf, Chicago_hexgrid)
thrasher$hex_id=unlist(lapply(ind, function(z) if(length(z)==0) NA else Chicago_hexgrid$hex_id[z[1]]))

print(sum(is.na(thrasher$hex_id))) #symbols outside the Chicago hexgrid

#hex-level counts
count_by_hex=function(ids){ as.numeric(table(factor(ids, levels=Chicago_hexgrid$hex_id))) }
tmp=thrasher[which(is.na(thrasher$hex_id)==FALSE),]

thrasher_hex=data.frame(hex_id=Chicago_hexgrid$hex_id)
thrasher_hex$thrasher_gangs_1923=count_by_hex(tmp$hex_id)
thrasher_hex$thrasher_italian_area_gangs_1923=count_by_hex(tmp[which(tmp$in_italian_area==1),"hex_id"])
thrasher_hex$thrasher_italian_area_gangs_1923_core=count_by_hex(tmp[which(tmp$in_italian_area==1 & tmp$near_boundary==0),"hex_id"])
thrasher_hex$any_thrasher_italian_area_gang_1923=ifelse(thrasher_hex$thrasher_italian_area_gangs_1923>0, 1, 0)

#share of each hexagon covered by an Italian-labelled enclosure
italian_areas=st_transform(st_make_valid(italian_areas), crs=st_crs(Chicago_hexgrid))
italian_union=st_union(italian_areas)
hex_area=as.numeric(st_area(Chicago_hexgrid))
overlap=st_intersection(Chicago_hexgrid, italian_union)
overlap_area=rep(0, nrow(Chicago_hexgrid))
if (nrow(overlap)>0) {
  overlap_area[match(overlap$hex_id, Chicago_hexgrid$hex_id)]=as.numeric(st_area(overlap))
}
thrasher_hex$italian_area_share_1923=overlap_area/hex_area

print(sum(thrasher_hex$thrasher_gangs_1923)) #should equal nrow(tmp)

#merge on city_hex, constant across census years; NA outside Chicago
thrasher_hex$city_hex=paste0("Chicago_", thrasher_hex$hex_id)
thrasher_vars=c("thrasher_gangs_1923","thrasher_italian_area_gangs_1923","thrasher_italian_area_gangs_1923_core",
                "any_thrasher_italian_area_gang_1923","italian_area_share_1923")
for (v in thrasher_vars){
  combined_df[,v]=thrasher_hex[match(combined_df$city_hex, thrasher_hex$city_hex), v]
}
#Chicago hexes with no symbol get 0, not NA
for (v in thrasher_vars){
  combined_df[which(combined_df$city=="Chicago" & is.na(combined_df[,v])), v]=0
}

print(table(combined_df$any_thrasher_italian_area_gang_1923[which(combined_df$year==1920)], useNA="ifany"))

save(combined_df, file="analysis_data/combined_df.rda")
