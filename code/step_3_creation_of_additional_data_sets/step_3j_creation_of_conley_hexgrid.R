########################################################################################################
################# STEP 3J: CREATION OF SPATIAL HEXGFRIS FOR CONLEY REGRESSIONS   #######################
########################################################################################################

files=list.files("intermediate_outputs/step_2_hexgrids", pattern = ".rda", full.names = TRUE)
tmp_list=list()
for (i in 1:length(files)){
  hexgrid=load(files[i])
  city=sub("_hexgrid$","",hexgrid)
  df=get(hexgrid)
  df$city_hex=paste0(city, "_", df$hex_id)
  tmp_list[[i]]=df
}
hexgrid=do.call(rbind.data.frame, tmp_list)
hexgrid=st_as_sf(hexgrid)
hexgrid=sf::st_transform(hexgrid, 4326)
hexgrid$centroid=sf::st_centroid(hexgrid$x)
coords=sf::st_coordinates(hexgrid$centroid)
hexgrid=sf::st_drop_geometry(hexgrid)
hexgrid$lon=coords[,1]
hexgrid$lat=coords[,2]
save(hexgrid, file="intermediate_outputs/hexgrid_conley.rda")

