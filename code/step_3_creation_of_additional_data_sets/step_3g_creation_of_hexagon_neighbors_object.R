########################################################################################################
################# STEP 3G: CONSTRUCTION OF HEXAGON NEIGHBORS OBJECT   ##################################
########################################################################################################

library(spdep)

files=list.files("intermediate_outputs/step_2_hexgrids")
files=paste0("intermediate_outputs/step_2_hexgrids/",files)
for(file in files){
  load(file)
}

load("analysis_data/combined_df.rda")

###### COMBINING HEXGRIDS INTO SINGLE OBJECT
objs <- mget(ls(pattern = "_hexgrid$"))
objs <- lapply(names(objs), function(nm) {
  obj <- objs[[nm]]
  prefix <- sub("_hexgrid$", "", nm) # e.g. "A" from "A_hexgrid"
  obj$hex_id <- paste0(prefix, "_", obj$hex_id)
  obj
})
combined <- do.call(rbind, objs)

##### COMPUTING FIRST AND SECOND DEGREE NEIGHBORS
nb=poly2nb(combined)
nb_matrix=nb2mat(nb, style="B")
mori_nb=nb_matrix[which(combined$hex_id %in% unique(combined_df[which(combined_df$fixed_mori_set==1),"city_hex"])),]
vec=vector(mode="numeric", length=ncol(mori_nb))
for (j in 1:ncol(mori_nb)){
  vec[j]=sum(mori_nb[,j])
}
neighbors=combined[which(vec>0), "hex_id"]
neighbors=st_drop_geometry(neighbors)
neighbors=neighbors[which((neighbors$hex_id %in% unique(combined_df[which(combined_df$fixed_mori_set==1),"city_hex"]))==FALSE),]
neighbors_2nd_degree_nb=nb_matrix[which(combined$hex_id %in% unique(neighbors)),]
vec=vector(mode="numeric", length=ncol(neighbors_2nd_degree_nb))
for (j in 1:ncol(neighbors_2nd_degree_nb)){
  vec[j]=sum(neighbors_2nd_degree_nb[,j])
}
neighbors2=combined[which(vec>0), "hex_id"]
neighbors2=st_drop_geometry(neighbors2)
neighbors2=neighbors2[which((neighbors2$hex_id %in% unique(combined_df[which(combined_df$fixed_mori_set==1),"city_hex"]))==FALSE),]
neighbors2=neighbors2[which((neighbors2 %in% unique(neighbors))==FALSE)]
neighbors_1=neighbors
neighbors=unique(append(neighbors_1, neighbors2))
neighbors=neighbors2

combined_df$mori_neighbor=ifelse(combined_df$city_hex %in% neighbors, 1, 0)
combined_df$post_mori_neighbor=combined_df$mori_neighbor*combined_df$post
save(combined_df, file="analysis_data/combined_df_with_neighbors.rda")
save(neighbors, file="intermediate_outputs/neighbors.rda")
