###########################################################################################################
######## STEP 3F: CONSTRUCTION OF COMBINED PANEL DATASET FOR FIRST STAGE ANALYSIS #########################
########          HEXMATS GENERATED IN PREVIOUS STEP ARE TAKEN AS INPUTS          #########################
###########################################################################################################

##### LOADING HEXAGON MATRICES  AND COMBINING

files=list.files(
  path="intermediate_outputs/step_3_hex_mats/",
  pattern=".rda"
)

files=paste0("intermediate_outputs/step_3_hex_mats/",files)

for (i in 1:length(files)){
  load(files[i])
}

dataframe_names=ls(
  pattern="^hex_mat"
)

dataframe_list=mget(
  dataframe_names,
  envir=.GlobalEnv
)


combined_df=do.call(
  rbind,
  dataframe_list
)

combined_df$city_hex=paste0(
  combined_df$city,
  "_",
  combined_df$hex_id
)

combined_df$post=ifelse(
  combined_df$year>1920,
  1,
  0
)


####### DEFINING MORI/TREATMENT SET

fixed_mori_set=unique(
  combined_df[
    which(
      (combined_df$year==1910|combined_df$year==1900|combined_df$year==1920) &
        combined_df$any_mori==1
    ),
    "city_hex"
  ]
)

    ####### 667 mori hexagons, fixing mori set to 1920 or earlier presence

combined_df$fixed_mori_set=ifelse(combined_df$city_hex %in% fixed_mori_set, 1, 0)

combined_df$post_mori=combined_df$post*combined_df$fixed_mori_set


########## NEIGHBORHOOD CHARACTERISTICS


########## 1900

combined_df$ethnic_frag_1900=0
combined_df$italian_prop_1900=0
combined_df$foreign_prop_1900=0

mat_1900=matrix(
  nrow=length(
    unique(
      combined_df$city_hex
    )
  ),
  ncol=4
)

mat_1900[,1]=unique(
  combined_df$city_hex
)

for (i in 1:nrow(mat_1900)){
  
  tmp=combined_df[
    which(
      combined_df$city_hex==mat_1900[i,1] &
        combined_df$year==1900
    ),
  ]
  
  if (nrow(tmp)>0){
    
    mat_1900[i,2]=tmp$ethnic_frag
    mat_1900[i,3]=tmp$ital_prop
    mat_1900[i,4]=tmp$foreign_prop
    
  }
  
}

combined_df$ethnic_frag_1900=as.numeric(
  mat_1900[
    match(
      combined_df$city_hex,
      mat_1900[,1]
    ),
    2
  ]
)

combined_df$italian_prop_1900=as.numeric(
  mat_1900[
    match(
      combined_df$city_hex,
      mat_1900[,1]
    ),
    3
  ]
)

combined_df$foreign_prop_1900=as.numeric(
  mat_1900[
    match(
      combined_df$city_hex,
      mat_1900[,1]
    ),
    4
  ]
)


########## 1910

combined_df$ethnic_frag_1910=0
combined_df$italian_prop_1910=0
combined_df$foreign_prop_1910=0

mat_1910=matrix(
  nrow=length(
    unique(
      combined_df$city_hex
    )
  ),
  ncol=4
)

mat_1910[,1]=unique(
  combined_df$city_hex
)

for (i in 1:nrow(mat_1910)){
  
  tmp=combined_df[
    which(
      combined_df$city_hex==mat_1910[i,1] &
        combined_df$year==1910
    ),
  ]
  
  if (nrow(tmp)>0){
    
    mat_1910[i,2]=tmp$ethnic_frag
    mat_1910[i,3]=tmp$ital_prop
    mat_1910[i,4]=tmp$foreign_prop
    
  }
  
}

combined_df$ethnic_frag_1910=as.numeric(
  mat_1910[
    match(
      combined_df$city_hex,
      mat_1910[,1]
    ),
    2
  ]
)

combined_df$italian_prop_1910=as.numeric(
  mat_1910[
    match(
      combined_df$city_hex,
      mat_1910[,1]
    ),
    3
  ]
)

combined_df$foreign_prop_1910=as.numeric(
  mat_1910[
    match(
      combined_df$city_hex,
      mat_1910[,1]
    ),
    4
  ]
)


########## 1920

combined_df$ethnic_frag_1920=0
combined_df$italian_prop_1920=0
combined_df$foreign_prop_1920=0

mat_1920=matrix(
  nrow=length(
    unique(
      combined_df$city_hex
    )
  ),
  ncol=4
)

mat_1920[,1]=unique(
  combined_df$city_hex
)

for (i in 1:nrow(mat_1920)){
  
  tmp=combined_df[
    which(
      combined_df$city_hex==mat_1920[i,1] &
        combined_df$year==1920
    ),
  ]
  
  if (nrow(tmp)>0){
    
    mat_1920[i,2]=tmp$ethnic_frag
    mat_1920[i,3]=tmp$ital_prop
    mat_1920[i,4]=tmp$foreign_prop
    
  }
  
}

combined_df$ethnic_frag_1920=as.numeric(
  mat_1920[
    match(
      combined_df$city_hex,
      mat_1920[,1]
    ),
    2
  ]
)

combined_df$italian_prop_1920=as.numeric(
  mat_1920[
    match(
      combined_df$city_hex,
      mat_1920[,1]
    ),
    3
  ]
)

combined_df$foreign_prop_1920=as.numeric(
  mat_1920[
    match(
      combined_df$city_hex,
      mat_1920[,1]
    ),
    4
  ]
)


###### INVCAERCATION RATES

combined_df$incarc_abe_per_10k=
  combined_df$incarc_abe_prop*
  10000

combined_df$incarc_cs_per_10k=
  combined_df$incarc_cs_prop*
  10000


############################################################
######## SICILIAN PRESENCE DEFINITIONS ######################
############################################################


############################################################
######## ALL SICILIANS ######################################
############################################################

sicilians_1900=unique(
  combined_df[
    which(
      combined_df$year==1900 &
        combined_df$any_sicilians==1
    ),
    "city_hex"
  ]
)

sicilians_1910=unique(
  append(
    sicilians_1900,
    unique(
      combined_df[
        which(
          combined_df$year==1910 &
            combined_df$any_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

sicilians_1920=unique(
  append(
    sicilians_1910,
    unique(
      combined_df[
        which(
          combined_df$year==1920 &
            combined_df$any_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

combined_df$any_sicilians_1900=ifelse(
  combined_df$city_hex %in% sicilians_1900,
  1,
  0
)

combined_df$any_sicilians_1910=ifelse(
  combined_df$city_hex %in% sicilians_1910,
  1,
  0
)

combined_df$any_sicilians_1920=ifelse(
  combined_df$city_hex %in% sicilians_1920,
  1,
  0
)


############################################################
######## CUTRERA 1900 MAP ###################################
############################################################

cutrera_1900=unique(
  combined_df[
    which(
      combined_df$year==1900 &
        combined_df$any_cutrera_1900_sicilians==1
    ),
    "city_hex"
  ]
)

cutrera_1910=unique(
  append(
    cutrera_1900,
    unique(
      combined_df[
        which(
          combined_df$year==1910 &
            combined_df$any_cutrera_1900_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

cutrera_1920=unique(
  append(
    cutrera_1910,
    unique(
      combined_df[
        which(
          combined_df$year==1920 &
            combined_df$any_cutrera_1900_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

combined_df$any_cutrera_1900_sicilians_1900=ifelse(
  combined_df$city_hex %in% cutrera_1900,
  1,
  0
)

combined_df$any_cutrera_1900_sicilians_1910=ifelse(
  combined_df$city_hex %in% cutrera_1910,
  1,
  0
)

combined_df$any_cutrera_1900_sicilians_1920=ifelse(
  combined_df$city_hex %in% cutrera_1920,
  1,
  0
)


############################################################
######## DAMIANI 1885 MAP ###################################
############################################################

damiani_1900=unique(
  combined_df[
    which(
      combined_df$year==1900 &
        combined_df$any_damiani_1885_sicilians==1
    ),
    "city_hex"
  ]
)

damiani_1910=unique(
  append(
    damiani_1900,
    unique(
      combined_df[
        which(
          combined_df$year==1910 &
            combined_df$any_damiani_1885_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

damiani_1920=unique(
  append(
    damiani_1910,
    unique(
      combined_df[
        which(
          combined_df$year==1920 &
            combined_df$any_damiani_1885_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

combined_df$any_damiani_1885_sicilians_1900=ifelse(
  combined_df$city_hex %in% damiani_1900,
  1,
  0
)

combined_df$any_damiani_1885_sicilians_1910=ifelse(
  combined_df$city_hex %in% damiani_1910,
  1,
  0
)

combined_df$any_damiani_1885_sicilians_1920=ifelse(
  combined_df$city_hex %in% damiani_1920,
  1,
  0
)


############################################################
######## CUTRERA OR DAMIANI MAPS ############################
############################################################

cutrera_or_damiani_1900=unique(
  combined_df[
    which(
      combined_df$year==1900 &
        combined_df$any_cutrera_or_damiani_maps_sicilians==1
    ),
    "city_hex"
  ]
)

cutrera_or_damiani_1910=unique(
  append(
    cutrera_or_damiani_1900,
    unique(
      combined_df[
        which(
          combined_df$year==1910 &
            combined_df$any_cutrera_or_damiani_maps_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

cutrera_or_damiani_1920=unique(
  append(
    cutrera_or_damiani_1910,
    unique(
      combined_df[
        which(
          combined_df$year==1920 &
            combined_df$any_cutrera_or_damiani_maps_sicilians==1
        ),
        "city_hex"
      ]
    )
  )
)

combined_df$any_cutrera_or_damiani_maps_sicilians_1900=ifelse(
  combined_df$city_hex %in% cutrera_or_damiani_1900,
  1,
  0
)

combined_df$any_cutrera_or_damiani_maps_sicilians_1910=ifelse(
  combined_df$city_hex %in% cutrera_or_damiani_1910,
  1,
  0
)

combined_df$any_cutrera_or_damiani_maps_sicilians_1920=ifelse(
  combined_df$city_hex %in% cutrera_or_damiani_1920,
  1,
  0
)

combined_df$fixed_cutrera_or_damiani_set=ifelse(
  combined_df$any_cutrera_or_damiani_maps_sicilians_1900==1 |
    combined_df$any_cutrera_or_damiani_maps_sicilians_1910==1 |
    combined_df$any_cutrera_or_damiani_maps_sicilians_1920==1,
  1,
  0
)
############################################################
######## SAVE ################################################
############################################################


save(
  combined_df,
  file="analysis_data/combined_df.rda"
)