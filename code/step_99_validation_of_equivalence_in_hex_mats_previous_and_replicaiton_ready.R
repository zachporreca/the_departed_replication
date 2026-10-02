############################################################
######## VALIDATE NEW VS OLD BALTIMORE 1910 HEX MAT ########
############################################################


############################################
# 1. LOAD NEW AND OLD VERSIONS
############################################

# New version
load("intermediate_outputs/step_3_hex_mats/hex_mat_Baltimore_1910.rda")
hex_mat_Baltimore_1910_new=hex_mat_Baltimore_1910

# Old version
load("/home/zach/Dropbox/corrected_intersections/hex_mats/hex_mat_Baltimore_1910.rda")
hex_mat_Baltimore_1910_old=hex_mat_Baltimore_1910

# Remove generic object to avoid ambiguity
rm(hex_mat_Baltimore_1910)


############################################
# 2. BASIC STRUCTURE
############################################

cat("\n")
cat("============================================\n")
cat("BASIC STRUCTURE\n")
cat("============================================\n")

cat("\nNEW dimensions:\n")
print(dim(hex_mat_Baltimore_1910_new))

cat("\nOLD dimensions:\n")
print(dim(hex_mat_Baltimore_1910_old))

cat("\nNumber of NEW columns:\n")
print(ncol(hex_mat_Baltimore_1910_new))

cat("\nNumber of OLD columns:\n")
print(ncol(hex_mat_Baltimore_1910_old))

cat("\nColumns only in NEW:\n")
print(
  setdiff(
    names(hex_mat_Baltimore_1910_new),
    names(hex_mat_Baltimore_1910_old)
  )
)

cat("\nColumns only in OLD:\n")
print(
  setdiff(
    names(hex_mat_Baltimore_1910_old),
    names(hex_mat_Baltimore_1910_new)
  )
)

common_cols=intersect(
  names(hex_mat_Baltimore_1910_new),
  names(hex_mat_Baltimore_1910_old)
)

cat("\nNumber of common columns:\n")
print(length(common_cols))

cat("\nCommon columns:\n")
print(common_cols)


############################################
# 3. HEX-ID VALIDATION
############################################

cat("\n")
cat("============================================\n")
cat("HEX-ID VALIDATION\n")
cat("============================================\n")

cat("\nDuplicate hex IDs in NEW:\n")
print(sum(duplicated(hex_mat_Baltimore_1910_new$hex_id)))

cat("\nDuplicate hex IDs in OLD:\n")
print(sum(duplicated(hex_mat_Baltimore_1910_old$hex_id)))

cat("\nHex IDs only in NEW:\n")
hex_only_new=setdiff(
  hex_mat_Baltimore_1910_new$hex_id,
  hex_mat_Baltimore_1910_old$hex_id
)
print(hex_only_new)

cat("\nHex IDs only in OLD:\n")
hex_only_old=setdiff(
  hex_mat_Baltimore_1910_old$hex_id,
  hex_mat_Baltimore_1910_new$hex_id
)
print(hex_only_old)

cat("\nSame set of hex IDs:\n")
print(
  length(hex_only_new)==0 &
    length(hex_only_old)==0
)

cat("\nSame hex-ID ordering before alignment:\n")
print(
  identical(
    as.character(hex_mat_Baltimore_1910_new$hex_id),
    as.character(hex_mat_Baltimore_1910_old$hex_id)
  )
)


############################################
# 4. CHECK DUPLICATES BEFORE MATCHING
############################################

if (
  anyDuplicated(hex_mat_Baltimore_1910_new$hex_id)>0 |
  anyDuplicated(hex_mat_Baltimore_1910_old$hex_id)>0
){
  stop("Duplicate hex_id values detected. Resolve before value comparison.")
}


############################################
# 5. ALIGN OLD AND NEW BY HEX_ID
############################################

common_hex=intersect(
  hex_mat_Baltimore_1910_new$hex_id,
  hex_mat_Baltimore_1910_old$hex_id
)

hex_mat_Baltimore_1910_new_compare=
  hex_mat_Baltimore_1910_new[
    match(
      common_hex,
      hex_mat_Baltimore_1910_new$hex_id
    ),
  ]

hex_mat_Baltimore_1910_old_compare=
  hex_mat_Baltimore_1910_old[
    match(
      common_hex,
      hex_mat_Baltimore_1910_old$hex_id
    ),
  ]

stopifnot(
  all(
    as.character(hex_mat_Baltimore_1910_new_compare$hex_id)==
      as.character(hex_mat_Baltimore_1910_old_compare$hex_id)
  )
)

cat("\nNumber of common hexagons compared:\n")
print(length(common_hex))


############################################
# 6. HELPER: IDENTIFY NUMERIC-LIKE COLUMNS
############################################

is_numeric_like=function(x){
  
  if (is.numeric(x) | is.integer(x)){
    return(TRUE)
  }
  
  x_char=as.character(x)
  
  observed=!is.na(x_char) & x_char!=""
  
  if (sum(observed)==0){
    return(FALSE)
  }
  
  x_numeric=suppressWarnings(
    as.numeric(x_char[observed])
  )
  
  return(all(!is.na(x_numeric)))
}


############################################
# 7. COMPARE EVERY COMMON COLUMN
############################################

# Exact differences are reported, but this tolerance also identifies
# whether numerical differences are merely floating-point noise.

tolerance=1e-10

validation=data.frame(
  variable=common_cols,
  old_class=NA_character_,
  new_class=NA_character_,
  old_missing=NA_integer_,
  new_missing=NA_integer_,
  missing_mismatch=NA_integer_,
  n_different_exact=NA_integer_,
  n_different_tolerance=NA_integer_,
  max_abs_difference=NA_real_,
  stringsAsFactors=FALSE
)


for (j in seq_along(common_cols)){
  
  var=common_cols[j]
  
  old_value=
    hex_mat_Baltimore_1910_old_compare[[var]]
  
  new_value=
    hex_mat_Baltimore_1910_new_compare[[var]]
  
  validation$old_class[j]=class(old_value)[1]
  validation$new_class[j]=class(new_value)[1]
  
  validation$old_missing[j]=sum(is.na(old_value))
  validation$new_missing[j]=sum(is.na(new_value))
  
  validation$missing_mismatch[j]=sum(
    xor(
      is.na(old_value),
      is.na(new_value)
    )
  )
  
  
  ##################################
  # NUMERIC / NUMERIC-LIKE COLUMNS
  ##################################
  
  if (
    is_numeric_like(old_value) &
    is_numeric_like(new_value)
  ){
    
    old_numeric=suppressWarnings(
      as.numeric(as.character(old_value))
    )
    
    new_numeric=suppressWarnings(
      as.numeric(as.character(new_value))
    )
    
    both_observed=
      !is.na(old_numeric) &
      !is.na(new_numeric)
    
    missing_mismatch=
      xor(
        is.na(old_numeric),
        is.na(new_numeric)
      )
    
    exact_difference=rep(FALSE, length(old_numeric))
    tolerance_difference=rep(FALSE, length(old_numeric))
    
    exact_difference[missing_mismatch]=TRUE
    tolerance_difference[missing_mismatch]=TRUE
    
    exact_difference[both_observed]=
      old_numeric[both_observed] !=
      new_numeric[both_observed]
    
    tolerance_difference[both_observed]=
      abs(
        old_numeric[both_observed] -
          new_numeric[both_observed]
      ) > tolerance
    
    validation$n_different_exact[j]=
      sum(exact_difference)
    
    validation$n_different_tolerance[j]=
      sum(tolerance_difference)
    
    if (sum(both_observed)>0){
      
      validation$max_abs_difference[j]=
        max(
          abs(
            old_numeric[both_observed] -
              new_numeric[both_observed]
          )
        )
      
    }
    
    
    ##################################
    # CHARACTER / OTHER COLUMNS
    ##################################
    
  } else {
    
    old_character=as.character(old_value)
    new_character=as.character(new_value)
    
    same=
      (
        is.na(old_character) &
          is.na(new_character)
      ) |
      (
        !is.na(old_character) &
          !is.na(new_character) &
          old_character==new_character
      )
    
    validation$n_different_exact[j]=sum(!same)
    
    validation$n_different_tolerance[j]=
      validation$n_different_exact[j]
    
  }
  
}


############################################
# 8. FULL COLUMN-BY-COLUMN RESULTS
############################################

cat("\n")
cat("============================================\n")
cat("ALL COMMON COLUMN COMPARISONS\n")
cat("============================================\n")

print(
  validation[
    order(
      -validation$n_different_tolerance,
      validation$variable
    ),
  ],
  row.names=FALSE
)


############################################
# 9. COLUMNS WITH ANY DIFFERENCES
############################################

changed=
  validation[
    validation$n_different_tolerance>0,
  ]

cat("\n")
cat("============================================\n")
cat("COMMON COLUMNS WITH VALUE DIFFERENCES\n")
cat("============================================\n")

if (nrow(changed)==0){
  
  cat("\nNo differences in any common column.\n")
  
} else {
  
  print(
    changed[
      order(
        -changed$n_different_tolerance,
        changed$variable
      ),
    ],
    row.names=FALSE
  )
  
}


############################################
# 10. COLUMNS WITH IDENTICAL VALUES
############################################

unchanged=
  validation[
    validation$n_different_tolerance==0,
  ]

cat("\n")
cat("============================================\n")
cat("COMMON COLUMNS WITH IDENTICAL VALUES\n")
cat("============================================\n")

print(
  unchanged$variable
)


############################################
# 11. CLASS DIFFERENCES
############################################

class_differences=
  validation[
    validation$old_class != validation$new_class,
  ]

cat("\n")
cat("============================================\n")
cat("COLUMNS WITH CLASS DIFFERENCES\n")
cat("============================================\n")

if (nrow(class_differences)==0){
  
  cat("\nNo class differences.\n")
  
} else {
  
  print(
    class_differences[
      ,
      c(
        "variable",
        "old_class",
        "new_class"
      )
    ],
    row.names=FALSE
  )
  
}


############################################
# 12. MISSING-VALUE DIFFERENCES
############################################

missing_differences=
  validation[
    validation$old_missing != validation$new_missing |
      validation$missing_mismatch>0,
  ]

cat("\n")
cat("============================================\n")
cat("COLUMNS WITH MISSING-VALUE DIFFERENCES\n")
cat("============================================\n")

if (nrow(missing_differences)==0){
  
  cat("\nNo missing-value differences.\n")
  
} else {
  
  print(
    missing_differences[
      ,
      c(
        "variable",
        "old_missing",
        "new_missing",
        "missing_mismatch"
      )
    ],
    row.names=FALSE
  )
  
}


############################################
# 13. DETAILED ROW-LEVEL DIFFERENCES
############################################

cat("\n")
cat("============================================\n")
cat("ROW-LEVEL DIFFERENCES\n")
cat("============================================\n")

if (nrow(changed)>0){
  
  for (var in changed$variable){
    
    old_value=
      hex_mat_Baltimore_1910_old_compare[[var]]
    
    new_value=
      hex_mat_Baltimore_1910_new_compare[[var]]
    
    
    if (
      is_numeric_like(old_value) &
      is_numeric_like(new_value)
    ){
      
      old_numeric=suppressWarnings(
        as.numeric(as.character(old_value))
      )
      
      new_numeric=suppressWarnings(
        as.numeric(as.character(new_value))
      )
      
      different=
        xor(
          is.na(old_numeric),
          is.na(new_numeric)
        ) |
        (
          !is.na(old_numeric) &
            !is.na(new_numeric) &
            abs(old_numeric-new_numeric)>tolerance
        )
      
      difference_table=data.frame(
        hex_id=
          hex_mat_Baltimore_1910_new_compare$hex_id[different],
        old=old_numeric[different],
        new=new_numeric[different],
        difference=
          new_numeric[different] -
          old_numeric[different]
      )
      
    } else {
      
      old_character=as.character(old_value)
      new_character=as.character(new_value)
      
      same=
        (
          is.na(old_character) &
            is.na(new_character)
        ) |
        (
          !is.na(old_character) &
            !is.na(new_character) &
            old_character==new_character
        )
      
      different=!same
      
      difference_table=data.frame(
        hex_id=
          hex_mat_Baltimore_1910_new_compare$hex_id[different],
        old=old_character[different],
        new=new_character[different]
      )
      
    }
    
    
    cat("\n\n--------------------------------------------\n")
    cat("VARIABLE:", var, "\n")
    cat("Number different:", nrow(difference_table), "\n")
    cat("--------------------------------------------\n")
    
    # Print first 50 differences to avoid flooding console
    print(
      head(difference_table, 50),
      row.names=FALSE
    )
    
    if (nrow(difference_table)>50){
      cat(
        "\nOnly first 50 of",
        nrow(difference_table),
        "differences shown.\n"
      )
    }
    
  }
  
}


############################################
# 14. SUMMARY STATISTICS FOR CHANGED
#     NUMERIC COLUMNS
############################################

cat("\n")
cat("============================================\n")
cat("SUMMARY STATISTICS FOR CHANGED COLUMNS\n")
cat("============================================\n")

if (nrow(changed)>0){
  
  for (var in changed$variable){
    
    old_value=
      hex_mat_Baltimore_1910_old_compare[[var]]
    
    new_value=
      hex_mat_Baltimore_1910_new_compare[[var]]
    
    if (
      is_numeric_like(old_value) &
      is_numeric_like(new_value)
    ){
      
      old_numeric=suppressWarnings(
        as.numeric(as.character(old_value))
      )
      
      new_numeric=suppressWarnings(
        as.numeric(as.character(new_value))
      )
      
      cat("\n\nVARIABLE:", var, "\n")
      
      cat("\nOLD summary:\n")
      print(summary(old_numeric))
      
      cat("\nNEW summary:\n")
      print(summary(new_numeric))
      
      cat("\nOLD sum:\n")
      print(sum(old_numeric, na.rm=TRUE))
      
      cat("\nNEW sum:\n")
      print(sum(new_numeric, na.rm=TRUE))
      
    }
    
  }
  
}


############################################
# 15. INSPECT VARIABLES ONLY IN NEW
############################################

new_only=
  setdiff(
    names(hex_mat_Baltimore_1910_new),
    names(hex_mat_Baltimore_1910_old)
  )

cat("\n")
cat("============================================\n")
cat("SUMMARY OF VARIABLES ONLY IN NEW\n")
cat("============================================\n")

if (length(new_only)==0){
  
  cat("\nNo variables only in NEW.\n")
  
} else {
  
  for (var in new_only){
    
    cat("\n\nVARIABLE:", var, "\n")
    print(summary(hex_mat_Baltimore_1910_new[[var]]))
    
    cat("Missing:", sum(is.na(hex_mat_Baltimore_1910_new[[var]])), "\n")
    
  }
  
}


############################################
# 16. FINAL VALIDATION SUMMARY
############################################

cat("\n")
cat("============================================\n")
cat("FINAL VALIDATION SUMMARY\n")
cat("============================================\n")

cat(
  "\nNew rows:",
  nrow(hex_mat_Baltimore_1910_new),
  "\n"
)

cat(
  "Old rows:",
  nrow(hex_mat_Baltimore_1910_old),
  "\n"
)

cat(
  "Hex IDs only in new:",
  length(hex_only_new),
  "\n"
)

cat(
  "Hex IDs only in old:",
  length(hex_only_old),
  "\n"
)

cat(
  "Common columns:",
  length(common_cols),
  "\n"
)

cat(
  "Common columns with differences:",
  nrow(changed),
  "\n"
)

cat(
  "Common columns identical:",
  nrow(unchanged),
  "\n"
)

cat(
  "Columns only in new:",
  length(new_only),
  "\n"
)

cat(
  "Columns only in old:",
  length(
    setdiff(
      names(hex_mat_Baltimore_1910_old),
      names(hex_mat_Baltimore_1910_new)
    )
  ),
  "\n"
)


###################all cities
############################################################
######## QUICK VALIDATION: ALL 1910 HEX MATS ################
############################################################

new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

files=list.files(
  new_dir,
  pattern="hex_mat_.*_1910\\.rda$",
  full.names=FALSE
)

# Variables whose differences have already been accepted / are not
# relevant to paper results
ignore_vars=c(
  "foreign",
  "foreign_prop",
  "pop_mlp",
  "incarc_mlp",
  "incarc_mlp_prop",
  "household_count",
  "occ_score",
  "sei",
  "owned",
  "rent"
)

tolerance=1e-10

validation_summary=data.frame(
  file=files,
  new_rows=NA_integer_,
  old_rows=NA_integer_,
  same_rows=NA,
  same_hex_ids=NA,
  same_hex_order=NA,
  n_common_cols=NA_integer_,
  n_checked_cols=NA_integer_,
  n_changed_checked_cols=NA_integer_,
  changed_checked_cols=NA_character_,
  stringsAsFactors=FALSE
)


for (f in files){
  
  cat("\n")
  cat("====================================================\n")
  cat("VALIDATING:", f, "\n")
  cat("====================================================\n")
  
  ############################################
  # LOAD NEW
  ############################################
  
  new_object_name=load(file.path(new_dir,f))
  new=get(new_object_name[1])
  
  rm(list=new_object_name)
  
  
  ############################################
  # LOAD OLD
  ############################################
  
  old_object_name=load(file.path(old_dir,f))
  old=get(old_object_name[1])
  
  rm(list=old_object_name)
  
  
  ############################################
  # BASIC STRUCTURE
  ############################################
  
  row=which(validation_summary$file==f)
  
  validation_summary$new_rows[row]=nrow(new)
  validation_summary$old_rows[row]=nrow(old)
  
  validation_summary$same_rows[row]=
    nrow(new)==nrow(old)
  
  
  ############################################
  # HEX IDS
  ############################################
  
  new_hex=as.character(new$hex_id)
  old_hex=as.character(old$hex_id)
  
  validation_summary$same_hex_ids[row]=
    setequal(new_hex,old_hex)
  
  validation_summary$same_hex_order[row]=
    identical(new_hex,old_hex)
  
  
  ############################################
  # ALIGN BY HEX ID
  ############################################
  
  common_hex=intersect(new_hex,old_hex)
  
  new_compare=
    new[
      match(common_hex,as.character(new$hex_id)),
    ]
  
  old_compare=
    old[
      match(common_hex,as.character(old$hex_id)),
    ]
  
  
  ############################################
  # COMMON VARIABLES
  ############################################
  
  common_cols=intersect(
    names(new_compare),
    names(old_compare)
  )
  
  validation_summary$n_common_cols[row]=
    length(common_cols)
  
  check_cols=setdiff(
    common_cols,
    ignore_vars
  )
  
  validation_summary$n_checked_cols[row]=
    length(check_cols)
  
  
  ############################################
  # COMPARE VALUES
  ############################################
  
  changed_cols=c()
  
  for (var in check_cols){
    
    x=new_compare[[var]]
    y=old_compare[[var]]
    
    # Numeric or numeric-like
    x_num=suppressWarnings(
      as.numeric(as.character(x))
    )
    
    y_num=suppressWarnings(
      as.numeric(as.character(y))
    )
    
    numeric_like=
      (
        is.numeric(x) |
          all(is.na(x) | !is.na(x_num))
      ) &
      (
        is.numeric(y) |
          all(is.na(y) | !is.na(y_num))
      )
    
    
    if (numeric_like){
      
      missing_difference=
        xor(
          is.na(x_num),
          is.na(y_num)
        )
      
      value_difference=
        !is.na(x_num) &
        !is.na(y_num) &
        abs(x_num-y_num)>tolerance
      
      if (
        any(missing_difference) |
        any(value_difference)
      ){
        changed_cols=c(changed_cols,var)
      }
      
    } else {
      
      x_char=as.character(x)
      y_char=as.character(y)
      
      same=
        (
          is.na(x_char) &
            is.na(y_char)
        ) |
        (
          !is.na(x_char) &
            !is.na(y_char) &
            x_char==y_char
        )
      
      if (any(!same)){
        changed_cols=c(changed_cols,var)
      }
      
    }
    
  }
  
  
  validation_summary$n_changed_checked_cols[row]=
    length(changed_cols)
  
  validation_summary$changed_checked_cols[row]=
    ifelse(
      length(changed_cols)==0,
      "",
      paste(changed_cols,collapse=", ")
    )
  
  
  ############################################
  # PRINT CITY RESULT
  ############################################
  
  cat("Rows identical:", validation_summary$same_rows[row], "\n")
  cat("Hex IDs identical:", validation_summary$same_hex_ids[row], "\n")
  cat("Hex order identical:", validation_summary$same_hex_order[row], "\n")
  cat("Checked common columns:", length(check_cols), "\n")
  cat("Changed checked columns:", length(changed_cols), "\n")
  
  if (length(changed_cols)>0){
    cat(
      "Unexpected changed columns:",
      paste(changed_cols,collapse=", "),
      "\n"
    )
  } else {
    cat("RESULT: PASS\n")
  }
  
}


############################################################
######## FINAL SUMMARY ######################################
############################################################

cat("\n\n")
cat("====================================================\n")
cat("1910 VALIDATION SUMMARY\n")
cat("====================================================\n")

print(
  validation_summary,
  row.names=FALSE
)


############################################################
######## CITIES REQUIRING ATTENTION #########################
############################################################

problems=validation_summary[
  !validation_summary$same_rows |
    !validation_summary$same_hex_ids |
    validation_summary$n_changed_checked_cols>0,
]

cat("\n")
cat("====================================================\n")
cat("CITIES REQUIRING ATTENTION\n")
cat("====================================================\n")

if (nrow(problems)==0){
  
  cat("\nALL 1910 CITIES PASS.\n")
  
} else {
  
  print(
    problems,
    row.names=FALSE
  )
  
}




############################################################
######## QUICK VALIDATION: ALL 1920 HEX MATS ################
############################################################

new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

files=list.files(
  new_dir,
  pattern="hex_mat_.*_1920\\.rda$",
  full.names=FALSE
)

# Variables whose differences have already been accepted / are not
# relevant to paper results
ignore_vars=c(
  "foreign",
  "foreign_prop",
  "pop_mlp",
  "incarc_mlp",
  "incarc_mlp_prop",
  "household_count",
  "occ_score",
  "sei",
  "owned",
  "rent"
)

tolerance=1e-10

validation_summary=data.frame(
  file=files,
  new_rows=NA_integer_,
  old_rows=NA_integer_,
  same_rows=NA,
  same_hex_ids=NA,
  same_hex_order=NA,
  n_common_cols=NA_integer_,
  n_checked_cols=NA_integer_,
  n_changed_checked_cols=NA_integer_,
  changed_checked_cols=NA_character_,
  stringsAsFactors=FALSE
)


for (f in files){
  
  cat("\n")
  cat("====================================================\n")
  cat("VALIDATING:", f, "\n")
  cat("====================================================\n")
  
  ############################################
  # LOAD NEW
  ############################################
  
  new_object_name=load(file.path(new_dir,f))
  new=get(new_object_name[1])
  
  rm(list=new_object_name)
  
  
  ############################################
  # LOAD OLD
  ############################################
  
  old_object_name=load(file.path(old_dir,f))
  old=get(old_object_name[1])
  
  rm(list=old_object_name)
  
  
  ############################################
  # BASIC STRUCTURE
  ############################################
  
  row=which(validation_summary$file==f)
  
  validation_summary$new_rows[row]=nrow(new)
  validation_summary$old_rows[row]=nrow(old)
  
  validation_summary$same_rows[row]=
    nrow(new)==nrow(old)
  
  
  ############################################
  # HEX IDS
  ############################################
  
  new_hex=as.character(new$hex_id)
  old_hex=as.character(old$hex_id)
  
  validation_summary$same_hex_ids[row]=
    setequal(new_hex,old_hex)
  
  validation_summary$same_hex_order[row]=
    identical(new_hex,old_hex)
  
  
  ############################################
  # ALIGN BY HEX ID
  ############################################
  
  common_hex=intersect(new_hex,old_hex)
  
  new_compare=
    new[
      match(common_hex,as.character(new$hex_id)),
    ]
  
  old_compare=
    old[
      match(common_hex,as.character(old$hex_id)),
    ]
  
  
  ############################################
  # COMMON VARIABLES
  ############################################
  
  common_cols=intersect(
    names(new_compare),
    names(old_compare)
  )
  
  validation_summary$n_common_cols[row]=
    length(common_cols)
  
  check_cols=setdiff(
    common_cols,
    ignore_vars
  )
  
  validation_summary$n_checked_cols[row]=
    length(check_cols)
  
  
  ############################################
  # COMPARE VALUES
  ############################################
  
  changed_cols=c()
  
  for (var in check_cols){
    
    x=new_compare[[var]]
    y=old_compare[[var]]
    
    x_num=suppressWarnings(
      as.numeric(as.character(x))
    )
    
    y_num=suppressWarnings(
      as.numeric(as.character(y))
    )
    
    numeric_like=
      (
        is.numeric(x) |
          all(is.na(x) | !is.na(x_num))
      ) &
      (
        is.numeric(y) |
          all(is.na(y) | !is.na(y_num))
      )
    
    
    if (numeric_like){
      
      missing_difference=
        xor(
          is.na(x_num),
          is.na(y_num)
        )
      
      value_difference=
        !is.na(x_num) &
        !is.na(y_num) &
        abs(x_num-y_num)>tolerance
      
      if (
        any(missing_difference) |
        any(value_difference)
      ){
        changed_cols=c(changed_cols,var)
      }
      
    } else {
      
      x_char=as.character(x)
      y_char=as.character(y)
      
      same=
        (
          is.na(x_char) &
            is.na(y_char)
        ) |
        (
          !is.na(x_char) &
            !is.na(y_char) &
            x_char==y_char
        )
      
      if (any(!same)){
        changed_cols=c(changed_cols,var)
      }
      
    }
    
  }
  
  
  validation_summary$n_changed_checked_cols[row]=
    length(changed_cols)
  
  validation_summary$changed_checked_cols[row]=
    ifelse(
      length(changed_cols)==0,
      "",
      paste(changed_cols,collapse=", ")
    )
  
  
  ############################################
  # PRINT CITY RESULT
  ############################################
  
  cat("Rows identical:", validation_summary$same_rows[row], "\n")
  cat("Hex IDs identical:", validation_summary$same_hex_ids[row], "\n")
  cat("Hex order identical:", validation_summary$same_hex_order[row], "\n")
  cat("Checked common columns:", length(check_cols), "\n")
  cat("Changed checked columns:", length(changed_cols), "\n")
  
  if (length(changed_cols)>0){
    cat(
      "Unexpected changed columns:",
      paste(changed_cols,collapse=", "),
      "\n"
    )
  } else {
    cat("RESULT: PASS\n")
  }
  
}


############################################################
######## FINAL SUMMARY ######################################
############################################################

cat("\n\n")
cat("====================================================\n")
cat("1920 VALIDATION SUMMARY\n")
cat("====================================================\n")

print(
  validation_summary,
  row.names=FALSE
)


############################################################
######## CITIES REQUIRING ATTENTION #########################
############################################################

problems=validation_summary[
  !validation_summary$same_rows |
    !validation_summary$same_hex_ids |
    validation_summary$n_changed_checked_cols>0,
]

cat("\n")
cat("====================================================\n")
cat("CITIES REQUIRING ATTENTION\n")
cat("====================================================\n")

if (nrow(problems)==0){
  
  cat("\nALL 1920 CITIES PASS.\n")
  
} else {
  
  print(
    problems,
    row.names=FALSE
  )
  
}

############################################################
######## QUICK DIAGNOSTIC: 1920 CHANGED VARIABLES ###########
############################################################

new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

files=list.files(
  new_dir,
  pattern="hex_mat_.*_1920\\.rda$",
  full.names=FALSE
)

vars_to_check=c(
  "ethnic_frag",
  "mori",
  "non_mori_sicilians",
  "any_mori"
)

diagnostic=data.frame()

for (f in files){
  
  new_object_name=load(file.path(new_dir,f))
  new=get(new_object_name[1])
  rm(list=new_object_name)
  
  old_object_name=load(file.path(old_dir,f))
  old=get(old_object_name[1])
  rm(list=old_object_name)
  
  old=old[
    match(
      new$hex_id,
      old$hex_id
    ),
  ]
  
  for (var in vars_to_check){
    
    if (
      var %in% names(new) &
      var %in% names(old)
    ){
      
      new_value=as.numeric(new[[var]])
      old_value=as.numeric(old[[var]])
      
      different=
        xor(is.na(new_value),is.na(old_value)) |
        (
          !is.na(new_value) &
            !is.na(old_value) &
            abs(new_value-old_value)>1e-10
        )
      
      diagnostic=rbind(
        diagnostic,
        data.frame(
          file=f,
          variable=var,
          n_different=sum(different),
          max_abs_difference=max(
            abs(new_value-old_value),
            na.rm=TRUE
          ),
          old_sum=sum(old_value,na.rm=TRUE),
          new_sum=sum(new_value,na.rm=TRUE),
          stringsAsFactors=FALSE
        )
      )
      
    }
    
  }
  
}

diagnostic=diagnostic[
  diagnostic$n_different>0,
]

print(
  diagnostic,
  row.names=FALSE
)


new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

cities=c("Chicago","Manhattan")

for (city in cities){
  
  f=paste0("hex_mat_",city,"_1920.rda")
  
  load(file.path(new_dir,f))
  new=get(paste0("hex_mat_",city,"_1920"))
  
  load(file.path(old_dir,f))
  old=get(paste0("hex_mat_",city,"_1920"))
  
  old=old[
    match(new$hex_id,old$hex_id),
  ]
  
  changed=which(
    new$any_mori != old$any_mori
  )
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  print(
    data.frame(
      hex_id=new$hex_id[changed],
      old_any_mori=old$any_mori[changed],
      new_any_mori=new$any_mori[changed],
      old_mori=old$mori[changed],
      new_mori=new$mori[changed],
      old_non_mori=old$non_mori_sicilians[changed],
      new_non_mori=new$non_mori_sicilians[changed]
    ),
    row.names=FALSE
  )
  
}






new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

files=list.files(
  new_dir,
  pattern="hex_mat_.*_1930\\.rda$",
  full.names=FALSE
)

ignore_vars=c(
  "foreign",
  "foreign_prop",
  "pop_mlp",
  "incarc_mlp",
  "incarc_mlp_prop",
  "household_count",
  "occ_score",
  "sei",
  "owned",
  "owned_free",
  "rent"
)

tolerance=1e-10

validation_summary=data.frame(
  file=files,
  new_rows=NA_integer_,
  old_rows=NA_integer_,
  same_rows=NA,
  same_hex_ids=NA,
  same_hex_order=NA,
  n_common_cols=NA_integer_,
  n_checked_cols=NA_integer_,
  n_changed_checked_cols=NA_integer_,
  changed_checked_cols=NA_character_,
  stringsAsFactors=FALSE
)

for (f in files){
  
  new_object_name=load(file.path(new_dir,f))
  new=get(new_object_name[1])
  rm(list=new_object_name)
  
  old_object_name=load(file.path(old_dir,f))
  old=get(old_object_name[1])
  rm(list=old_object_name)
  
  row=which(validation_summary$file==f)
  
  validation_summary$new_rows[row]=nrow(new)
  validation_summary$old_rows[row]=nrow(old)
  validation_summary$same_rows[row]=nrow(new)==nrow(old)
  
  new_hex=as.character(new$hex_id)
  old_hex=as.character(old$hex_id)
  
  validation_summary$same_hex_ids[row]=setequal(new_hex,old_hex)
  validation_summary$same_hex_order[row]=identical(new_hex,old_hex)
  
  common_hex=intersect(new_hex,old_hex)
  
  new_compare=new[
    match(common_hex,as.character(new$hex_id)),
  ]
  
  old_compare=old[
    match(common_hex,as.character(old$hex_id)),
  ]
  
  common_cols=intersect(
    names(new_compare),
    names(old_compare)
  )
  
  validation_summary$n_common_cols[row]=length(common_cols)
  
  check_cols=setdiff(
    common_cols,
    ignore_vars
  )
  
  validation_summary$n_checked_cols[row]=length(check_cols)
  
  changed_cols=c()
  
  for (var in check_cols){
    
    x=new_compare[[var]]
    y=old_compare[[var]]
    
    x_num=suppressWarnings(as.numeric(as.character(x)))
    y_num=suppressWarnings(as.numeric(as.character(y)))
    
    numeric_like=
      (
        is.numeric(x) |
          all(is.na(x) | !is.na(x_num))
      ) &
      (
        is.numeric(y) |
          all(is.na(y) | !is.na(y_num))
      )
    
    if (numeric_like){
      
      missing_difference=xor(
        is.na(x_num),
        is.na(y_num)
      )
      
      value_difference=
        !is.na(x_num) &
        !is.na(y_num) &
        abs(x_num-y_num)>tolerance
      
      if (
        any(missing_difference) |
        any(value_difference)
      ){
        changed_cols=c(changed_cols,var)
      }
      
    } else {
      
      x_char=as.character(x)
      y_char=as.character(y)
      
      same=
        (is.na(x_char) & is.na(y_char)) |
        (
          !is.na(x_char) &
            !is.na(y_char) &
            x_char==y_char
        )
      
      if (any(!same)){
        changed_cols=c(changed_cols,var)
      }
      
    }
    
  }
  
  validation_summary$n_changed_checked_cols[row]=length(changed_cols)
  
  validation_summary$changed_checked_cols[row]=
    ifelse(
      length(changed_cols)==0,
      "",
      paste(changed_cols,collapse=", ")
    )
  
  cat("\n====================================================\n")
  cat("VALIDATING:",f,"\n")
  cat("====================================================\n")
  cat("Rows identical:",validation_summary$same_rows[row],"\n")
  cat("Hex IDs identical:",validation_summary$same_hex_ids[row],"\n")
  cat("Hex order identical:",validation_summary$same_hex_order[row],"\n")
  cat("Checked common columns:",length(check_cols),"\n")
  cat("Changed checked columns:",length(changed_cols),"\n")
  
  if (length(changed_cols)>0){
    cat("Unexpected changed columns:",paste(changed_cols,collapse=", "),"\n")
  }
  
}

cat("\n\n")
cat("====================================================\n")
cat("1930 VALIDATION SUMMARY\n")
cat("====================================================\n")

print(
  validation_summary,
  row.names=FALSE
)

problems=validation_summary[
  !validation_summary$same_rows |
    !validation_summary$same_hex_ids |
    validation_summary$n_changed_checked_cols>0,
]

cat("\n====================================================\n")
cat("CITIES REQUIRING ATTENTION\n")
cat("====================================================\n")

if (nrow(problems)==0){
  
  cat("\nALL 1930 CITIES PASS.\n")
  
} else {
  
  print(
    problems,
    row.names=FALSE
  )
  
}


new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"

cities=c(
  "Brooklyn",
  "Chicago",
  "Pittsburgh",
  "StLouis"
)

for (city in cities){
  
  f=paste0("hex_mat_",city,"_1930.rda")
  
  new_object_name=load(file.path(new_dir,f))
  new=get(new_object_name[1])
  rm(list=new_object_name)
  
  old_object_name=load(file.path(old_dir,f))
  old=get(old_object_name[1])
  rm(list=old_object_name)
  
  old=old[
    match(new$hex_id,old$hex_id),
  ]
  
  changed=which(
    new$any_mori != old$any_mori
  )
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  print(
    data.frame(
      hex_id=new$hex_id[changed],
      old_any_mori=old$any_mori[changed],
      new_any_mori=new$any_mori[changed],
      old_mori=old$mori[changed],
      new_mori=new$mori[changed],
      old_non_mori=old$non_mori_sicilians[changed],
      new_non_mori=new$non_mori_sicilians[changed]
    ),
    row.names=FALSE
  )
  
}












new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"

files_1900=list.files(
  new_dir,
  pattern="hex_mat_.*_1900\\.rda$",
  full.names=FALSE
)

incarceration_vars=c(
  "pop_mlp",
  "pop_abe",
  "pop_cs",
  "incarc_mlp",
  "incarc_abe",
  "incarc_cs",
  "incarc_mlp_prop",
  "incarc_abe_prop",
  "incarc_cs_prop"
)

validation=data.frame(
  city=character(),
  cols_1900=integer(),
  cols_1910=integer(),
  same_columns=logical(),
  same_column_order=logical(),
  compatible_classes=logical(),
  incarceration_all_na=logical(),
  duplicate_hex_ids=integer(),
  year_correct=logical(),
  stringsAsFactors=FALSE
)

for (f1900 in files_1900){
  
  city=sub(
    "^hex_mat_(.*)_1900\\.rda$",
    "\\1",
    f1900
  )
  
  f1910=paste0(
    "hex_mat_",
    city,
    "_1910.rda"
  )
  
  obj1900_name=load(
    file.path(new_dir,f1900)
  )
  x1900=get(obj1900_name[1])
  rm(list=obj1900_name)
  
  obj1910_name=load(
    file.path(new_dir,f1910)
  )
  x1910=get(obj1910_name[1])
  rm(list=obj1910_name)
  
  same_columns=setequal(
    names(x1900),
    names(x1910)
  )
  
  same_column_order=identical(
    names(x1900),
    names(x1910)
  )
  
  compatible_classes=FALSE
  
  if (same_column_order){
    
    compatible_classes=TRUE
    
    for (var in names(x1900)){
      
      class1900=class(x1900[[var]])[1]
      class1910=class(x1910[[var]])[1]
      
      numeric1900=class1900 %in% c(
        "numeric",
        "integer"
      )
      
      numeric1910=class1910 %in% c(
        "numeric",
        "integer"
      )
      
      character1900=class1900 %in% c(
        "character",
        "factor"
      )
      
      character1910=class1910 %in% c(
        "character",
        "factor"
      )
      
      compatible=
        class1900==class1910 |
        (numeric1900 & numeric1910) |
        (character1900 & character1910)
      
      if (!compatible){
        compatible_classes=FALSE
      }
      
    }
    
  }
  
  incarceration_all_na=all(
    sapply(
      incarceration_vars,
      function(var){
        var %in% names(x1900) &
          all(is.na(x1900[[var]]))
      }
    )
  )
  
  duplicate_hex_ids=sum(
    duplicated(x1900$hex_id)
  )
  
  year_correct=
    "year" %in% names(x1900) &
    all(x1900$year==1900)
  
  validation=rbind(
    validation,
    data.frame(
      city=city,
      cols_1900=ncol(x1900),
      cols_1910=ncol(x1910),
      same_columns=same_columns,
      same_column_order=same_column_order,
      compatible_classes=compatible_classes,
      incarceration_all_na=incarceration_all_na,
      duplicate_hex_ids=duplicate_hex_ids,
      year_correct=year_correct,
      stringsAsFactors=FALSE
    )
  )
  
}

cat("\n====================================================\n")
cat("1900 VS 1910 PANEL CONFORMABILITY\n")
cat("====================================================\n")

print(
  validation,
  row.names=FALSE
)

problems=validation[
  validation$cols_1900!=validation$cols_1910 |
    !validation$same_columns |
    !validation$same_column_order |
    !validation$compatible_classes |
    !validation$incarceration_all_na |
    validation$duplicate_hex_ids>0 |
    !validation$year_correct,
]

if (nrow(problems)==0){
  
  cat("\nALL 1900 MATRICES ARE PANEL-CONFORMABLE WITH 1910.\n")
  
} else {
  
  cat("\nCITIES REQUIRING ATTENTION:\n")
  
  print(
    problems,
    row.names=FALSE
  )
  
}










############################################################
######## 1940 OLD VS NEW VALIDATION #########################
############################################################

new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
new_balanced_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats_balanced"

old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"
old_balanced_dir="/home/zach/Dropbox/corrected_intersections/hex_mats_balanced_1940_change"


regular_cities=c(
  "Baltimore",
  "Boston",
  "Chicago",
  "Cincinnati",
  "Cleveland",
  "Philadelphia",
  "Pittsburgh",
  "StLouis"
)

balanced_cities=c(
  "Brooklyn",
  "Detroit",
  "Manhattan"
)


ignore_vars=c(
  "foreign",
  "foreign_prop",
  "pop_mlp",
  "incarc_mlp",
  "incarc_mlp_prop",
  "household_count",
  "occ_score",
  "sei",
  "owned",
  "rent"
)

tolerance=1e-10


validation_summary=data.frame(
  city=character(),
  version=character(),
  new_rows=integer(),
  old_rows=integer(),
  same_rows=logical(),
  same_hex_ids=logical(),
  same_hex_order=logical(),
  n_common_cols=integer(),
  n_checked_cols=integer(),
  n_changed_checked_cols=integer(),
  changed_checked_cols=character(),
  stringsAsFactors=FALSE
)


############################################################
######## FUNCTION ###########################################
############################################################

validate_city=function(city,version,new_folder,old_folder){
  
  f=paste0("hex_mat_",city,"_1940.rda")
  
  new_object_name=load(
    file.path(new_folder,f)
  )
  
  new=get(new_object_name[1])
  
  rm(list=new_object_name)
  
  
  old_object_name=load(
    file.path(old_folder,f)
  )
  
  old=get(old_object_name[1])
  
  rm(list=old_object_name)
  
  
  new_hex=as.character(new$hex_id)
  old_hex=as.character(old$hex_id)
  
  common_hex=intersect(
    new_hex,
    old_hex
  )
  
  new_compare=new[
    match(common_hex,new_hex),
  ]
  
  old_compare=old[
    match(common_hex,old_hex),
  ]
  
  common_cols=intersect(
    names(new_compare),
    names(old_compare)
  )
  
  check_cols=setdiff(
    common_cols,
    ignore_vars
  )
  
  changed_cols=c()
  
  
  for (var in check_cols){
    
    x=new_compare[[var]]
    y=old_compare[[var]]
    
    x_num=suppressWarnings(
      as.numeric(as.character(x))
    )
    
    y_num=suppressWarnings(
      as.numeric(as.character(y))
    )
    
    numeric_like=
      (
        is.numeric(x) |
          all(is.na(x) | !is.na(x_num))
      ) &
      (
        is.numeric(y) |
          all(is.na(y) | !is.na(y_num))
      )
    
    if (numeric_like){
      
      missing_difference=xor(
        is.na(x_num),
        is.na(y_num)
      )
      
      value_difference=
        !is.na(x_num) &
        !is.na(y_num) &
        abs(x_num-y_num)>tolerance
      
      if (
        any(missing_difference) |
        any(value_difference)
      ){
        
        changed_cols=c(
          changed_cols,
          var
        )
        
      }
      
    } else {
      
      x_char=as.character(x)
      y_char=as.character(y)
      
      same=
        (is.na(x_char) & is.na(y_char)) |
        (
          !is.na(x_char) &
            !is.na(y_char) &
            x_char==y_char
        )
      
      if (any(!same)){
        
        changed_cols=c(
          changed_cols,
          var
        )
        
      }
      
    }
    
  }
  
  
  cat("\n====================================================\n")
  cat("VALIDATING:",f,"\n")
  cat("VERSION:",version,"\n")
  cat("====================================================\n")
  
  cat(
    "Rows identical:",
    nrow(new)==nrow(old),
    "\n"
  )
  
  cat(
    "Hex IDs identical:",
    setequal(new_hex,old_hex),
    "\n"
  )
  
  cat(
    "Hex order identical:",
    identical(new_hex,old_hex),
    "\n"
  )
  
  cat(
    "Checked common columns:",
    length(check_cols),
    "\n"
  )
  
  cat(
    "Changed checked columns:",
    length(changed_cols),
    "\n"
  )
  
  if (length(changed_cols)>0){
    
    cat(
      "Unexpected changed columns:",
      paste(changed_cols,collapse=", "),
      "\n"
    )
    
  }
  
  
  return(
    data.frame(
      city=city,
      version=version,
      new_rows=nrow(new),
      old_rows=nrow(old),
      same_rows=nrow(new)==nrow(old),
      same_hex_ids=setequal(new_hex,old_hex),
      same_hex_order=identical(new_hex,old_hex),
      n_common_cols=length(common_cols),
      n_checked_cols=length(check_cols),
      n_changed_checked_cols=length(changed_cols),
      changed_checked_cols=
        ifelse(
          length(changed_cols)==0,
          "",
          paste(changed_cols,collapse=", ")
        ),
      stringsAsFactors=FALSE
    )
  )
  
}


############################################################
######## REGULAR 1940 CITIES ###############################
############################################################

for (city in regular_cities){
  
  validation_summary=rbind(
    validation_summary,
    validate_city(
      city=city,
      version="regular",
      new_folder=new_dir,
      old_folder=old_dir
    )
  )
  
}


############################################################
######## BALANCED-ONLY 1940 CITIES #########################
############################################################

for (city in balanced_cities){
  
  validation_summary=rbind(
    validation_summary,
    validate_city(
      city=city,
      version="balanced",
      new_folder=new_balanced_dir,
      old_folder=old_balanced_dir
    )
  )
  
}


############################################################
######## FINAL SUMMARY ######################################
############################################################

cat("\n\n")
cat("====================================================\n")
cat("1940 VALIDATION SUMMARY\n")
cat("====================================================\n")

print(
  validation_summary,
  row.names=FALSE
)


problems=validation_summary[
  !validation_summary$same_rows |
    !validation_summary$same_hex_ids |
    validation_summary$n_changed_checked_cols>0,
]


cat("\n====================================================\n")
cat("CITIES REQUIRING ATTENTION\n")
cat("====================================================\n")

if (nrow(problems)==0){
  
  cat("\nALL 1940 CITIES PASS.\n")
  
} else {
  
  print(
    problems,
    row.names=FALSE
  )
  
}




new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
new_balanced_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats_balanced"

old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"
old_balanced_dir="/home/zach/Dropbox/corrected_intersections/hex_mats_balanced_1940_change"

checks=list(
  Chicago=c("pop","pop_abe","incarc_abe_prop","ital_prop","any_mori"),
  Philadelphia=c("any_mori"),
  Pittsburgh=c("any_mori"),
  StLouis=c("any_mori")
)

balanced_checks=list(
  Brooklyn=c("pop_cs","incarc_cs","incarc_cs_prop","any_mori","mafia_book_hex"),
  Detroit=c("pop_cs","incarc_cs","incarc_cs_prop","mafia_book_hex"),
  Manhattan=c("pop_cs","incarc_cs","incarc_cs_prop","any_mori","mafia_book_hex")
)

check_city=function(city,vars,new_folder,old_folder){
  
  f=paste0("hex_mat_",city,"_1940.rda")
  
  xname=load(file.path(new_folder,f))
  new=get(xname[1])
  rm(list=xname)
  
  xname=load(file.path(old_folder,f))
  old=get(xname[1])
  rm(list=xname)
  
  old=old[
    match(new$hex_id,old$hex_id),
  ]
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  for (var in vars){
    
    x=as.numeric(new[[var]])
    y=as.numeric(old[[var]])
    
    changed=which(
      xor(is.na(x),is.na(y)) |
        (
          !is.na(x) &
            !is.na(y) &
            abs(x-y)>1e-10
        )
    )
    
    cat("\n",var,"\n",sep="")
    cat("N different:",length(changed),"\n")
    cat("Old sum:",sum(y,na.rm=TRUE),"\n")
    cat("New sum:",sum(x,na.rm=TRUE),"\n")
    
    if (length(changed)>0){
      
      print(
        data.frame(
          hex_id=new$hex_id[changed],
          old=y[changed],
          new=x[changed]
        ),
        row.names=FALSE
      )
      
    }
    
  }
  
}

for (city in names(checks)){
  check_city(
    city,
    checks[[city]],
    new_dir,
    old_dir
  )
}

for (city in names(balanced_checks)){
  check_city(
    city,
    balanced_checks[[city]],
    new_balanced_dir,
    old_balanced_dir
  )
}


new_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats"
new_balanced_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats_balanced"

old_dir="/home/zach/Dropbox/corrected_intersections/hex_mats"
old_balanced_dir="/home/zach/Dropbox/corrected_intersections/hex_mats_balanced_1940_change"

cities=c(
  "Chicago",
  "Philadelphia",
  "Pittsburgh",
  "StLouis"
)

for (city in cities){
  
  f=paste0("hex_mat_",city,"_1940.rda")
  
  xname=load(file.path(new_dir,f))
  new=get(xname[1])
  rm(list=xname)
  
  xname=load(file.path(old_dir,f))
  old=get(xname[1])
  rm(list=xname)
  
  old=old[
    match(new$hex_id,old$hex_id),
  ]
  
  changed=which(
    old$any_mori!=new$any_mori
  )
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  print(
    data.frame(
      hex_id=new$hex_id[changed],
      old_any_mori=old$any_mori[changed],
      new_any_mori=new$any_mori[changed],
      old_mori=old$mori[changed],
      new_mori=new$mori[changed],
      old_non_mori=old$non_mori_sicilians[changed],
      new_non_mori=new$non_mori_sicilians[changed],
      old_all=old$all_sicilians[changed],
      new_all=new$all_sicilians[changed]
    ),
    row.names=FALSE
  )
  
}
############################################################
######## OLD VS NEW 1940 INCARCERATION SOURCE ##############
############################################################

load(
  "/home/zach/Dropbox/transfer/inc_rates/ed_incarceration_rates_1940.rda"
)

inc_old=ed_incarceration_rates_1940
rm(ed_incarceration_rates_1940)

load(
  "intermediate_outputs/incarceration_rates/ed_incarceration_rates_1940.rda"
)

inc_new=ed_incarceration_rates_1940
rm(ed_incarceration_rates_1940)


inc_old$state_county=gsub(
  "^([^_]*_[^_]*_).*$",
  "\\1",
  inc_old$ed_state_county_id
)

inc_old$ed=as.numeric(
  substr(
    inc_old$enum_dist,
    4,
    nchar(inc_old$enum_dist)
  )
)

inc_old$ed=as.character(inc_old$ed)
inc_old$ed=paste0(
  inc_old$state_county,
  inc_old$ed
)


inc_new$state_county=gsub(
  "^([^_]*_[^_]*_).*$",
  "\\1",
  inc_new$ed_state_county_id
)

inc_new$ed=as.numeric(
  substr(
    inc_new$enum_dist,
    4,
    nchar(inc_new$enum_dist)
  )
)

inc_new$ed=as.character(inc_new$ed)
inc_new$ed=paste0(
  inc_new$state_county,
  inc_new$ed
)


cities=c(
  "Brooklyn",
  "Detroit",
  "Manhattan"
)

prefixes=c(
  Brooklyn="13_470_",
  Detroit="23_1630_",
  Manhattan="13_610_"
)


for (city in cities){
  
  f=paste0(
    "intermediate_outputs/step_2_intersections/intersections_",
    city,
    "_1930.rda"
  )
  
  xname=load(f)
  x=get(xname[1])
  rm(list=xname)
  
  eds=paste0(
    prefixes[city],
    x$ED
  )
  
  eds=unique(eds)
  
  tmp=data.frame(
    ed=eds,
    old_pop_cs=as.numeric(
      inc_old[
        match(eds,inc_old$ed),
        10
      ]
    ),
    new_pop_cs=as.numeric(
      inc_new[
        match(eds,inc_new$ed),
        "pop_tree"
      ]
    ),
    old_incarc_cs=as.numeric(
      inc_old[
        match(eds,inc_old$ed),
        9
      ]
    ),
    new_incarc_cs=as.numeric(
      inc_new[
        match(eds,inc_new$ed),
        "num_incarc_tree"
      ]
    )
  )
  
  changed=which(
    xor(
      is.na(tmp$old_pop_cs),
      is.na(tmp$new_pop_cs)
    ) |
      xor(
        is.na(tmp$old_incarc_cs),
        is.na(tmp$new_incarc_cs)
      ) |
      (
        !is.na(tmp$old_pop_cs) &
          !is.na(tmp$new_pop_cs) &
          abs(tmp$old_pop_cs-tmp$new_pop_cs)>1e-10
      ) |
      (
        !is.na(tmp$old_incarc_cs) &
          !is.na(tmp$new_incarc_cs) &
          abs(tmp$old_incarc_cs-tmp$new_incarc_cs)>1e-10
      )
  )
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  cat(
    "EDs compared:",
    nrow(tmp),
    "\n"
  )
  
  cat(
    "EDs different:",
    length(changed),
    "\n"
  )
  
  cat(
    "Old pop_cs sum:",
    sum(tmp$old_pop_cs,na.rm=TRUE),
    "\n"
  )
  
  cat(
    "New pop_cs sum:",
    sum(tmp$new_pop_cs,na.rm=TRUE),
    "\n"
  )
  
  cat(
    "Old incarc_cs sum:",
    sum(tmp$old_incarc_cs,na.rm=TRUE),
    "\n"
  )
  
  cat(
    "New incarc_cs sum:",
    sum(tmp$new_incarc_cs,na.rm=TRUE),
    "\n"
  )
  
}





cities=c(
  "Brooklyn",
  "Detroit",
  "Manhattan"
)

old_base="/home/zach/Dropbox/corrected_intersections/intersections_800"
new_base="intermediate_outputs/step_2_intersections"

for (city in cities){
  
  old_file=file.path(
    old_base,
    city,
    paste0("intersections_",city,"_1930.rda")
  )
  
  new_file=file.path(
    new_base,
    paste0("intersections_",city,"_1930.rda")
  )
  
  xname=load(old_file)
  old=get(xname[1])
  rm(list=xname)
  
  xname=load(new_file)
  new=get(xname[1])
  rm(list=xname)
  
  old=st_drop_geometry(old)
  new=st_drop_geometry(new)
  
  old$key=paste0(
    old$ED,
    "_",
    old$hex_id
  )
  
  new$key=paste0(
    new$ED,
    "_",
    new$hex_id
  )
  
  common=intersect(
    old$key,
    new$key
  )
  
  old_compare=old[
    match(common,old$key),
  ]
  
  new_compare=new[
    match(common,new$key),
  ]
  
  prop_diff=which(
    abs(
      old_compare$proportion_intersected -
        new_compare$proportion_intersected
    )>1e-10
  )
  
  area_diff=which(
    abs(
      old_compare$area_intersect -
        new_compare$area_intersect
    )>1e-10
  )
  
  cat("\n============================\n")
  cat(city,"\n")
  cat("============================\n")
  
  cat("Old rows:",nrow(old),"\n")
  cat("New rows:",nrow(new),"\n")
  
  cat(
    "Same ED-hex pairs:",
    setequal(old$key,new$key),
    "\n"
  )
  
  cat(
    "Common ED-hex pairs:",
    length(common),
    "\n"
  )
  
  cat(
    "Different proportion_intersected:",
    length(prop_diff),
    "\n"
  )
  
  cat(
    "Different area_intersect:",
    length(area_diff),
    "\n"
  )
  
  if (length(prop_diff)>0){
    
    cat(
      "Maximum proportion difference:",
      max(
        abs(
          old_compare$proportion_intersected[prop_diff] -
            new_compare$proportion_intersected[prop_diff]
        )
      ),
      "\n"
    )
    
  }
  
}






############################################################
######## DIRECTLY VERIFY BALANCED 1940 CS ##################
############################################################

new_balanced_dir="~/Desktop/Drive1/the_departed/intermediate_outputs/step_3_hex_mats_balanced"
old_balanced_dir="/home/zach/Dropbox/corrected_intersections/hex_mats_balanced_1940_change"


############################################################
######## LOAD AND PREP 1940 INCARCERATION DATA #############
############################################################

load(
  "intermediate_outputs/incarceration_rates/ed_incarceration_rates_1940.rda"
)

ed_incarceration_rates_1940[
  is.na(ed_incarceration_rates_1940)
]=0

ed_incarceration_rates_1940$state_county=gsub(
  "^([^_]*_[^_]*_).*$",
  "\\1",
  ed_incarceration_rates_1940$ed_state_county_id
)

ed_incarceration_rates_1940$ed=as.numeric(
  substr(
    ed_incarceration_rates_1940$enum_dist,
    4,
    nchar(ed_incarceration_rates_1940$enum_dist)
  )
)

ed_incarceration_rates_1940$ed=as.character(
  ed_incarceration_rates_1940$ed
)

ed_incarceration_rates_1940$ed=paste0(
  ed_incarceration_rates_1940$state_county,
  ed_incarceration_rates_1940$ed
)


############################################################
######## CITIES #############################################
############################################################

cities=c(
  "Brooklyn",
  "Detroit",
  "Manhattan"
)

prefixes=c(
  Brooklyn="13_470_",
  Detroit="23_1630_",
  Manhattan="13_610_"
)


############################################################
######## DIRECT RECONSTRUCTION ##############################
############################################################

for (city in cities){
  
  ##########################################################
  # LOAD RAW 1930 INTERSECTION SHELL
  ##########################################################
  
  f=paste0(
    "intermediate_outputs/step_2_intersections/intersections_",
    city,
    "_1930.rda"
  )
  
  xname=load(f)
  intersections=get(xname[1])
  rm(list=xname)
  
  intersections$ED=paste0(
    prefixes[city],
    intersections$ED
  )
  
  intersections=st_drop_geometry(
    intersections
  )
  
  
  ##########################################################
  # ATTACH 1940 CS DATA
  ##########################################################
  
  intersections$pop_cs=as.numeric(
    ed_incarceration_rates_1940[
      match(
        intersections$ED,
        ed_incarceration_rates_1940$ed
      ),
      "pop_tree"
    ]
  )
  
  intersections$incarc_cs=as.numeric(
    ed_incarceration_rates_1940[
      match(
        intersections$ED,
        ed_incarceration_rates_1940$ed
      ),
      "num_incarc_tree"
    ]
  )
  
  
  ##########################################################
  # WEIGHT BY 1930 INTERSECTION SHARE
  ##########################################################
  
  intersections$pop_cs_weighted=
    intersections$pop_cs*
    intersections$proportion_intersected
  
  intersections$incarc_cs_weighted=
    intersections$incarc_cs*
    intersections$proportion_intersected
  
  
  ##########################################################
  # DIRECT HEX RECONSTRUCTION
  ##########################################################
  
  direct=data.frame(
    hex_id=unique(intersections$hex_id),
    pop_cs=0,
    incarc_cs=0
  )
  
  for (i in 1:nrow(direct)){
    
    direct[i,"pop_cs"]=sum(
      intersections$pop_cs_weighted[
        which(
          intersections$hex_id==
            direct[i,"hex_id"]
        )
      ]
    )
    
    direct[i,"incarc_cs"]=sum(
      intersections$incarc_cs_weighted[
        which(
          intersections$hex_id==
            direct[i,"hex_id"]
        )
      ]
    )
    
  }
  
  
  ##########################################################
  # LOAD NEW BALANCED HEX MAT
  ##########################################################
  
  fhex=paste0(
    "hex_mat_",
    city,
    "_1940.rda"
  )
  
  xname=load(
    file.path(
      new_balanced_dir,
      fhex
    )
  )
  
  new=get(xname[1])
  rm(list=xname)
  
  
  ##########################################################
  # LOAD OLD BALANCED_1940_CHANGE HEX MAT
  ##########################################################
  
  xname=load(
    file.path(
      old_balanced_dir,
      fhex
    )
  )
  
  old=get(xname[1])
  rm(list=xname)
  
  
  ##########################################################
  # ALIGN BY HEX ID
  ##########################################################
  
  new=new[
    match(
      direct$hex_id,
      new$hex_id
    ),
  ]
  
  old=old[
    match(
      direct$hex_id,
      old$hex_id
    ),
  ]
  
  
  ##########################################################
  # DIFFERENCE COUNTS
  ##########################################################
  
  direct_new_pop_diff=which(
    abs(
      direct$pop_cs-
        new$pop_cs
    )>1e-10
  )
  
  direct_old_pop_diff=which(
    abs(
      direct$pop_cs-
        old$pop_cs
    )>1e-10
  )
  
  direct_new_incarc_diff=which(
    abs(
      direct$incarc_cs-
        new$incarc_cs
    )>1e-10
  )
  
  direct_old_incarc_diff=which(
    abs(
      direct$incarc_cs-
        old$incarc_cs
    )>1e-10
  )
  
  
  ##########################################################
  # RESULTS
  ##########################################################
  
  cat("\n")
  cat("====================================================\n")
  cat(city,"\n")
  cat("====================================================\n")
  
  
  cat("\nPOP_CS\n")
  
  cat(
    "Direct sum:",
    sum(
      direct$pop_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "New sum:",
    sum(
      new$pop_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "Old sum:",
    sum(
      old$pop_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "Direct vs new differences:",
    length(
      direct_new_pop_diff
    ),
    "\n"
  )
  
  cat(
    "Direct vs old differences:",
    length(
      direct_old_pop_diff
    ),
    "\n"
  )
  
  
  cat("\nINCARC_CS\n")
  
  cat(
    "Direct sum:",
    sum(
      direct$incarc_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "New sum:",
    sum(
      new$incarc_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "Old sum:",
    sum(
      old$incarc_cs,
      na.rm=TRUE
    ),
    "\n"
  )
  
  cat(
    "Direct vs new differences:",
    length(
      direct_new_incarc_diff
    ),
    "\n"
  )
  
  cat(
    "Direct vs old differences:",
    length(
      direct_old_incarc_diff
    ),
    "\n"
  )
  
  
  ##########################################################
  # OPTIONAL: SHOW FIRST DIFFERENCES
  ##########################################################
  
  if (length(direct_old_pop_diff)>0){
    
    cat("\nFirst direct vs old POP_CS differences:\n")
    
    print(
      head(
        data.frame(
          hex_id=
            direct$hex_id[
              direct_old_pop_diff
            ],
          direct=
            direct$pop_cs[
              direct_old_pop_diff
            ],
          old=
            old$pop_cs[
              direct_old_pop_diff
            ]
        ),
        10
      ),
      row.names=FALSE
    )
    
  }
  
  
  if (length(direct_old_incarc_diff)>0){
    
    cat("\nFirst direct vs old INCARC_CS differences:\n")
    
    print(
      head(
        data.frame(
          hex_id=
            direct$hex_id[
              direct_old_incarc_diff
            ],
          direct=
            direct$incarc_cs[
              direct_old_incarc_diff
            ],
          old=
            old$incarc_cs[
              direct_old_incarc_diff
            ]
        ),
        10
      ),
      row.names=FALSE
    )
    
  }
  
}






############################################################
######## FINAL 1940 SOURCE CHECKS ###########################
############################################################


############################################################
######## 1. OLD VS NEW 1940 HOUSEHOLD SAMPLES ##############
############################################################

old_household_dir="/home/zach/Dropbox/numident_link/outputs_for_hexagons"
new_household_dir="intermediate_outputs/outputs_for_hexagons"


prep_household=function(x){
  
  x=as.data.frame(x)
  
  x$enumdist=as.character(x$enumdist)
  
  x$ed=substr(
    x$enumdist,
    4,
    nchar(x$enumdist)
  )
  
  x$ed=gsub("^0+","",x$ed)
  x$ed=gsub("0$","",x$ed)
  
  x$ed=paste0(
    x$stateicp,
    "_",
    x$countyicp,
    "_",
    x$ed
  )
  
  return(x)
  
}


############################################################
# ALL SICILIANS
############################################################

load(
  file.path(
    old_household_dir,
    "household_sample_1940.rda"
  )
)

old_all=prep_household(
  household_sample_1940
)

rm(household_sample_1940)


load(
  file.path(
    new_household_dir,
    "household_sample_1940.rda"
  )
)

new_all=prep_household(
  household_sample_1940
)

rm(household_sample_1940)


############################################################
# MORI
############################################################

load(
  file.path(
    old_household_dir,
    "household_sample_mori_1940.rda"
  )
)

old_mori=prep_household(
  household_sample_mori_1940
)

rm(household_sample_mori_1940)


load(
  file.path(
    new_household_dir,
    "household_sample_mori_1940.rda"
  )
)

new_mori=prep_household(
  household_sample_mori_1940
)

rm(household_sample_mori_1940)


############################################################
# NON-MORI
############################################################

load(
  file.path(
    old_household_dir,
    "household_sample_non_mori_1940.rda"
  )
)

old_non=prep_household(
  household_sample_non_mori_1940
)

rm(household_sample_non_mori_1940)


load(
  file.path(
    new_household_dir,
    "household_sample_non_mori_1940.rda"
  )
)

new_non=prep_household(
  household_sample_non_mori_1940
)

rm(household_sample_non_mori_1940)


############################################################
# ED COUNTS
############################################################

eds=sort(
  unique(
    c(
      old_all$ed,
      new_all$ed,
      old_mori$ed,
      new_mori$ed,
      old_non$ed,
      new_non$ed
    )
  )
)

hh_compare=data.frame(
  ed=eds,
  old_all=0,
  new_all=0,
  old_mori=0,
  new_mori=0,
  old_non=0,
  new_non=0
)


for (i in 1:nrow(hh_compare)){
  
  e=hh_compare$ed[i]
  
  hh_compare$old_all[i]=
    nrow(old_all[which(old_all$ed==e),])
  
  hh_compare$new_all[i]=
    nrow(new_all[which(new_all$ed==e),])
  
  hh_compare$old_mori[i]=
    nrow(old_mori[which(old_mori$ed==e),])
  
  hh_compare$new_mori[i]=
    nrow(new_mori[which(new_mori$ed==e),])
  
  hh_compare$old_non[i]=
    nrow(old_non[which(old_non$ed==e),])
  
  hh_compare$new_non[i]=
    nrow(new_non[which(new_non$ed==e),])
  
}


changed=which(
  hh_compare$old_all!=hh_compare$new_all |
    hh_compare$old_mori!=hh_compare$new_mori |
    hh_compare$old_non!=hh_compare$new_non
)


cat("\n")
cat("====================================================\n")
cat("1940 HOUSEHOLD SAMPLE COMPARISON\n")
cat("====================================================\n")

cat(
  "Old all-Sicilian rows:",
  nrow(old_all),
  "\n"
)

cat(
  "New all-Sicilian rows:",
  nrow(new_all),
  "\n"
)

cat(
  "Old Mori rows:",
  nrow(old_mori),
  "\n"
)

cat(
  "New Mori rows:",
  nrow(new_mori),
  "\n"
)

cat(
  "Old non-Mori rows:",
  nrow(old_non),
  "\n"
)

cat(
  "New non-Mori rows:",
  nrow(new_non),
  "\n"
)

cat(
  "EDs with changed household counts:",
  length(changed),
  "\n"
)

if (length(changed)>0){
  
  print(
    hh_compare[changed,],
    row.names=FALSE
  )
  
}


############################################################
######## 2. OLD VS NEW 1940 POPULATION SOURCE — CHICAGO ####
############################################################

load(
  "/home/zach/Dropbox/mori_transfer/mori_work_on_ferry/transfer/population_1940_relevant.rda"
)

old_pop=pop_1940_relevant
rm(pop_1940_relevant)


load(
  "intermediate_outputs/population_1940_relevant.rda"
)

new_pop=pop_1940_relevant
rm(pop_1940_relevant)


prep_pop=function(x){
  
  substr(
    x$enum_dist_id,
    nchar(x$enum_dist_id),
    nchar(x$enum_dist_id)
  )=
    ifelse(
      substr(
        x$enum_dist_id,
        nchar(x$enum_dist_id),
        nchar(x$enum_dist_id)
      )=="1",
      "a",
      ifelse(
        substr(
          x$enum_dist_id,
          nchar(x$enum_dist_id),
          nchar(x$enum_dist_id)
        )=="2",
        "b",
        "0"
      )
    )
  
  x$enum_dist_id=gsub(
    "^0+",
    "",
    x$enum_dist_id
  )
  
  x$enum_dist_id=gsub(
    "0$",
    "",
    x$enum_dist_id
  )
  
  return(x)
  
}


old_pop=prep_pop(old_pop)
new_pop=prep_pop(new_pop)


old_chicago=old_pop[
  grepl(
    "^21_310_",
    old_pop$enum_dist_id
  ),
]

new_chicago=new_pop[
  grepl(
    "^21_310_",
    new_pop$enum_dist_id
  ),
]


eds=sort(
  unique(
    c(
      old_chicago$enum_dist_id,
      new_chicago$enum_dist_id
    )
  )
)


pop_compare=data.frame(
  ed=eds,
  old_population=old_chicago[
    match(
      eds,
      old_chicago$enum_dist_id
    ),
    "population"
  ],
  new_population=new_chicago[
    match(
      eds,
      new_chicago$enum_dist_id
    ),
    "population"
  ],
  old_italian_population=old_chicago[
    match(
      eds,
      old_chicago$enum_dist_id
    ),
    "italian_population"
  ],
  new_italian_population=new_chicago[
    match(
      eds,
      new_chicago$enum_dist_id
    ),
    "italian_population"
  ]
)


changed=which(
  xor(
    is.na(pop_compare$old_population),
    is.na(pop_compare$new_population)
  ) |
    xor(
      is.na(pop_compare$old_italian_population),
      is.na(pop_compare$new_italian_population)
    ) |
    (
      !is.na(pop_compare$old_population) &
        !is.na(pop_compare$new_population) &
        pop_compare$old_population!=
        pop_compare$new_population
    ) |
    (
      !is.na(pop_compare$old_italian_population) &
        !is.na(pop_compare$new_italian_population) &
        pop_compare$old_italian_population!=
        pop_compare$new_italian_population
    )
)


cat("\n")
cat("====================================================\n")
cat("CHICAGO 1940 POPULATION SOURCE\n")
cat("====================================================\n")

cat(
  "Old Chicago EDs:",
  nrow(old_chicago),
  "\n"
)

cat(
  "New Chicago EDs:",
  nrow(new_chicago),
  "\n"
)

cat(
  "Changed EDs:",
  length(changed),
  "\n"
)

if (length(changed)>0){
  
  print(
    pop_compare[changed,],
    row.names=FALSE
  )
  
}