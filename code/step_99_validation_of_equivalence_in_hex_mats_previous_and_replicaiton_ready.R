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