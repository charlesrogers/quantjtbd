# This is another weird function...  This function takes the labels from SPSS data and converts them to columns and strips out labels IF they match exactly what I had in my data :/

# limitations: have to do this by section AND imp vs sat

# Switch column labels with variable labels
 convert_labels_to_row_names<- function(df) {
  sjlabelled::label_to_colnames()
 }

remove_data_prefix <- function(df) {
# Remove the prefix by splitting on space
names(df) = sub(".*?\\s", "", names(df))
# Remove the suffix by splitting on "-"
names(df) = sub("-.*$", "", names(df))
}
#names(df_imp_3) = gsub(pattern = ".*-.*", replacement = "", x = names(df_imp_3))

# change the job names from having spaces to UNDERSCORES
replace_spaces_with_underscores <- function(df){
  df %<>%
    janitor::clean_names()
}


# Add in the "imp__" and "job_step"
build_imp_column_names <- function(df, job_section) {
  # Add "imp__" to the beginning of the column name
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))

  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)

  return(df)
}

build_sat_column_names <- function(df, job_section) {
  # Add "imp__" to the beginning of the column name
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))

  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)

  return(df)
}


# df_imp_5 <- build_imp_column_names(df_imp_4, "job_section_1")

