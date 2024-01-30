# Swap the labels and the column names
convert_labels_to_row_names<- function(df) {
  df %<>%
    sjlabelled::label_to_colnames()
}


# Remove the prefix by splitting on space
remove_data_prefix <- function(df) {
  df <- df %>% setNames(sub(".*?\\s", "", names(.)))
}

# Remove the suffix by splitting on "-"
remove_data_suffix <- function(df) {
  df <- df %>% setNames(sub("-.*$","", names(.)))
}

# change the job names from having spaces to UNDERSCORES
replace_spaces_with_underscores <- function(df){
  df %<>%
    janitor::clean_names()
}

# Merging all the functions that are universal and putting them into a sub-function for th two functions we'll actually use
prep_data <- function(df){
  # Switch column labels with variable labels
  df <-  convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- replace_spaces_with_underscores(df)
}

build_imp_column_names <- function(df, job_section) {
  working_df <- prep_data(df)
# Add "imp__" to the beginning of the column name
# Add "." + "job_section" variable to just after the "imp__" portion
  new_col_name <- paste0("imp__", job_section, ".", colnames(working_df))
  # Replace the original column name with the new one
  working_df <- setNames(working_df, new_col_name)

  return(working_df)
}

build_sat_column_names <- function(df, job_section) {
  working_df <- prep_data(df)
  # Add "sat__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "sat__" portion
  new_col_name <- paste0("sat__", job_section, ".", colnames(working_df))

  # Replace the original column name with the new one
  working_df <- setNames(working_df, new_col_name)

  return(working_df)
}

