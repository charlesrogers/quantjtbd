#' Takes SPSS Data that has a label format of "XXXX "Job Name" - "Something you don't want" and convert it the column name and strip everything except the "Job_Name"... It has to be in that format
#'
#' @param data this is the dataframe you want, needs LABELS from importing via Haven
#' @param job_section name of job section
#'
#' @return Dataframe with updated column names
#' @export
#'
#' @examples
#' build_sat_column_names(data = "df_imp_cols", job_section = "Finding_A_Restaurant")
#' build_imp_column_names(data = "df_sat_cols", job_section = "Finding_A_Restaurant")

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
# Change all haven data labels to factors
change_labeles_to_factors <- function(df){
  df <- df %>%
  mutate_if(haven::is.labelled, as_factor)
}
# Merging all the functions that are universal and putting them into a sub-function for th two functions we'll actually use
prep_data <- function(df){
  # Switch column labels with variable labels
  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
}

# Prepend the updated columns with the needed IMP and job section prefixes
build_imp_column_names <- function(df, job_section) {
  df <- prep_data(df)
# Add "imp__" to the beginning of the column name
# Add "." + "job_section" variable to just after the "imp__" portion
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))
  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)

  return(df)
}
# Prepend the updated columns with the needed SAT and job section prefixes
build_sat_column_names <- function(df, job_section) {
  df <- prep_data(df)
  # Add "sat__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "sat__" portion
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))

  # Replace the original column name with the new one
  df <- setNames(df, new_col_name)

  return(df)
}

