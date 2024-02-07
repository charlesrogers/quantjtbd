## Rename a section of importance or satisfaction columns with the appropriate prefixes to be used in the opportunity calculation functions




#' Title
#'
#' @param df
#' @param unique_string_the_column_name_starts_with
#' @param job_section
#' @param imp_or_sat_string
#'
#' @return
#' @export
#'
#' @examples df_sample <- rename_spss_col.imp_sat(df,"unique_column_prefix","job_section","imp_or_sat")
#'
#'
rename_spss_col_imp_sat <- function(df,unique_string_the_column_name_starts_with,job_section,imp_or_sat_string){
  df <- convert_labels_to_row_names(df)
  df_working.1 <- select_columns_by_type(df,unique_string_the_column_name_starts_with)
  df_working.2 <- modify_col_names(df_working.1)
  df_working.3 <- add_col_prefix(df_working.2,imp_or_sat_string,job_section)
  df <- merge_named_cols_with_data_set(df,df_working.3)
}


# Swap the labels and the column names
convert_labels_to_row_names<- function(df) {
  df <- df %>%
    sjlabelled::label_to_colnames()
}


# Create a function that subsets by columns that start with x
select_columns_by_type <- function(df, unique_string_the_column_name_starts_with){
  #column_name <- deparse(substitute(unique_string_the_column_name_starts_with))
  column_name <- unique_string_the_column_name_starts_with
  df <- df %>%
    select(starts_with(column_name))
  #print(unique_string_the_column_name_starts_with)
  return(df)
}

modify_col_names <- function(df){
  # Switch column labels with variable labels
  #  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
}

# Prepend the updated columns with the needed SAT and job section prefixes
add_col_prefix <- function(df,col_prefix,job_section) {
  new_col_name <- paste0(col_prefix,"__", job_section, ".", colnames(df))
  df <- setNames(df, new_col_name)
  return(df)
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
merge_named_cols_with_data_set <- function(df,df_modified){
  df <- cbind(df,df_modified)
}

















