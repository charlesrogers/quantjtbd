usethis::use_package("haven")
usethis::use_package("sjlabelled")
usethis::use_package("dplyr")
usethis::use_package("magrittr")

#### PREP DATA - BUILD JTBD COLS ####
#' Rename importance columns so they can be used in 'get_jtbd_scores'
#'
#' @param df a subsetted data frame that only includes the importance columns from your dataset
#' @param job_section A text string in parentheses; this is the grouping variable, most often the "job step"
#'
#' @return a data frame with duplicated columns with "imp__job_section." added to the front of the column name
#' @export
#'
#' @examples
#' build_imp_column_names(1,"find_available_options")
build_imp_column_names <- function(df, job_section) {
  df <- prep_data(df)
  # Add "imp__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "imp__" portion
  new_col_name <- paste0("imp__", job_section, ".", colnames(df))
  # Replace the original column name with the new one
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)

  return(df)
}


# Prepend the updated columns with the needed SAT and job section prefixes
#' Rename satisfaction columns so they can be used in 'get_jtbd_scores'
#'
#' @param df a subsetted data frame that only includes the satisfaction columns from your dataset
#' @param job_section tA text string in parentheses; this is the grouping variable, most often the "job step"
#'
#' @return a data frame with duplicated columns with "sat__job_section." added to the front of the column name
#' @export
#'
#' @examples
#' #' build_sat_column_names(your_data_frame,"job_step_1")
#' build_sat_column_names(df.clean,"find_available_options")
build_sat_column_names <- function(df, job_section) {
  df <- prep_data(df)
  # Add "sat__" to the beginning of the column name
  # Add "." + "job_section" variable to just after the "sat__" portion
  new_col_name <- paste0("sat__", job_section, ".", colnames(df))

  # Replace the original column name with the new one
  df <- setNames(df, new_col_name) %>%
    rename(caseid = 1)

  return(df)
}

prep_data <- function(df) {
  # Switch column labels with variable labels
  df <- convert_labels_to_row_names(df)
  df <- remove_data_prefix(df)
  df <- remove_data_suffix(df)
  df <- replace_spaces_with_underscores(df)
  df <- change_labeles_to_factors(df)
}

# Swap the labels and the column names
convert_labels_to_row_names <- function(df) {
  df <- df %>%
    sjlabelled::label_to_colnames()
}


# Remove the prefix by splitting on space
remove_data_prefix <- function(df) {
  df <- df %>% setNames(gsub(".*?--", "", names(.)))
}

# Remove the suffix by splitting on "-"
remove_data_suffix <- function(df) {
  df <- df %>% setNames(sub("-.*$", "", names(.)))
}

# change the job names from having spaces to UNDERSCORES
replace_spaces_with_underscores <- function(df) {
  df <- df %>%
    janitor::clean_names()
}
# Change all haven data labels to factors
change_labeles_to_factors <- function(df) {
  df <- df %>%
    mutate_if(haven::is.labelled, as_factor)
}
# Merging all the functions that are universal and putting them into a sub-function for th two functions we'll actually use
