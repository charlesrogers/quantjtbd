#### Print Data Table ####
# Load the 'gt' library for creating tables
library('gt')

# Define a function to print a data table for the general population
print_data_table_general_population <- function(your_data_frame,deparsed_column_name){
  # Modify the data frame by replacing underscores with spaces in 'objective' and 'job_step', renaming some columns,
  # removing the 'rank_total_population' column, and creating a gt table with 'job_step' as the group name column and 'objective' as the row name column
  modified_data_frame <- your_data_frame %>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           job_step=str_to_title(str_replace_all(job_step,"_"," "))) %>%
    rename(`Opportunity Score`=opportunity_score_total_population,
           `Importance`=imp_total_population,
           `Satisfaction`=sat_total_population,
           `Step`=job_step,
           `Objective`=objective) %>%
    select(-rank_total_population) %>%
    gt(groupname_col = "Job Step",
       rowname_col = "Objective") %>%
    # Add a header to the table with a title and subtitle
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short)),
      subtitle = html(paste0("Sample: ",sample_description))
    ) %>%
    # Set some options for the table
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    # Add a style to the cells in the 'Opportunity Score' column where the score is greater than or equal to 10
    tab_style(
      style = cell_text(color = "green",weight = "normal"),
      locations = list(
        cells_body(
          columns = vars(`Opportunity Score`),
          rows = `Opportunity Score` >= 10
        ))) %>%
    # Add a style to the cells in the 'Opportunity Score' column where the score is greater than or equal to 12
    tab_style(
      style = cell_text(color = "darkgreen",weight = "bold"),
      locations = list(
        cells_body(
          columns = vars(`Opportunity Score`),
          rows = `Opportunity Score` >= 12
        ))) %>%
    # Add a style to the group name cells and column label cells
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))

  # Create a file name for saving the table
  file_name <- paste0(project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')

  # Save the table as a PNG file
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
}
