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


print_data_table_compare_2 <- function(your_data_frame,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b){
  columns_of_interest <- c(1:9)
  row_count<- nrow(your_data_frame)
  segement_name <- str_to_title(str_replace_all(deparsed_column_name,"_"," "))

  modified_data_frame <- your_data_frame %>%
    select(all_of(columns_of_interest)) %>%
    rename(pctDiff=pct_diff)%>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           sourcing_stage=str_to_title(str_replace_all(sourcing_stage,"_"," ")),
           factor_a_opp=.[[7]],
           factor_b_opp=.[[8]])


  factor_a_opp <- names(your_data_frame[7]) %>%
    str_replace_all(.,"opportunity_score_","") %>%
    str_to_title(.)
  factor_a_opp_name <- factor_a_opp

  factor_b_opp <- names(your_data_frame[8]) %>%
    str_replace_all(.,"opportunity_score_","") %>%
    str_to_title(.)
  factor_b_opp_name <- factor_b_opp

  modified_data_frame  <- modified_data_frame %>%
    rename(`Sourcing Stage`=sourcing_stage,
           `Objective`=objective,
           !!factor_a_opp:=factor_a_opp,
           !!factor_b_opp:=factor_b_opp) %>%
    dplyr::relocate(pctDiff,.after = everything())  %>%
    gt(groupname_col = "Sourcing Stage",
       rowname_col = "Objective") %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short, " - ",segement_name)),
      subtitle = html(paste0("Sample: ",factor_a_opp_name," group N = ",sample_size_factor_a," & ",factor_b_opp_name," group N = ",sample_size_factor_b))
    )  %>%
    tab_spanner(
      label = "Importance",
      columns = 3:4
    ) %>%
    tab_spanner(
      label = "Satisfaction",
      columns = 5:6
    ) %>%
    tab_spanner(
      label = "Opportunities",
      columns = 9:10
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    data_color(
      columns = 9:10,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything()))) %>%
    cols_hide(
      columns = c(7:8)) %>%
    cols_align(align = "right",
               columns = "pctDiff")

  #Strip these out ASAP
  file_name <- paste0(project_name,"-",deparsed_column_name,"-",file_type,lubridate::today(),'.png')
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-",file_type,"-",lubridate::today(),'.png')
  # file_name <- create_file_name(project_name,deparsed_column_name,file_type)
  # file_name_backup <- create_file_name_backup(backup_file_location,project_name,deparsed_column_name)
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
  # save_file(modified_data_frame,file_name_backup)
}

#### WIP ####

create_file_name <- function(project_name,deparsed_column_name,backup_file_location, deparsed_column_name){
  file_name <- paste0(project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
}
## MAKE THIS OPTIONAL
create_file_name_backup <- function(backup_file_location,project_name,deparsed_column_name){
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
}

save_file <- function(dataframe, filename, file_name_backup){
  gtsave(dataframe,file_name)
  gtsave(dataframe,file_name_backup)
}
