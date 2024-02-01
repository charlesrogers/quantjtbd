
#### Get Total Population Importance, Satisfaction, and Opportunity Scores ####
#' JTBD_SCORES
#'
#' @param your_data_frame
#'
#' @return a datframe of
#' @export
#'
#' @examples jtbd_scores("jtbd_importance_and_satisfaction_df")
#'
#'
#'
jtbd_scores <- function(your_data_frame){
  # Find the columns that start with "imp_" and "sat_" in the data frame
  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)

  # Calculate the opportunity scores for the selected columns
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)

  # Split the 'objective_name' column into two new columns 'imp_sat' and 'objective'
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)

  # Calculate the opportunity scores for the split data frame
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)

  # Recalculate the 'opportunity_score' based on a condition, arrange the data in descending order of 'opportunity_score',
  # add a new column 'segment_name' with the value "total_population", and rank the data based on 'opportunity_score'
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score)) %>%
    mutate(segment_name="total_population",
           rank=rank(desc(opportunity_score)))

  # Reshape the data from long to wide format based on 'objective' and 'segment_name', convert 'objective' to factor,
  # split 'objective' into two new columns 'job_step' and 'objective', round the numeric columns to 1 decimal place,
  # and reorder the levels of 'objective'
  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    mutate_if(is.numeric,round,1) %>%
    mutate(objective=factor(objective, levels=objective))

  # Assign "total_opportunity" to 'deparsed_column_name'
  deparsed_column_name <- "total_opportunity"

  # Print the 'importance_satisfaction_opportunity' data frame using the 'print_data_table_general_population' function
  importance_satisfaction_opportunity %>%
    print_data_table_general_population(.,deparsed_column_name)

  # Return the 'importance_satisfaction_opportunity' data frame
  return(importance_satisfaction_opportunity)
}
#### JTBD Opp Scores ####
# Initialize 'individual_data' and 'all_data' as NULL
individual_data <- NULL
all_data <- NULL

calculate_pop_pct_score <- function(objectives){
  # Loop through each 'objective' in 'objectives'
  for (objective in seq_along(objectives)){
    # Assign the name of the current 'objective' to 'namez'
    namez <- names(objectives)[[objective]]

    # Count the frequency of each level in the current 'objective' and add a new column 'objective_name' with the value of 'namez'
    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name=namez)

    # Filter the 'objective_score' where 'user_rating' is in 1 to 5 and select the columns 'objective_name', 'user_rating', and 'n'
    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(objective_name,user_rating,n)

    # Print the 'objective_score_tibble'
    print(objective_score_tibble)

    # Summarize the 'objective_score_tibble' to get the total sum of 'n' and the sum of 'n' where 'user_rating' is 4 or 5
    # Calculate 'imp_sat_score' as the ratio of 'imp_sat_sum' to 'total_sum' multiplied by 10
    individual_data <-  objective_score_tibble %>%
      summarize(objective_name=unique(objective_name),
                total_sum=sum(n),
                imp_sat_sum=sum(n[user_rating==5|
                                    user_rating==4]))  %>%
      mutate(imp_sat_score=((imp_sat_sum/total_sum)*10))

    # Print the 'individual_data'
    print(individual_data)

    # Append the 'individual_data' to 'all_data'
    all_data <- rbind(all_data,individual_data)
  }

  # Return the 'all_data'
  return(all_data)
}

#### Find JTBD Columns ####
find_imp_sat_columns <- function(your_data_frame){
  # Create a new variable 'imp_columns' and assign the columns that start with "imp_" from the data frame to it
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))

  # Create a new variable 'sat_columns' and assign the columns that start with "sat_" from the data frame to it
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))

  # Combine 'imp_columns' and 'sat_columns' into a new data frame 'data_frame_imp_sat'
  data_frame_imp_sat <- cbind(imp_columns,sat_columns)

  # Return the 'data_frame_imp_sat' data frame
  return(data_frame_imp_sat)
}
#### Split Imp/Sat Columns ####
split_imp_sat_columns <- function(data_frame_imp_sat){
  # Create a new variable 'data_frame_imp_sat_split' and assign the transformed data frame to it
  data_frame_imp_sat_split <-  data_frame_imp_sat %>%
    # Split the 'objective_name' column into two new columns 'imp_sat' and 'objective' based on the separator '__'
    separate(objective_name,"__",into = c("imp_sat","objective"),remove = FALSE)

  # Return the 'data_frame_imp_sat_split' data frame
  return(data_frame_imp_sat_split)
}

#### Calculate Opp Score ####
calculate_opportunity_score <- function(data_frame_split) {
  # Create a new variable 'opportunity_scores' and assign the transformed data frame to it
  opportunity_scores <- data_frame_split %>%
    # Reshape the data from long to wide format based on 'objective' and 'imp_sat'
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    # Create a new column 'opportunity_score' based on the condition if 'imp' is less than 'sat', return 'imp', else return 'imp+imp-sat'
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    # Arrange the data in descending order based on 'opportunity_score'
    arrange(desc(opportunity_score))

  # Return the 'opportunity_scores' data frame
  return(opportunity_scores)
}

#### Print Data Table ####
# * Print Data Table: General Population ----------------------------------
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
