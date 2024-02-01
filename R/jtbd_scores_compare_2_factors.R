
#### Compare the JTBD Scores for 2 different factor levels  ####
# Function to calculate and compare opportunity scores for two factors
get_imp_sat_opp_scores_compare_2 <- function(your_data_frame,column_to_split_on,factor_a,factor_b) {
  # Assign "opportunity_list_table" to 'file_type' for later use in naming the output file
  file_type <- "opportunity_list_table"

  # Filter the data frame where 'column_to_split_on' equals 'factor_a'
  # This creates a subset of the data frame for the first factor
  opportunity_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  # Get the sample size of 'opportunity_calc_group_1' using the 'get_sample_size' function
  # This will be used for calculating proportions
  sample_size_factor_a<- get_sample_size(opportunity_calc_group_1)

  # Find the columns that start with "imp_" and "sat_" in 'opportunity_calc_group_1' using the 'find_imp_sat_columns' function
  # These columns are assumed to contain the data needed for calculating opportunity scores
  opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)

  # Calculate the opportunity scores for 'opportunity_columns_group_1' using the 'calculate_pop_pct_score' function
  # This function is assumed to calculate opportunity scores based on some criteria
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)

  # Split the 'objective_name' column into two new columns 'imp_sat' and 'objective' in 'opportunity_score_group_1' using the 'split_imp_sat_columns' function
  # This is done to separate the information contained in the 'objective_name' column into two separate columns
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)

  # Calculate the opportunity scores for 'opportunity_score_group_1' using the 'calculate_opportunity_score' function
  # Add a new column 'segment_name' with the value of 'factor_a' and rank the data based on 'opportunity_score'
  # The 'segment_name' column is used to identify the data belonging to 'factor_a' and the 'rank' column is used to order the data based on 'opportunity_score'
  opportunity_score_group_1<- calculate_opportunity_score(opportunity_score_group_1) %>%
    mutate(segment_name=factor_a,
           rank=rank(desc(opportunity_score)))

  # Repeat the same steps for 'factor_b' to create a subset of the data frame for the second factor
  # The data for 'factor_b' is processed separately to keep it distinct from 'factor_a'
  opportunity_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)

  sample_size_factor_b<- get_sample_size(opportunity_calc_group_2)

  opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)

  opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)

  opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)

  opportunity_score_group_2<- calculate_opportunity_score(opportunity_score_group_2) %>%
    mutate(segment_name=factor_b,
           rank=rank(desc(opportunity_score)))

  # Combine 'opportunity_score_group_1' and 'opportunity_score_group_2' into a single data frame
  # This is done to bring together the data for 'factor_a' and 'factor_b' for comparison
  merged_opportunity_data_frame <- rbind(opportunity_score_group_1,opportunity_score_group_2)

  # Reshape the data from long to wide format based on 'objective' and 'segment_name', convert 'objective' to factor,
  # split 'objective' into two new columns 'sourcing_stage' and 'objective', and calculate some new columns
  # The reshaping is done to make the data easier to analyze and the new columns are calculated for further analysis
  importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("sourcing_stage","objective")) %>%
    # More code here...

    # Print the 'importance_satisfaction_opportunity' data frame using the 'print_data_table_compare_2' function
    # Print the 'importance_satisfaction_opportunity' data frame using the 'print_data_table_compare_2' function
    # This function is assumed to print the data frame in a specific format for comparison
    importance_satisfaction_opportunity %>%
    arrange(desc(max_opportunity))%>%
    select(-group_difference_score,-max_opportunity) %>%
    filter(Opportunity_Group!="None") %>%
    select(-contains("rank"),-contains("group"),-contains("Group")) %>%
    print_data_table_compare_2(.,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b)

  # Prepare the data frame for the opportunity graph using the 'prep_data_frame_for_opportunity_graph' function
  # This function is assumed to transform the data frame into a format suitable for creating a graph
  opportunity_graph_data_frame <- prep_data_frame_for_opportunity_graph(importance_satisfaction_opportunity)

  # Get the opportunity score graph for the individual factors using the 'get_opportunity_score_graph_individual' function
  # This function is assumed to create a graph of opportunity scores for the individual factors
  plot <- get_opportunity_score_graph_individual(opportunity_graph_data_frame,factor_a,factor_b)

  # Save the graph as a PNG file using the 'save_yo_file_png_take_file_name' function
  # The file name is created by concatenating the 'deparsed_column_name' and the string "_plot__opportunity_score"
  save_yo_file_png_take_file_name(plot,paste0(deparsed_column_name,"_plot__opportunity_score"))

  # The following lines are commented out, but if they were to be used, they would calculate opportunities by stage for each group and print the data tables
  # opportunities_by_stage_group_1 <- get_opportunities_by_stage(opportunity_score_group_1)
  # print_data_table(opportunities_by_stage_group_1,paste0("opp_by_stage-",factor_a))
  #
  # opportunities_by_stage_group_2 <- get_opportunities_by_stage(opportunity_score_group_2)
  # print_data_table(opportunities_by_stage_group_2,paste0("opp_by_stage-",factor_b))

  # Return the 'importance_satisfaction_opportunity' data frame
  # This data frame contains the calculated opportunity scores and other derived metrics for 'factor_a' and 'factor_b'
  return(importance_satisfaction_opportunity)
}
#### Get Sample Size of Each Factor Group ####
# Function to get the sample size of a data frame
get_sample_size <- function(your_data_frame){
  # Select the columns that start with "imp__" in 'your_data_frame'
  # This is done under the assumption that these columns contain the data of interest
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__")) %>%
    # Select the last column of the resulting data frame
    # This is done under the assumption that the last column contains the most recent or relevant data
    select(last_col()) %>%
    # Filter out the rows where the value in the last column is NA
    # This is done to exclude missing data from the sample size calculation
    filter(!is.na(.)) %>%
    # Get the number of rows in the resulting data frame
    # This is the sample size, i.e., the number of non-missing data points in the last column that starts with "imp__"
    nrow()

  # Return the sample size
  return(sample_size)
}
#### Print Data Table Comparing 2 Factors ####
# Function to print a data table for comparison
# Function to print a data table for comparison
print_data_table_compare_2 <- function(your_data_frame,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b){
  # Define the columns of interest to be selected from the data frame
  columns_of_interest <- c(1:9)

  # Get the number of rows in the data frame
  row_count<- nrow(your_data_frame)

  # Convert the 'deparsed_column_name' to title case and replace underscores with spaces
  segement_name <- str_to_title(str_replace_all(deparsed_column_name,"_"," "))

  # Select the columns of interest from the data frame
  modified_data_frame <- your_data_frame %>%
    select(all_of(columns_of_interest))

  # Rename the 'pct_diff' column to 'pctDiff'
  modified_data_frame <- modified_data_frame %>%
    rename(pctDiff=pct_diff)

  # Replace underscores with spaces in 'objective' and 'sourcing_stage', and assign the 7th and 8th columns to 'factor_a_opp' and 'factor_b_opp'
  modified_data_frame <- modified_data_frame %>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           sourcing_stage=str_to_title(str_replace_all(sourcing_stage,"_"," ")),
           factor_a_opp=.[[7]],
           factor_b_opp=.[[8]])

  # Get the names of the 7th and 8th columns in 'your_data_frame', remove the prefix "opportunity_score_", and convert to title case
  factor_a_opp <- names(your_data_frame[7]) %>%
    str_replace_all(.,"opportunity_score_","") %>%
    str_to_title(.)
  factor_a_opp_name <- factor_a_opp

  factor_b_opp <- names(your_data_frame[8]) %>%
    str_replace_all(.,"opportunity_score_","") %>%
    str_to_title(.)
  factor_b_opp_name <- factor_b_opp

  # Rename the 'sourcing_stage' and 'objective' columns to 'Sourcing Stage' and 'Objective',
  modified_data_frame  <- modified_data_frame %>%
    rename(`Sourcing Stage`=sourcing_stage,
           `Objective`=objective)

  # Rename the 7th and 8th columns to 'factor_a_opp' and 'factor_b_opp', and move the 'pctDiff' column to the end
  modified_data_frame  <- modified_data_frame %>%
    rename(!!factor_a_opp:=factor_a_opp,
           !!factor_b_opp:=factor_b_opp) %>%
    dplyr::relocate(pctDiff,.after = everything())

  # Create a gt table with 'Sourcing Stage' as the group name column and 'Objective' as the row name column
  modified_data_frame  <- modified_data_frame %>%
    gt(groupname_col = "Sourcing Stage",
       rowname_col = "Objective")

  # Add a header to the table with a title and subtitle
  modified_data_frame  <- modified_data_frame %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short, " - ",segement_name)),
      subtitle = html(paste0("Sample: ",factor_a_opp_name," group N = ",sample_size_factor_a," & ",factor_b_opp_name," group N = ",sample_size_factor_b))
    )

  # Add spanners to the table
  modified_data_frame  <- modified_data_frame %>%
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
    )

  # Set some options for the table
  modified_data_frame  <- modified_data_frame %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    )

  # Add a color scale to the 9th and 10th columns
  modified_data_frame  <- modified_data_frame %>%
    data_color(
      columns = 9:10,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    )

  # Add a style to the group name cells and column label cells
  modified_data_frame  <- modified_data_frame %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))

  # Hide the 7th and 8th columns
  modified_data_frame  <- modified_data_frame %>%
    cols_hide(
      columns = c(7:8))

  # Align the 'pctDiff' column to the right
  modified_data_frame  <- modified_data_frame %>%
    cols_align(align = "right",
               columns = "pctDiff")

  # Create a file name for saving the table by concatenating the 'project_name', 'deparsed_column_name', 'file_type', and today's date
  file_name <- paste0(project_name,"-",deparsed_column_name,"-",file_type,lubridate::today(),'.png')

  # Create a backup file name by concatenating the 'backup_file_location', 'project_name', 'deparsed_column_name', 'file_type', and today's date
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-",file_type,"-",lubridate::today(),'.png')

  # Save the table as a PNG file using the 'gtsave' function from the 'gt' package
  gtsave(modified_data_frame,file_name)

  # Save a backup of the table as a PNG file
  gtsave(modified_data_frame,file_name_backup)
}


# Function to create an opportunity score graph for individual factors
get_opportunity_score_graph_individual <- function(data_frame,segment_a,segment_b){
  # Create a scatter plot with 'importance' on the x-axis, 'satisfaction' on the y-axis, 'opportunity' as the color, 'segment' as the shape, and a fixed size
  plot <-  data_frame %>%
    ggplot(aes(x=importance,y=satisfaction,color=opportunity,shape=segment,size=3)) +
    # Add some jitter to the points to avoid overplotting
    geom_jitter(width = 0.025, height = 0.05) +
    # Set the title and labels for the axes and legends
    labs(title=paste0("Opportunities: ",segment_a," vs ",segment_b),subtitle="Under-Served Opportunities >= 10 Opportunity Score ",x="Importance",y="Satisfaction",color="Opportunity\nScore",fill="", size="",shape="Segment") +
    # Set the theme for the plot
    theme(text=element_text(family = "Roboto"),
          panel.grid.major = element_line(color = "#DAE1E7"),
          panel.background = element_blank(),axis.text = element_text(size = 12),
          axis.text.x = element_text(margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 5)),
          axis.title = element_text (size = 15),
          axis.line = element_line(),
          axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
          plot.caption = element_text(size = 8,
                                      margin = margin(t = 10),
                                      color = "#3D4852"),
          title = element_text (size = 15,margin = margin(b = 10)),) +
    # Remove the size legend
    guides(size=FALSE) +
    # Expand the limits of the x and y axes to start at 0
    expand_limits(x=0,y=0) +
    # Add a diagonal line segment annotation from (5, 0) to (10, 10)
    annotate("segment",
             x = 5,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    # Add a text annotation at (7.75, 2) for "Under-Served Objectives"
    annotate("text", x = 7.75, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Under-Served\nObjectives")) +
    # Add a diagonal line segment annotation from (0, 0) to (10, 10)
    annotate("segment",
             x = 0,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    # Add a text annotation at (3, 2) for "Appropriately-Served Objectives"
    annotate("text", x = 3, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Appropriately-Served\nObjectives")) +
    # Add a diagonal line segment annotation from (0, 0) to (10, 10)
    annotate("segment",
             x = 0,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    # Add a text annotation at (0.15, 2) for "Over-Served Objectives"
    annotate("text", x = 0.15, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Over-Served\nObjectives")) +
    # Add a horizontal line segment annotation at y = 7.5
    annotate("segment",
             x = 0,
             xend=10,
             y = 7.5,
             yend=7.5,
             color = "#3D4852") +
    # Add a text annotation at (1, 7.75) for "Table Stakes"
    annotate("text", x = 1, y = 7.75,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Table Stakes")) +
    # Set the scale for the x-axis and remove the expansion of the scale
    scale_x_continuous(expand=c(0,0)) +
    # Set the scale for the y-axis and remove the expansion of the scale
    scale_y_continuous(expand=c(0,0)) +
    # Set the limits for the x-axis to be from 0 to 10
    coord_cartesian(xlim=c(0,10))

  # Return the plot
  return(plot)
}

# Function to save a plot as a PNG file
save_yo_file_png_take_file_name <- function(plot,file_name){
  # Save the 'plot' as a PNG file using the 'ggsave' function from the 'ggplot2' package
  # The file name is created by concatenating the 'project_name', 'file_name', and today's date
  # The width and height of the output image are set to 15 and 10, respectively
  ggsave(paste0(project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)

  # The following line is commented out, but if it were to be used, it would save the 'plot' as a PNG file in a specific directory
  # ggsave(paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)
}
