#' Takes takes a dataframe of importance and satisfaction columns in this format "imp__job_step.job_name" & "sat__job_step.job_name" and calculates the importance, satisfaction, and opportunity scores for each. Currently it also prints those elements, but I need to split those into separate functions.
#'
#' @param data this is the dataframe you want, needs LABELS from importing via Haven
#'
#' @return
#' @export
#'
#' @examples
#' get_imp_sat_opp_scores_total_population(data = "importance_satisfaction_datafraome")
#### Get Total Population Importance, Satisfaction, and Opportunity Scores ####
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
library('gt')
print_data_table_general_population <- function(your_data_frame,deparsed_column_name){
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
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short)),
      subtitle = html(paste0("Sample: ",sample_description))
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    tab_style(
      style = cell_text(color = "green",weight = "normal"),
      locations = list(
        cells_body(
          columns = vars(`Opportunity Score`),
          rows = `Opportunity Score` >= 10
        ))) %>%
    tab_style(
      style = cell_text(color = "darkgreen",weight = "bold"),
      locations = list(
        cells_body(
          columns = vars(`Opportunity Score`),
          rows = `Opportunity Score` >= 12
        ))) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))
  file_name <- paste0(project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
}



get_imp_sat_opp_scores_compare_2 <- function(your_data_frame,column_to_split_on,factor_a,factor_b) {
  file_type <- "opportunity_list_table"
  opportunity_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  sample_size_factor_a<- get_sample_size(opportunity_calc_group_1)

  opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)

  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)

  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)

  opportunity_score_group_1<- calculate_opportunity_score(opportunity_score_group_1) %>%
    mutate(segment_name=factor_a,
           rank=rank(desc(opportunity_score)))

  opportunity_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)

  sample_size_factor_b<- get_sample_size(opportunity_calc_group_2)

  opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)

  opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)

  opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)

  opportunity_score_group_2<- calculate_opportunity_score(opportunity_score_group_2) %>%
    mutate(segment_name=factor_b,
           rank=rank(desc(opportunity_score)))

  merged_opportunity_data_frame <- rbind(opportunity_score_group_1,opportunity_score_group_2)

  importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("sourcing_stage","objective"))
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name=V2) %>%
    pluck(.,1)

  importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
    mutate(group_difference=.[[7]]-.[[8]],
           group_difference_score=sum(abs(group_difference)),
           Opportunity_Group=case_when(.[[7]]>=10&.[[8]]>=10~paste0(factor_a,"__",factor_b),
                                       .[[7]]>=10&.[[8]]<10~factor_a,
                                       .[[7]]<10&.[[8]]>=10~factor_b,
                                       TRUE~"None")) %>%
    mutate(max_opportunity=if_else(.[[7]]>.[[8]],.[[7]],.[[8]])) %>%
    mutate_if(is.numeric,round,1) %>%
    arrange(max_opportunity) %>%
    mutate(objective=factor(objective, levels=objective),
           pct_diff=abs(group_difference/max_opportunity),
           segmentation_factor=deparsed_column_name,
           pct_diff=scales::percent(pct_diff,accuracy = 2))

  importance_satisfaction_opportunity %>%
    arrange(desc(max_opportunity))%>%
    select(-group_difference_score,-max_opportunity) %>%
    filter(Opportunity_Group!="None") %>%
    select(-contains("rank"),-contains("group"),-contains("Group")) %>%
    print_data_table_compare_2(.,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b)

  opportunity_graph_data_frame <- prep_data_frame_for_opportunity_graph(importance_satisfaction_opportunity)

  plot <- get_opportunity_score_graph_individual(opportunity_graph_data_frame,factor_a,factor_b)
  save_yo_file_png_take_file_name(plot,paste0(deparsed_column_name,"_plot__opportunity_score"))

  # opportunities_by_stage_group_1 <- get_opportunities_by_stage(opportunity_score_group_1)
  # print_data_table(opportunities_by_stage_group_1,paste0("opp_by_stage-",factor_a))
  #
  # opportunities_by_stage_group_2 <- get_opportunities_by_stage(opportunity_score_group_2)
  # print_data_table(opportunities_by_stage_group_2,paste0("opp_by_stage-",factor_b))

  return(importance_satisfaction_opportunity)
}

get_sample_size <- function(your_data_frame){
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__"))%>%
    select(last_col())%>%
    filter(!is.na(.))%>%
    nrow()
  return(sample_size)
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
  file_name <- paste0(project_name,"-",deparsed_column_name,"-",file_type,lubridate::today(),'.png')
  file_name_backup <- paste0(backup_file_location,project_name,"-",deparsed_column_name,"-",file_type,"-",lubridate::today(),'.png')
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
}


prep_data_frame_for_opportunity_graph <- function(data_frame){
  imp_long <- data_frame %>%
    select(sourcing_stage,objective,starts_with("imp_"),starts_with("sat_")) %>%
    pivot_longer(cols=c(starts_with("imp_")),values_to = "rating",names_to = c("imp")) %>%
    select(sourcing_stage,objective,imp,rating) %>%
    separate(imp,into = c("imp","segment"),sep="\\_",extra = "merge")

  sat_long <- data_frame %>%
    select(sourcing_stage,objective,starts_with("imp_"),starts_with("sat_")) %>%
    pivot_longer(cols=c(starts_with("sat_")),values_to = "rating",names_to = c("sat")) %>%
    select(sourcing_stage,objective,sat,rating) %>%
    separate(sat,into = c("sat","segment"),sep="\\_",extra = "merge")


  data_frame_ready_for_graph <- imp_long %>%
    left_join(sat_long, by=c("objective","segment")) %>%
    select(-imp,-sat) %>%
    transmute(objective,
              importance=rating.x,
              satisfaction=rating.y,
              segment,
              opportunity=if_else(importance>satisfaction,importance+importance-satisfaction,importance),
              opp_flag=if_else(opportunity>=10,1,0))

  return(data_frame_ready_for_graph)
}


get_opportunity_score_graph_individual <- function(data_frame,segment_a,segment_b){
  plot <-  data_frame %>%
    ggplot(aes(x=importance,y=satisfaction,color=opportunity,shape=segment,size=3))+geom_jitter(width = 0.025, height = 0.05) +
    labs(title=paste0("Opportunities: ",segment_a," vs ",segment_b),subtitle="Under-Served Opportunities >= 10 Opportunity Score ",x="Importance",y="Satisfaction",color="Opportunity\nScore",fill="", size="",shape="Segment")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
    theme(text=element_text(family = "Roboto"),
          panel.grid.major = element_line(color = "#DAE1E7"),
          panel.background = element_blank(),axis.text = element_text(size = 12),
          axis.text.x = element_text(margin = margin(t = 5)),#hjust = 1,angle=90
          axis.text.y = element_text(margin = margin(r = 5)),
          axis.title = element_text (size = 15),
          axis.line = element_line(),
          axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
          plot.caption = element_text(size = 8,
                                      margin = margin(t = 10),
                                      color = "#3D4852"),
          title = element_text (size = 15,margin = margin(b = 10)),) +
    guides(size=FALSE) +
    expand_limits(x=0,y=0) +
    annotate("segment",
             x = 5,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    annotate("text", x = 7.75, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Under-Served\nObjectives")) +
    annotate("segment",
             x = 0,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    annotate("text", x = 3, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Appropriately-Served\nObjectives")) +
    annotate("segment",
             x = 0,
             xend=10,
             y = 0,
             yend=10,
             color = "#3D4852") +
    annotate("text", x = 0.15, y = 2,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Over-Served\nObjectives")) +
    annotate("segment",
             x = 0,
             xend=10,
             y = 7.5,
             yend=7.5,
             color = "#3D4852") +
    annotate("text", x = 1, y = 7.75,
             hjust = 0,
             color = "#3D4852",
             size = 3.7,
             label = paste0("Table Stakes")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_cartesian(xlim=c(0,10))
  return(plot)
}

save_yo_file_png_take_file_name <- function(plot,file_name){
  ggsave(paste0(project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)
  #ggsave(paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)
}
