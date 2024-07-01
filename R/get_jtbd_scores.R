
#### JTBD FUNCTION ####

#' Calculate the importance, satisfaction, and opportunity scores
#'
#' @param your_data_frame your dataframe
#' @param segment_var this is the text string, within parentheses, that will be appended to each of the rows, we typically use "all"
#'
#' @return a data frame of scores
#' @export
#'
#' @examples
#' #' get_jtbd_scores(1,"all")
#### JTBD FUNCTION ####

get_jtbd_scores <- function(your_data_frame,segment_var){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp)) %>%
    mutate(segment_name=segment_var,
           rank=rank(desc(opp)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = round(opp/median(opp),2))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opp,rank,opp_index),
                names_sep = ".") %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    mutate_if(is.numeric,round,2) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"

  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)

  return(importance_satisfaction_opportunity)
}


find_imp_sat_columns <- function(your_data_frame){
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns,sat_columns)
  return(data_frame_imp_sat)
}

# * Calculate Importance & Satisfaction-----------------------------------
individual_data <- NULL
all_data <- NULL
calculate_pop_pct_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(objective_name,user_rating,n)

    print(objective_score_tibble)

    individual_data <-  objective_score_tibble %>%
      summarize(objective_name=unique(objective_name),
                total_sum=sum(n),
                imp_sat_sum=sum(n[user_rating==5|
                                    user_rating==4]))  %>%
      mutate(imp_sat_score=((imp_sat_sum/total_sum)*10))
    print(individual_data)
    all_data <- rbind(all_data,individual_data)

  }
  return(all_data)
}

# ** Split Imp/Sat into Columns -------------------------------------------
split_imp_sat_columns <- function(data_frame_imp_sat){
  data_frame_imp_sat_split <-  data_frame_imp_sat %>%
    separate(objective_name,"__",into = c("imp_sat","objective"),remove = FALSE)
  return(data_frame_imp_sat_split)
}


calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp))

  return(opportunity_scores)
} <- function(your_data_frame,segment_var){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp)) %>%
    mutate(segment_name=segment_var,
           rank=rank(desc(opp)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = round(opp/median(opp),2))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opp,rank,opp_index),
                names_sep = ".") %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    mutate_if(is.numeric,round,2) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"

  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)

  return(importance_satisfaction_opportunity)
}


find_imp_sat_columns <- function(your_data_frame){
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns,sat_columns)
  return(data_frame_imp_sat)
}

# * Calculate Importance & Satisfaction-----------------------------------
individual_data <- NULL
all_data <- NULL
calculate_pop_pct_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(objective_name=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(objective_name,user_rating,n)

    print(objective_score_tibble)

    individual_data <-  objective_score_tibble %>%
      summarize(objective_name=unique(objective_name),
                total_sum=sum(n),
                imp_sat_sum=sum(n[user_rating==5|
                                    user_rating==4]))  %>%
      mutate(imp_sat_score=((imp_sat_sum/total_sum)*10))
    print(individual_data)
    all_data <- rbind(all_data,individual_data)

  }
  return(all_data)
}

# ** Split Imp/Sat into Columns -------------------------------------------
split_imp_sat_columns <- function(data_frame_imp_sat){
  data_frame_imp_sat_split <-  data_frame_imp_sat %>%
    separate(objective_name,"__",into = c("imp_sat","objective"),remove = FALSE)
  return(data_frame_imp_sat_split)
}


calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opp=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opp))

  return(opportunity_scores)
}




#### Summary Stats ####
get_count <- function(data_frame, col_name) {
  count <- data_frame %>%
    mutate(col_name = labelled::to_factor(col_name)) %>%
    group_by(col_name) %>%
    count()
  return(count)
}

get_sample_size <- function(your_data_frame) {
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__")) %>%
    select(last_col()) %>%
    filter(!is.na(.)) %>%
    nrow()
  return(sample_size)
}
