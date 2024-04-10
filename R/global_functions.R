# table styling
gt_theme.yellow <- function(gt_object, ...) {
  
  # get id, if one is passed through to use with CSS
  # table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]
  
  table <- gt_object |> 
    data_color(columns = where(is.numeric),
               # colors = scales::col_numeric(
               #   palette = paletteer::paletteer_d(
               #     palette = "ggsci::yellow_material"
               #   ) %>% as.character(),
               #  domain = c(10.5, 6))
               colors = scales::col_numeric(
                 palette = c(
                   "white", "orange"),
                 domain = NULL)
    ) |>
    # set table font
    gt::opt_table_font(
      font = list(
        gt::google_font('Spline Sans Mono'),
        gt::default_fonts()
      ),
      weight = 500
    ) |> 
    # set the column label font and style
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 650,
        size = px(14),
        transform = 'uppercase', # column labels to uppercase
        align = 'left'
      )
    ) |> 
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 650
      )
    ) |> 
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 500
      )
    ) |>
    # set think black column sep.
    gt::tab_style(
      style = gt::cell_borders(sides = 'left', weight = px(0.5), color = 'black'),
      locations = gt::cells_body(
        # everything but the first column
        columns = c(-names(gt_object[['_data']])[1])
      )
    ) |> 
    # set thin dotted row sep.
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = 'black', weight = px(1.5), style = 'solid'),
      locations = gt::cells_body(
        rows = gt::everything()
      )
    )|>
    # left align cell text
    gt::cols_align(
      align = 'left',
      columns = gt::everything()
    ) |> 
    gt::tab_options(
      table.font.size = 14,
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = 'black',
      column_labels.border.top.color = 'white',
      row_group.border.bottom.color = 'white',
      table.border.top.style = 'none',
      table.border.bottom.style = 'none',
      heading.border.bottom.style = 'none',
      heading.align = 'left',
      heading.title.font.size = px(30),
      source_notes.border.lr.style = 'none',
      source_notes.font.size = 10
    )
  
  # # add css if table id is passed through
  # table <- if(!is.null(table_id)) {
  #   table |> 
  #     # remove the border from the bottom cell
  #     gt::opt_css(
  #       paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
  #       add = TRUE
  #     )
  #}
  
  return(table)
  
}


## Batch table creation
get_imp_sat_opp_scores.loop <- function(your_data_frame,column_to_split_on,factor_a,factor_b) {
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
    separate(objective,sep="([.])",into= c("job_step","objective"))
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name=V2) %>%
    pluck(.,1)
  importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
    mutate(objective=factor(objective, levels=objective)) %>%
            mutate_if(is.numeric,round,1)


  # importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
  # #  mutate(#imp_diff=.[[3]]-.[[4]],
  #          #sat_diff=.[[5]]-.[[6]],
  #          #group_difference=.[[7]]-.[[8]],
  #         # group_difference_score=sum(abs(group_difference)),
  #         # Opportunity_Group=case_when(.[[7]]>=10&.[[8]]>=10~paste0(factor_a,"__",factor_b),
  #                                   #   .[[7]]>=10&.[[8]]<10~factor_a,
  #                       #               .[[7]]<10&.[[8]]>=10~factor_b,
  #                        #              TRUE~"None")) %>%
  #  # mutate(max_opportunity=if_else(.[[7]]>.[[8]],.[[7]],.[[8]])) %>%
  #   mutate_if(is.numeric,round,1) %>%
  #  # arrange(max_opportunity) %>%
  #   mutate(objective=factor(objective, levels=objective),
  #          #pct_diff=abs(group_difference/max_opportunity),
  #          segmentation_factor=deparsed_column_name,
  #        #  pct_diff=scales::percent(pct_diff,accuracy = 2),
  #          sample_size_factor_a=sample_size_factor_a,
  #          sample_size_factor_b=sample_size_factor_b)  %>%
  #   # Cal the difference between each groups imp & satisfaction
  #  mutate(imp_diff=.[[3]]-.[[4]],
  #         sat_diff=.[[5]]-.[[6]],
          # segment_a=factor_a,
    #      #  segment_b=factor_b) %>%
    # mutate(ave_imp_slope=round(mean(imp_diff),2),
    #        ave_sat_slope=round(mean(sat_diff),2),
    #        total_imp_diff=sum(imp_diff),
    #        total_sat_diff=sum(sat_diff))# %>%
  # measure the difference between group AT SEGMENT LEVEL & suggest
  # mutate(indv_slope_cat=case_when(
  #   imp_diff>=.3&sat_diff>=.3~"Catch Up",
  #   imp_diff>=.3&(sat_diff<=.15&sat_diff>=-.15)~"More Performance",
  #   # sat_diff>=.3&imp_diff>=.15&imp_diff>=-.15~"More Perf",
  #   sat_diff>=.3&(imp_diff<=.15&imp_diff>=-.15)~"Catch Up",
  #   imp_diff>=.3&sat_diff<=-.15~"INVEST",
  #   imp_diff<=.3&sat_diff<=.3~"Satisfice",
  #   imp_diff<=-.15&sat_diff<=-.15~"Satisfice",
  #   TRUE ~ "BROKEN")) %>%
  # # measure the difference between group AT SEGMENT LEVEL & suggest
  # mutate(seg_slope_cat=case_when(
  #   ave_imp_slope>=.3&ave_sat_slope>=.3~"Catch Up",
  #   ave_imp_slope>=.3&(ave_sat_slope<=.15&ave_sat_slope>=-.15)~"More Performance",
  #   ave_sat_slope>=.3&(ave_imp_slope<=.15&ave_imp_slope>=-.15)~"Catch Up",
  #   ave_imp_slope>=.3&ave_sat_slope<=-.15~"Invest",
  #   ave_imp_slope<=.3&ave_sat_slope<=.3~"Satisfice",
  #   ave_imp_slope<=-.15&ave_sat_slope<=-.15~"Satisfice",
  #   TRUE ~ "BROKEN"))

  # temp_title <- paste0(factor_a," vs ",factor_b)
  #
  # get_jtbd_segment.compare_2.table(importance_satisfaction_opportunity,temp_title)
  # get_opportunity_score_graph.compare_2(importance_satisfaction_opportunity,factor_a,factor_b)
  return(importance_satisfaction_opportunity)
}
# Master table
get_jtbd_scores.master_table <- function(master_table,data_frame,merged_df_and_segmentation_column,static_segment,list_of_unique_segments){

  for (i in 1:length(list_of_unique_segments)) {

    section  <- deparse(substitute(list_of_unique_segments))
    df_name <- list_of_unique_segments[i]
    df_name <- df_name %>%
      str_replace_all(.," ","_")
    df_name <- paste0("df.",section,".",df_name)
print(df_name)
    df_of_scores <- get_imp_sat_opp_scores.loop(data_frame,merged_df_and_segmentation_column,static_segment,list_of_unique_segments[i])

      assign(df_name,df_of_scores, envir = .GlobalEnv)
truncated_list <- df_of_scores %>%
      select(objective,starts_with("opportunity"))

      master_table <-  master_table %>%
       left_join(truncated_list)


  }

  return(master_table)
}


get_quick_count <- function(data_frame,question){
  data_frame %>%
    select(!!question) %>%
    sjlabelled::label_to_colnames() %>%
    mutate_if(haven::is.labelled, haven::as_factor) %>%
    group_by(.[[1]])%>%
    count() %>%
    view()
}





# adjust opportunity score with exponent

adjust.opp_score <- function(data_frame,exponent){

  df_working <- data_frame %>%
    select(c(1:2),7) %>%
    mutate(opp_score.adj =.[[3]]^exponent)



  #%>%
   # mutate(opp_score.adj.normalized=opp_score.adj/(12^exponent))

  #         max_opp_score.adj=max(opp_score.adj),
  # min_opp_score.adj=min(opp_score.adj)

  # opp_score.norm= ((opp_score.adj - min_opp_score.adj) *(9/(max_opp_score.adj - min_opp_score.adj))+1))


 return(df_working)
}


# Comparing Segments
## Build the individual segment score
build_group_score.segmentation <- function(total_pop,original_df,new_table){
  median_opp_sum <- total_pop %>%
    select(max_value) %>%
    rename(vs_total=max_value)

  median_opp_9s <- total_pop %>%
    select(nines) %>%
    rename(median_opp_9s=nines)
  median_opp_10s <- total_pop %>%
    select(tens) %>%
    rename(median_opp_10s=tens)
  median_summed_over_9 <- total_pop %>%
    select(summed_over_9) %>%
    rename(median_summed_over_9=summed_over_9)

 new_table %>%
    group_by(segmentation_factor)%>%
    mutate(num_opportunity=n()) %>%
    mutate(max_value=sum(max_opportunity),
           value_over_9 =max_opportunity-9,
           value_over_9 =case_when(
             value_over_9>0~value_over_9,
             TRUE~0),
           summed_over_9=sum(value_over_9),
           vs_med_over_9.indx=round(summed_over_9/median_summed_over_9$median_summed_over_9,2),
           value_over_10 =max_opportunity-10,
           value_over_10 =case_when(
             value_over_10>0~value_over_10,
             TRUE~0),
           summed_over_10=sum(value_over_10),
           nines=sum(max_opportunity>8.9),
           tens=sum(max_opportunity>9.9),
           nines_per_opp=round(nines/num_opportunity,2),
           tens_per_opp=round(tens/num_opportunity,2),
           summed_over_9_per_opp=round(summed_over_9/num_opportunity,2),
           summed_over_10_per_opp=round(summed_over_10/num_opportunity,2),
           pct_over_median=paste(100*round(1-(median_opp_sum/max_value),2),"%"),
           vs_med.9s.indx=paste(100*round(1-(median_opp_9s/nines),2),"%"),
           vs_med.10s.indx=paste(100*round(1-(median_opp_10s/tens),2),"%"),
           ) %>%
    select(segmentation_factor,group_difference_score,nines,vs_med.9s.indx,tens,vs_med.10s.indx,max_value,pct_over_median,summed_over_9,vs_med_over_9.indx,summed_over_10,num_opportunity,nines_per_opp,tens_per_opp,summed_over_9_per_opp,summed_over_10_per_opp) %>%
    # Write code to rename segmentation factor with the segment so we know which segment this represents!

    filter(row_number()==1) %>%
    bind_rows(original_df)
}

# Build the overall-score by which you compare all other segements:
# table.all_jobs.no_segmentation <- build_group_score.no_segmentation(table.all_jobs.calculated)
# # Measure Which "Interperatable" Segmentation has the Highest Opportunities & Greatest Difference
# table.all_jobs.segment_comparison <- NULL
# table.all_jobs.segment_comparison <- build_group_score.segmentation(table.all_jobs.no_segmentation,table.all_jobs.segment_comparison,table.all_jobs.prod_serv)
build_group_score.no_segmentation <- function(df){
  df %>%
    group_by(objective)%>%
    mutate(num_opportunity=n()) %>%
    ungroup() %>%
    mutate(num_opportunity=sum(num_opportunity)) %>%
    mutate(segmentation_factor="all_jobs.no_segmentation",
           group_difference_score=0,
           max_value=sum(opportunity_score),
           value_over_9 =opportunity_score-9,
           value_over_9 =case_when(
             value_over_9>0~value_over_9,
             TRUE~0),
           summed_over_9=sum(value_over_9),
           value_over_10 =opportunity_score-10,
           value_over_10 =case_when(
             value_over_10>0~value_over_10,
             TRUE~0),
           summed_over_10=sum(value_over_10),
           nines=sum(opportunity_score>8.9),
           tens=sum(opportunity_score>9.9),
           nines_per_opp=round(nines/num_opportunity,2),
           tens_per_opp=round(tens/num_opportunity,2),
           summed_over_9_per_opp=round(summed_over_9/num_opportunity,2),
           summed_over_10_per_opp=round(summed_over_10/num_opportunity,2)) %>%
    select(segmentation_factor,group_difference_score,nines,tens,max_value,summed_over_9,summed_over_10,num_opportunity,nines_per_opp,tens_per_opp,summed_over_9_per_opp,summed_over_10_per_opp) %>%
    filter(row_number()==1)
}


rename_spss_col_imp_sat <- function(df,unique_string_the_column_name_starts_with,job_section,imp_or_sat_string){
  df <- convert_labels_to_row_names(df)
  df_working.1 <- select_columns_by_type(df,unique_string_the_column_name_starts_with)
  df_working.2 <- modify_col_names(df_working.1)
  df_working.3 <- add_col_prefix(df_working.2,imp_or_sat_string,job_section)
  df <- df_working.3
  #df <- merge_named_cols_with_data_set(df,df_working.3)
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

#### Next Problem ####


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
}


#' Takes SPSS Data that has a label format of "XXXX "Job Name" - "Something you don't want" and convert it the column name and strip everything except the "Job_Name"... It has to be in that format
#'
#' @param data this is the dataframe you want, needs LABELS from importing via Haven
#' @param job_section name of job section
#'
#' @return Dataframe with updated column names
#' @export
#'
#' @examples
#' build_imp_column_names(df,"job_step")



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

# Swap the labels and the column names
convert_labels_to_row_names<- function(df) {
  df <- df %>%
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









# Likelihood to Renew: Error Bars - Adjusted Wald -------------------------
get_error_bars_adjusted_wald <- function(your_data_frame){
  upper_lower_bands <- NULL
  lower_bound <- NULL
  upper_bound <- NULL
  for (i in 1:nrow(your_data_frame)){
    lower_bound[i] <- proportion::ciAWDx(your_data_frame$ltr_count[i],your_data_frame$sample_size[i],alp =0.1,h=2)%>%pull(var=2)
    upper_bound[i] <- proportion::ciAWDx(your_data_frame$ltr_count[i],your_data_frame$sample_size[i],alp =0.1,h=2)%>%pull(var=3)
    upper_lower_bands <- rbind(upper_lower_bands,cbind(lower_bound[i],upper_bound[i])) %>% as_tibble()
  }

  upper_lower_bands <- upper_lower_bands %>%
    select(ltr_lower_bound=V1,ltr_upper_bound=V2)

  ltr_data_frame <- cbind(your_data_frame,upper_lower_bands)
  return(ltr_data_frame)
}

# Analysis ----------------------------------------------------------------

# * Net Promoter Score ----------------------------------------------------
get_nps_score <- function(your_data_frame){
  nps_score_df <- your_data_frame %>%
    filter(!is.na(nps_groups)) %>%
    select(net_promoter_score,nps_groups) %>%
    group_by(nps_groups) %>%
    count %>%
    ungroup() %>%
    mutate(total_sum=sum(n),
           nps_group_sum=n,
           nps_pct=nps_group_sum/total_sum)
  nps_score <- round((nps_score_df$nps_pct[3]-nps_score_df$nps_pct[1])*100,0)
  nps_sample_size <- nps_score_df$total_sum
  plot_nps_score(your_data_frame,nps_score,nps_sample_size)
  return(nps_score)
}

# * Get NPS Score Batch ---------------------------------------------------
get_sat_score_batch <- function(your_data_frame){
  sat_data_frame <- your_data_frame %>%
    select(starts_with("sat__"))%>%
    calculate_pop_pct_score(.) %>%
    mutate(sat_pct=round((imp_sat_score*10),0)) %>%
    select(objective_name,sat_pct) %>%
    mutate_if(is.character,~str_replace_all(.,pattern = "_platform_value.",replacement = "")) %>%
    mutate_if(is.character,~str_replace_all(.,pattern = "_onboarding.",replacement = "")) %>%
    pivot_wider(names_from = objective_name,values_from = sat_pct)  %>%
    mutate(survey_name=project_name)
  return(sat_data_frame)
}

# * Master NPS Score Function ---------------------------------------------


# * Get Sample Size -------------------------------------------------------
get_sample_size_general <- function(your_data_frame,column_filter){
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with(column_filter))%>%
    select(last_col())%>%
    filter(!is.na(.))%>%
    nrow()
  return(sample_size)
}

# * Get Sample Size: Importance -------------------------------------------
get_sample_size <- function(your_data_frame){
  sample_size <- your_data_frame %>%
    select(dplyr::starts_with("imp__"))%>%
    select(last_col())%>%
    filter(!is.na(.))%>%
    nrow()
  return(sample_size)
}
# * Opportunity Functions --------------------------------------------------
# ** Find Importance Columns ----------------------------------------------
find_imp_sat_columns <- function(your_data_frame){
  imp_columns <- your_data_frame %>%
    select(starts_with("imp_"))
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  data_frame_imp_sat <- cbind(imp_columns,sat_columns)
  return(data_frame_imp_sat)
}

# ** Find SAT columns -----------------------------------------------------
find_sat_columns <- function(your_data_frame){
  sat_columns <- your_data_frame %>%
    select(starts_with("sat_"))
  return(sat_columns)
}

# Calculate Satisfaction --------------------------------------------------
calculate_satisfaction_score <- function(your_data_frame){
  sat_columns <- find_sat_columns(your_data_frame)
  satisfaction_scores <- calculate_pop_pct_score(sat_columns)
  return(satisfaction_scores)
}

# ** Calculate Importance & Satisfaction ----------------------------------
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

# ** Calculate Opportunity Score ------------------------------------------
calculate_opportunity_score <- function(data_frame_split) {
  opportunity_scores <- data_frame_split %>%
    pivot_wider(objective,
                names_from = c(imp_sat),
                values_from = imp_sat_score) %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score))

  return(opportunity_scores)
}

# ** Opportunity - Total Population ---------------------------------------
get_jtbd_scores <- function(your_data_frame){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)

# Formatting
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score)) #%>%
    # mutate("Segmentation Factor"="None",
    #        rank=rank(desc(Opportunity)))
# Calculate Index
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate("Opp Index" =scales::percent(opportunity_score/median(opportunity_score),accuracy = 2))
# More formatting
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective"))  %>%
    mutate_if(is.numeric,round,1) %>%
    mutate(objective=factor(objective, levels=objective)) %>%
    mutate("Imp vs Sat"=imp-sat) %>%
    # mutate(ave_slope=mean(imp_sat_direction) %>%

  # measure the difference between group AT SEGMENT LEVEL & suggest
    mutate(Recommendation=case_when(
      opportunity_score>=10~"Prioritize",
      opportunity_score>=9~"Consider",
      opportunity_score>=7.5&sat>7.5~"Catch up",
      opportunity_score<9&sat>7.5~"Segment",
      sat>imp~"Simplify",
      opportunity_score<9~"Segment",
      TRUE ~ "BROKEN"))
    # measure the difference between group AT SEGMENT LEVEL & suggest

  return(opportunity_score_group_1)
}

# Fresh

get_jtbd_scores.table <- function(your_data_frame,graph_title,sample_description){
  # ADD IN SAMPLE calculator
  your_table <- your_data_frame %>%
    make_opps_columns_legible %>%
    gt(groupname_col = "Job Step",
       rowname_col = "Objective")%>%
    cols_align(
      align = "left",
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',graph_title)),
      subtitle = html(paste0("Sample: ",sample_description))
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    data_color(
      columns = 5,
      rows = Opportunity >= 9,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL)) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))


  file_name <- paste0(project_name,"-",graph_title,"-",lubridate::today(),'.png')
  gt::gtsave(your_table,file_name)
  return(your_table)
}

#

get_jtbd_segment.comparison.table <- function(your_data_frame,graph_title,sample_description){
  # ADD IN SAMPLE calculator
  your_table <- your_data_frame %>%
  filter(!is.na(vs_med.9s.indx))%>%
    select(segmentation_factor,group_difference_score,vs_med.9s.indx,summed_over_9) %>%
    rename("Segmentation"=segmentation_factor,
           "Segment Diff"=group_difference_score,
           "Agr HV Opp vs Index"=vs_med.9s.indx,
           "Agr HV Opp"=summed_over_9
    ) %>%
    remove_weird_text_formatting %>%
    gt(
      groupname_col = "segmentation_factor",
      rowname_col =
    ) %>%
    cols_align(
      align = "left",
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste0('Segmentation: ',graph_title)),
      subtitle = html(paste0("Sample: ",sample_description))
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    data_color(
      columns = 4,
      # rows = Opportunity >= 9,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL)) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))

    # tab_footnote(
    #   footnote = "Stat sig",
    #   locations = cells_body(columns =  "Agr HV Opp", rows = 3:4))

  file_name <- paste0(project_name,"-",graph_title,"-",lubridate::today(),'.png')
  gt::gtsave(your_table,file_name)
  return(your_table)
}

# Fresh

get_jtbd_segment.compare_2.table <- function(your_data_frame,graph_title){
  sample_size.a <- pluck(your_data_frame$sample_size_factor_a[1])
  sample_size.b <- pluck(your_data_frame$sample_size_factor_b[1])
  segment_a <- pluck(your_data_frame$segment_a[1])
  segment_b <- pluck(your_data_frame$segment_b[1])

  # ADD IN SAMPLE calculator
  your_table <- your_data_frame %>%
    filter(max_opportunity>9)%>%
    arrange(desc(max_opportunity))%>%
    select(-group_difference_score,-Opportunity_Group,-starts_with("rank"),-starts_with("imp"),-starts_with("sat"),-segmentation_factor,-starts_with("sample"),-starts_with("segment"),-starts_with("ave"),-starts_with("total"),-ends_with("cat"),imp_diff,sat_diff)%>%
    remove_weird_text_formatting %>%
    gt(
      # groupname_col = "segmentation_factor",
      rowname_col =
    ) %>%
    cols_align(
      align = "left",
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste0('Segmentation: ',graph_title)),
      subtitle = html(paste0("Sample: ",segment_a, ": n=", sample_size.a," | ",segment_b, ": n=", sample_size.b))
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    data_color(columns = 3:4,
               colors = scales::col_numeric(
                 palette = paletteer::paletteer_d(
                   palette = "colorBlindness::Blue2Orange10Steps"
                 ) %>% as.character(),
                 domain = c(11.7, 0))) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything())))

  # tab_footnote(
  #   footnote = "Stat sig",
  #   locations = cells_body(columns =  "Agr HV Opp", rows = 3:4))
  file_name <- paste0(project_name,"-",graph_title,"-",lubridate::today(),'.png')
  gt::gtsave(your_table,file_name)
  return(your_table)
}



get_jtbd_scores.no_segmentation <- function(your_data_frame,df_description){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score)) %>%
    mutate(segment_name=!!df_description,
           rank=rank(desc(opportunity_score)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = paste0(round(opportunity_score/median(opportunity_score),2)*100,"%"))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank,opp_index)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    # mutate(max_opportunity=rank()) %>%
    # arrange(max_opportunity) %>%
    mutate_if(is.numeric,round,1) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"

  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)

  return(importance_satisfaction_opportunity)
}

print_table.no_segmentation <- function(your_data_frame,df_description){
  modified_data_frame <- your_data_frame %>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           job_step=str_to_title(str_replace_all(job_step,"_"," ")))  %>%
  modified_data_frame.part_2 <-  modified_data_frame %>%
    # select(-rank_total_population) %>%
    gt(groupname_col = "Sourcing Stage",
       rowname_col = "Objective") %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',df_description)),
      subtitle = html(paste0("Sample: ",sample_description))
    )
  file_name <- paste0(project_name,"-",df_description,"-table-",lubridate::today(),'.png')
  gtsave(modified_data_frame.part_2,file_name)
}

#
get_imp_sat_opp_scores_total_population <- function(your_data_frame){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score)) %>%
    mutate(segment_name="total_population",
           rank=rank(desc(opportunity_score)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = paste0(round(opportunity_score/median(opportunity_score),2)*100,"%"))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank,opp_index)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    # mutate(max_opportunity=rank()) %>%
    # arrange(max_opportunity) %>%
    mutate_if(is.numeric,round,1) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"

  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)

  return(importance_satisfaction_opportunity)
}
# ** COMPARE 2 IMP SAT ----------------------------------------------------
# Fresh
get_imp_sat_opp_scores.compare_2.further_categorization <- function(your_data_frame){
  df_new <- your_data_frame %>%
    # Cal the difference between each groups imp & satisfaction
    mutate(imp_diff=.[[3]]-.[[4]],
           sat_diff=.[[5]]-.[[6]],
           ) %>%
    mutate(ave_imp_slope=round(mean(imp_diff),2),
           ave_sat_slope=round(mean(sat_diff),2),
           total_imp_diff=sum(imp_diff),
           total_sat_diff=sum(sat_diff)) %>%
    # measure the difference between group AT SEGMENT LEVEL & suggest
    mutate(indv_slope_cat=case_when(
      imp_diff>=.3&sat_diff>=.3~"Catch Up",
      imp_diff>=.3&(sat_diff<=.15&sat_diff>=-.15)~"More Performance",
      # sat_diff>=.3&imp_diff>=.15&imp_diff>=-.15~"More Perf",
      sat_diff>=.3&(imp_diff<=.15&imp_diff>=-.15)~"Catch Up",
      imp_diff>=.3&sat_diff<=-.15~"INVEST",
      imp_diff<=.3&sat_diff<=.3~"Satisfice",
      imp_diff<=-.15&sat_diff<=-.15~"Satisfice",
      TRUE ~ "BROKEN")) %>%
    # measure the difference between group AT SEGMENT LEVEL & suggest
    mutate(seg_slope_cat=case_when(
      ave_imp_slope>=.3&ave_sat_slope>=.3~"Catch Up",
      ave_imp_slope>=.3&(ave_sat_slope<=.15&ave_sat_slope>=-.15)~"More Performance",
      ave_sat_slope>=.3&(ave_imp_slope<=.15&ave_imp_slope>=-.15)~"Catch Up",
      ave_imp_slope>=.3&ave_sat_slope<=-.15~"Invest",
      ave_imp_slope<=.3&ave_sat_slope<=.3~"Satisfice",
      ave_imp_slope<=-.15&ave_sat_slope<=-.15~"Satisfice",
      TRUE ~ "BROKEN"))
    return(df_new)
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
    separate(objective,sep="([.])",into= c("job_step","objective"))
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name=V2) %>%
    pluck(.,1)

  importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
    mutate(imp_diff=.[[3]]-.[[4]],
           sat_diff=.[[5]]-.[[6]],
      group_difference=.[[7]]-.[[8]],
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
           pct_diff=scales::percent(pct_diff,accuracy = 2),
           sample_size_factor_a=sample_size_factor_a,
           sample_size_factor_b=sample_size_factor_b)  %>%
    # Cal the difference between each groups imp & satisfaction
    mutate(imp_diff=.[[3]]-.[[4]],
           sat_diff=.[[5]]-.[[6]],
           segment_a=factor_a,
           segment_b=factor_b) %>%
    mutate(ave_imp_slope=round(mean(imp_diff),2),
           ave_sat_slope=round(mean(sat_diff),2),
           total_imp_diff=sum(imp_diff),
           total_sat_diff=sum(sat_diff))# %>%
    # measure the difference between group AT SEGMENT LEVEL & suggest
    # mutate(indv_slope_cat=case_when(
    #   imp_diff>=.3&sat_diff>=.3~"Catch Up",
    #   imp_diff>=.3&(sat_diff<=.15&sat_diff>=-.15)~"More Performance",
    #   # sat_diff>=.3&imp_diff>=.15&imp_diff>=-.15~"More Perf",
    #   sat_diff>=.3&(imp_diff<=.15&imp_diff>=-.15)~"Catch Up",
    #   imp_diff>=.3&sat_diff<=-.15~"INVEST",
    #   imp_diff<=.3&sat_diff<=.3~"Satisfice",
    #   imp_diff<=-.15&sat_diff<=-.15~"Satisfice",
    #   TRUE ~ "BROKEN")) %>%
    # # measure the difference between group AT SEGMENT LEVEL & suggest
    # mutate(seg_slope_cat=case_when(
    #   ave_imp_slope>=.3&ave_sat_slope>=.3~"Catch Up",
    #   ave_imp_slope>=.3&(ave_sat_slope<=.15&ave_sat_slope>=-.15)~"More Performance",
    #   ave_sat_slope>=.3&(ave_imp_slope<=.15&ave_imp_slope>=-.15)~"Catch Up",
    #   ave_imp_slope>=.3&ave_sat_slope<=-.15~"Invest",
    #   ave_imp_slope<=.3&ave_sat_slope<=.3~"Satisfice",
    #   ave_imp_slope<=-.15&ave_sat_slope<=-.15~"Satisfice",
    #   TRUE ~ "BROKEN"))

  temp_title <- paste0(factor_a," vs ",factor_b)

  get_jtbd_segment.compare_2.table(importance_satisfaction_opportunity,temp_title)
  get_opportunity_score_graph.compare_2(importance_satisfaction_opportunity,factor_a,factor_b)


  # importance_satisfaction_opportunity %>%
  #   arrange(desc(max_opportunity))%>%
  #   select(-group_difference_score,-max_opportunity) %>%
  #   #filter(Opportunity_Group!="None") %>%
  #   select(-contains("rank"),-contains("group"),-contains("Group")) %>%
  #   print_data_table_compare_2(.,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b)

  # opportunity_graph_data_frame <- prep_data_frame_for_opportunity_graph(importance_satisfaction_opportunity)

  # plot <- get_opportunity_score_graph_individual(opportunity_graph_data_frame,factor_a,factor_b)
  # save_yo_file_png_take_file_name(plot,paste0(deparsed_column_name,"_plot__opportunity_score"))

  # opportunities_by_stage_group_1 <- get_opportunities_by_stage(opportunity_score_group_1)
  # print_data_table(opportunities_by_stage_group_1,paste0("opp_by_stage-",factor_a))
  #
  # opportunities_by_stage_group_2 <- get_opportunities_by_stage(opportunity_score_group_2)
  # print_data_table(opportunities_by_stage_group_2,paste0("opp_by_stage-",factor_b))

  return(importance_satisfaction_opportunity)
}


get_imp_sat_opp_scores_compare_2.ranked <- function(table_containing_ranking,your_data_frame){
  ranked_table <- your_data_frame %>%
    left_join(table_containing_ranking,by="objective") %>%
    mutate(
      # rank_vs_overall.factor_a=.[[9]]-rank_total_population,
      #      rank_vs_overall.factor_b=.[[10]]-rank_total_population,
           opp_vs_overall_opp.factor_a=.[[7]]-opportunity_score_total_population,
           opp_vs_overall_opp.factor_b=.[[8]]-opportunity_score_total_population,
           opp_vs_overall_pct_diff.factor_a=opp_vs_overall_opp.factor_a/opportunity_score_total_population,
           opp_vs_overall_pct_diff.factor_b=opp_vs_overall_opp.factor_b/opportunity_score_total_population,
           new_opp.factor_a=case_when(
             opportunity_score_total_population<9&your_data_frame[[7]]>=9~1,
             TRUE~0),
           new_opp.factor_b=case_when(
             opportunity_score_total_population<9&your_data_frame[[8]]>=9~1,
             TRUE~0)) %>%
    select(-c(opp_vs_overall_opp.factor_a,
              opp_vs_overall_opp.factor_b,
              opp_vs_overall_pct_diff.factor_a,
              opp_vs_overall_pct_diff.factor_b))

  segmentation_factor <- pluck(your_data_frame$segmentation_factor[[1]])

  graph_title <- paste0("segmentation factor: ",segmentation_factor)
  # print(graph_title)
  get_jtbd_segment.compare_2.table(ranked_table,graph_title)
  return(ranked_table)
}

# * Compare 2: Prep Data Frame for Graph ----------------------------------
prep_data_frame_for_opportunity_graph <- function(data_frame){
  imp_long <- data_frame %>%
    select(job_step,objective,starts_with("imp_"),starts_with("sat_")) %>%
    pivot_longer(cols=c(starts_with("imp_")),values_to = "rating",names_to = c("imp")) %>%
    select(job_step,objective,imp,rating) %>%
    separate(imp,into = c("imp","segment"),sep="\\_",extra = "merge")

  sat_long <- data_frame %>%
    select(job_step,objective,starts_with("imp_"),starts_with("sat_")) %>%
    pivot_longer(cols=c(starts_with("sat_")),values_to = "rating",names_to = c("sat")) %>%
    select(job_step,objective,sat,rating) %>%
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


# * Get Opportunities by Segment WIP --------------------------------------
get_opportunities_by_segment <- function(your_list_of_data_frames){
  for (data_frame in length(your_list_of_data_frames)){
    view(as.data.frame(your_list_of_data_frames[data_frame]))# %>%
    # separate(Opportunity_Group,sep="__",c("Opportunity_Group_A","Opportunity_Group_B",remove=FALSE)) %>%
    # pivot_longer(cols=starts_with("Opportunity_Group"),values_to = "Opportunity_Group", names_to = "Stuff") %>%
    # select(objective,Opportunity_Group) %>%
    # view()
    # print(a)
  }
}

# * Get Opportunity by Stage TBD ------------------------------------------
get_opportunities_by_stage <- function(your_data_frame){
  stage_opportunity_count <- your_data_frame %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective"))  %>%
    dplyr::mutate(job_step_count=if_else(.[[5]]>=10,1,0)) %>%
    dplyr::group_by(job_step) %>%
    dplyr::mutate(stage_opp_count=sum(job_step_count==1),
                  average_stage_opp=mean(.[[5]])) %>%
    ungroup() %>%
    distinct(job_step,stage_opp_count) %>%
    arrange(desc(stage_opp_count))
  # dplyr::group_by(job_step) %>%
  # mutate(max_stage_opp_score=max(.[[5]])) %>%
  # ungroup()
  # print(stage_opportunity_count)

  return(stage_opportunity_count)
}






# SEGMENTATION ------------------------------------------------------------

get_abs_difference <- function(your_data_frame){
  segment_name <- deparse(substitute(your_data_frame))
  new_data_frame <- your_data_frame %>%
    select(objective,group_difference) %>%
    mutate(group_difference=abs(group_difference))%>%
    rename(!!segment_name:=group_difference) %>%
    mutate(objective=as.character(objective))%>%
    arrange(objective)
  return(new_data_frame)
}
# * Segmentation - PCT ----------------------------------------------------
get_pct_difference <- function(your_data_frame){
  segment_name <- deparse(substitute(your_data_frame))
  new_data_frame <- your_data_frame %>%
    select(objective,pct_diff) %>%
    rename(!!segment_name:=pct_diff) %>%
    mutate(objective=as.character(objective))%>%
    arrange(objective)
  return(new_data_frame)
}

# Usefulness --------------------------------------------------------------
# * Find Usefulness Columns -----------------------------------------------
find_useful_columns <- function(your_data_frame) {
  useful_columns <- your_data_frame %>%
    select(starts_with("useful"))
  return(useful_columns)
}

# * Find Usefulness Scores ------------------------------------------------
individual_data <- NULL
all_data <- NULL
calculate_useful_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(solution=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(solution,user_rating,n)

    print(objective_score_tibble)

    individual_data <-  objective_score_tibble %>%
      summarize(solution=unique(solution),
                total_sum=sum(n),
                useful_sum=sum(n[user_rating==5|
                                   user_rating==4]))  %>%
      mutate(usefulness_score=((useful_sum/total_sum)*100))
    print(individual_data)
    all_data <- rbind(all_data,individual_data)

  }

  all_data <- all_data %>%
    separate(solution,sep="([.])",into= c("objective","solution")) %>%
    separate(objective,sep="(__)",into= c("theme","category")) %>%
    select(-theme,-total_sum,-useful_sum)%>%
    arrange(desc(usefulness_score)) %>%
    mutate(score_index=usefulness_score/median(usefulness_score)) %>%
    mutate_if(is.numeric,round,1)


  deparsed_column_name <- "total_usefulness"

  all_data %>%
    print_data_table(.,deparsed_column_name)

  return(all_data)
}
# * Usefulness - General --------------------------------------------------
get_usefulness_score <- function(your_data_frame){
  useful_columns <- find_useful_columns(your_data_frame)
  useful_score <- calculate_useful_score(useful_columns)
  return(useful_score)
}
# * Usefulness - Compare 2 ------------------------------------------------
get_usefulness_compare_2 <- function(your_data_frame,column_to_split_on,factor_a,factor_b){
  group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  usefulness_score_group_1 <- get_usefulness_score(group_1) %>%
    mutate(segment_name=factor_a,
           rank=rank(desc(usefulness_score)),
           index=usefulness_score_score/median(usefulness_score)) %>%
    arrange(desc(solution))
  print(usefulness_score_group_1)

  group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)
}

# * ATTRACTIVENESS ---------------------------------------------------------
# ** MASTER Attractiveness Calculation ------------------------------------
get_attractiveness <- function(your_data_frame){
  attractiveness_columns <- find_attractiveness_columns(your_data_frame)
  attractiveness_score <- calculate_attractiveness_score(attractiveness_columns)
  return(attractiveness_score)
}
# ** Find Attractiveness Columns ------------------------------------------
find_attractiveness_columns <- function(your_data_frame) {
  attractiveness_columns <- your_data_frame %>%
    select(starts_with("sol_att"))
  return(attractiveness_columns)
}

# ** Calculate Attractiveness ---------------------------------------------
individual_data <- NULL
all_data <- NULL
calculate_attractiveness_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(solution=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5)) %>%
      select(solution,user_rating,n)

    print(objective_score_tibble)

    individual_data <-  objective_score_tibble %>%
      summarize(solution=unique(solution),
                total_sum=sum(n),
                attractiveness_sum=sum(n[user_rating==5|
                                           user_rating==4]))  %>%
      mutate(attractiveness_score=((attractiveness_sum/total_sum)*10))
    # solution=str_remove(.,"sol"),
    # solution=str_replace(.,"att","attractiveness:_"))
    deparsed_column_name <- "solution_attractiveness"
    print(individual_data)
    all_data <- rbind(all_data,individual_data)
    all_data %>%
      print_data_table(.,deparsed_column_name)
  }
  return(all_data)
}

# ** Compare 2 Group Attractiveness ---------------------------------------
get_attractiveness_compare_2 <- function(your_data_frame,column_to_split_on,factor_a,factor_b){
  attractiveness_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  attractiveness_columns_group_1 <- find_attractiveness_columns(attractiveness_calc_group_1)

  attractiveness_score_group_1 <- calculate_attractiveness_score(attractiveness_columns_group_1) %>%
    mutate(segment_name=factor_a,
           rank=rank(desc(attractiveness_score)),
           index=attractiveness_score/median(attractiveness_score)) %>%
    arrange(desc(attractiveness_score))

  attractiveness_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)

  attractiveness_columns_group_2 <- find_attractiveness_columns(attractiveness_calc_group_2)

  attractiveness_score_group_2 <- calculate_attractiveness_score(attractiveness_columns_group_2) %>%
    mutate(segment_name=factor_b,
           rank=rank(desc(attractiveness_score)),
           index=attractiveness_score/median(attractiveness_score)) %>%
    arrange(desc(attractiveness_score))

  merged_attractivness_data_frame <- rbind(attractiveness_score_group_1,attractiveness_score_group_2)

  pivoted_attractiveness <- merged_attractivness_data_frame %>%
    pivot_wider(solution,
                names_from = c(segment_name),
                values_from = c(attractiveness_score,rank,index)) %>%
    mutate(objective=as_factor(solution)) %>%
    select(-objective)
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name=V2) %>%
    pluck(.,1)
  print_data_table(pivoted_attractiveness,deparsed_column_name)

  return(pivoted_attractiveness)
}


# * ATTRACTIVENESS RANKING ------------------------------------------------
# ** Find Attractiveness: Rank Columns ------------------------------------
find_attractiveness_rank_columns <- function(your_data_frame){
  attractivness_rank_columns <- your_data_frame %>%
    select(starts_with("sol_rank"))
  return(attractivness_rank_columns)
}

# ** Calculate Attractiveness: RANK Score ---------------------------------
individual_data <- NULL
all_data <- NULL
calculate_attractiveness_rank_score <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(solution=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5,6,7,8)) %>%
      select(solution,user_rating,n)

    individual_data <-  objective_score_tibble %>%
      summarize(solution=unique(solution),
                total_sum=sum(n),
                attractiveness_score_1=sum(n[user_rating==1])*3,
                attractiveness_score_2=sum(n[user_rating==2])*2,
                attractiveness_score_3=sum(n[user_rating==3])*1) %>%
      mutate(attractiveness_score=(attractiveness_score_1+attractiveness_score_2+attractiveness_score_3)) %>%
      select(solution,attractiveness_score)

    all_data <- rbind(all_data,individual_data)

  }
  return(all_data)
}

# * Calculate Attractiveness: RANK Score - FULL ----------------------------
individual_data <- NULL
all_data <- NULL
calculate_attractiveness_rank_score_full <- function(objectives){
  for (objective in seq_along(objectives)){
    namez <- names(objectives)[[objective]]

    objective_score <- fct_count(objectives[[objective]]) %>%
      mutate(solution=namez)

    objective_score_tibble <- objective_score %>%
      mutate(user_rating=f) %>%
      filter(user_rating %in% c(1,2,3,4,5,6,7,8)) %>%
      select(solution,user_rating,n)

    individual_data <-  objective_score_tibble %>%
      summarize(solution=unique(solution),
                total_sum=sum(n),
                attractiveness_score_1=sum(n[user_rating==1])*5,
                attractiveness_score_2=sum(n[user_rating==2])*4,
                attractiveness_score_3=sum(n[user_rating==3])*3,
                attractiveness_score_4=sum(n[user_rating==4])*2,
                attractiveness_score_5=sum(n[user_rating==5])*1) %>%
      mutate(attractiveness_score=(attractiveness_score_1+attractiveness_score_2+attractiveness_score_3+attractiveness_score_4+attractiveness_score_5)) %>%
      select(solution,attractiveness_score)

    all_data <- rbind(all_data,individual_data)
  }
  return(all_data)
}

# * Get Attractiveness: RANK Compared -------------------------------------
get_attractiveness_rank_compare_2 <- function(your_data_frame,column_to_split_on,factor_a,factor_b){
  attractiveness_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  attractiveness_columns_group_1 <- find_attractiveness_rank_columns(attractiveness_calc_group_1)

  attractiveness_score_group_1 <- calculate_attractiveness_rank_score(attractiveness_columns_group_1) %>%
    mutate(segment_name=factor_a,
           rank=rank(desc(attractiveness_score)),
           index=attractiveness_score/median(attractiveness_score)) %>%
    arrange(desc(solution))

  attractiveness_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)

  attractiveness_columns_group_2 <- find_attractiveness_rank_columns(attractiveness_calc_group_2)

  attractiveness_score_group_2 <- calculate_attractiveness_rank_score(attractiveness_columns_group_2) %>%
    mutate(segment_name=factor_b,
           rank=rank(desc(attractiveness_score)),
           index=attractiveness_score/median(attractiveness_score)) %>%
    arrange(desc(attractiveness_score))

  merged_attractivness_data_frame <- rbind(attractiveness_score_group_1,attractiveness_score_group_2)

  pivoted_attractiveness <- merged_attractivness_data_frame %>%
    pivot_wider(solution,
                names_from = c(segment_name),
                values_from = c(attractiveness_score,rank,index)) %>%
    mutate(solution=as_factor(solution))



  deparsed_column_name <- deparse(substitute(column_to_split_on))
  deparsed_column_name <- str_split_fixed(deparsed_column_name[1],"([$])",2) %>%
    as.data.frame(.) %>%
    select(deparsed_column_name=V2) %>%
    pluck(.,1)
  print_data_table(pivoted_attractiveness,deparsed_column_name)
  return(pivoted_attractiveness)
}

# * Get Attractiveness: RANK Compared - FULL ------------------------------
get_attractiveness_rank_compare_2_full <- function(your_data_frame,column_to_split_on,factor_a,factor_b){
  attractiveness_calc_group_1 <- your_data_frame %>%
    filter(column_to_split_on==factor_a)

  attractiveness_columns_group_1 <- find_attractiveness_rank_columns(attractiveness_calc_group_1)

  attractiveness_score_group_1 <- calculate_attractiveness_rank_score_full(attractiveness_columns_group_1) %>%
    mutate(segment_name=factor_a)

  attractiveness_calc_group_2 <- your_data_frame %>%
    filter(column_to_split_on==factor_b)

  attractiveness_columns_group_2 <- find_attractiveness_rank_columns(attractiveness_calc_group_2)

  attractiveness_score_group_2 <- calculate_attractiveness_rank_score_full(attractiveness_columns_group_2) %>%
    mutate(segment_name=factor_b)

  merged_attractivness_data_frame <- rbind(attractiveness_score_group_1,attractiveness_score_group_2)

  pivoted_attractiveness <- merged_attractivness_data_frame %>%
    pivot_wider(solution,
                names_from = c(segment_name),
                values_from = attractiveness_score) %>%
    mutate(objective=as_factor(solution)) %>%
    select(-objective)
  deparsed_column_name <- deparse(substitute(column_to_split_on))
  print_data_table(pivoted_attractiveness,deparsed_column_name)
  return(pivoted_attractiveness)
}

# Market Size -------------------------------------------------------------
# * Conditional Market Size -----------------------------------------------
get_market_size_by_segment <- function(your_data_frame,column_to_split_on){
  market_size <- your_data_frame %>%
    group_by(!!column_to_split_on)%>%
    mutate(factor_sourcing_spend_numeric=as.numeric(as.character(factor_sourcing_spend_numeric)),
           segment_sourcing_spend=sum(factor_sourcing_spend_numeric),
           segement_count=n(),
           sourcing_spend_per_user=segment_sourcing_spend/segement_count,
           segment_name=!!column_to_split_on) %>%
    ungroup() %>%
    distinct(segment_name,sourcing_spend_per_user)
  # select(segment_sourcing_spend,segement_count,sourcing_spend_per_user)
  return(market_size)
}

# SUMMARIZE ---------------------------------------------------------------

get_historgram <- function(your_data_frame,column_name,title_string){
  file_save_name <- paste0("plot ",title_string)
  file_save_name <-  str_replace_all(file_save_name,pattern = " ",replacement = "_")
  sample_size <- get_sample_size_general(your_data_frame,column_name)
  plot  <- your_data_frame %>%
    filter(!is.na(!!as.name(column_name))) %>%
    group_by(!!as.name(column_name))%>%
    count(column_name = factor(column_name)) %>%
    ggplot(aes(x=fct_rev(!!as.name(column_name)), y=n,fill = !!as.name(column_name))) +
    geom_col()+
    coord_flip()+
    labs(title=title_string,subtitle=paste0("Sample: ",sample_size),x="",y="Count",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
  save_yo_file_png_take_file_name(plot,file_save_name)
  return(plot)
}
# * Batch Summarization ---------------------------------------------------
get_batch_histograms <- function(your_data_frame,column_antecedent_string,title_string){
  file_save_name <- paste0("plot ",title_string)
  file_save_name <-  str_replace_all(file_save_name,pattern = " ",replacement = "_")
  sample_size <- get_sample_size_general(your_data_frame,column_antecedent_string)
  plot  <- your_data_frame %>%
    select(starts_with(column_antecedent_string)) %>%
    rename_all(.,~str_replace_all(.,column_antecedent_string,"")) %>%
    rename_all(.,~str_replace_all(.,"_"," ")) %>%
    rename_all(.,~str_to_title(.)) %>%
    gather() %>%
    filter(!is.na(value))%>%
    group_by(key) %>%
    count(value = factor(value)) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()%>%
    ggplot(aes(x = value, y = pct, fill = value, label = scales::percent(pct))) +
    geom_col(position = 'dodge') +
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 3) +
    scale_y_continuous(labels = scales::percent)+
    # coord_flip()+
    facet_wrap(~ key,ncol=2)+
    labs(title=title_string,subtitle=paste0("Sample: ",sample_size),x="",y="",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
  save_yo_file_png_take_file_name(plot,file_save_name)
  return(plot)
}


# * Batch Segmentation - Horizonal ----------------------------------------
get_batch_histograms_horizontal <- function(your_data_frame,column_antecedent_string,title_string){
  file_save_name <- paste0("plot ",title_string)
  file_save_name <-  str_replace_all(file_save_name,pattern = " ",replacement = "_")
  sample_size <- get_sample_size_general(your_data_frame,column_antecedent_string)
  plot  <- your_data_frame %>%
    select(starts_with(column_antecedent_string)) %>%
    rename_all(.,~str_replace_all(.,column_antecedent_string,"")) %>%
    rename_all(.,~str_replace_all(.,"_"," ")) %>%
    rename_all(.,~str_to_title(.)) %>%
    gather() %>%
    filter(!is.na(value))%>%
    group_by(key) %>%
    count(value = factor(value)) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()%>%
    ggplot(aes(x = value, y = pct, fill = value, label = scales::percent(pct))) +
    geom_col(position = 'dodge') +
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 3) +
    scale_y_continuous(labels = scales::percent)+
    coord_flip()+
    facet_wrap(~ key,ncol=2)+
    labs(title=title_string,subtitle=paste0("Sample: ",sample_size),x="",y="",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
  save_yo_file_png_take_file_name(plot,file_save_name)
  return(plot)
}

# * Cut Code --------------------------------------------------------------
# attractiveness_rank_company_size %>%
#   mutate(Midmarket_retailer_rank=rank(desc(Midmarket_retailers)),
#          Small_Retailers_rank=rank(desc(Small_Retailers)),
#          average_rank=(Midmarket_retailer_rank+Small_Retailers_rank)/2) %>%
#   select(solution,Midmarket_retailers,Midmarket_retailer_rank,Small_Retailers,Small_Retailers_rank,average_rank) %>%
#   View("Rank_by_Company_Size")
#
# get_ranking_table <- function(your_data_frame,factor_1,factor_2){
#   ranked_table <- your_data_frame %>%
#     select(names(factor_1))
#   print(ranked_table)
#   return(ranked_table)
# }
# Visulization ------------------------------------------------------------
# How to use quosures https://ggplot2.tidyverse.org/reference/aes.html




# Fresh
# *  Opportunity Score Graph: Individual ----------------------------------
get_opportunity_score_graph.compare_2 <- function(data_frame,segment_a,segment_b){
  sample_size.a <- pluck(data_frame$sample_size_factor_a)
  sample_size.b <- pluck(data_frame$sample_size_factor_b)
  imp_diff <- pluck(data_frame$ave_imp_slope)
  sat_diff <- pluck(data_frame$ave_sat_slope)

  data_frame <- prep_data_frame_for_opportunity_graph(data_frame) %>%
    filter(!segment=="diff")
  plot <-  data_frame %>%
    ggplot(aes(x=importance,y=satisfaction,color=opportunity,shape=segment,size=3))+geom_jitter(width = 0.025, height = 0.05) + geom_line(aes(group = objective),color="grey",size=1)+
    labs(title=paste0("Opportunities: ",segment_a," vs ",segment_b),subtitle=paste0("Sample: ",segment_a, ": n=", sample_size.a," | ",segment_b, ": n=", sample_size.b),x="Importance",y="Satisfaction",color="Opportunity\nScore",fill="", size="",shape="Segment")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    ggsave(paste0(project_name,"-","-",lubridate::today(),".png"),plot, width=15, height=10)

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


# *  Opportunity Score Graph: General -------------------------------------
get_opportunity_score_graph <- function(data,title_string,subtitle_string) {
  data %>%
    mutate(high_opportunity=if_else(opportunity_score>=10,"high-priority opportunity","other opportunity")) %>%
    ggplot(aes(x=imp,y=sat,color=high_opportunity,label=objective))+geom_point() +
    geom_text(aes(label=ifelse(opportunity_score>=10,as.character(objective),'')),hjust=0,vjust=0,angle = 45) +
    #coord_flip() +
    #+ scale_x_discrete(drop=FALSE)
    labs(title=title_string,subtitle=subtitle_string,x="Importance",y="Satisfaction",color="Opportunity\nValue",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0) +
    # scale_x_discrete(drop=FALSE) +
    #scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) #+
    coord_cartesian(xlim=c(1,10),ylim=c(1,10))
}

# *  Cleveland Graph ------------------------------------------------------
get_cleveland_graph <- function(data, objective, group_1,group_2,title_string,subtitle_string) {
  objective <- enquo(objective)
  group_1 <- enquo(group_1)
  group_2 <- enquo(group_2)
  # print(dplyr::quo_name(!!group_1))
  plot <- data %>%
    ggplot() + geom_segment(aes(x=!!objective, xend=!!objective, y=!!group_1, yend=!!group_2), color="grey") +
    geom_point(aes(x=!!objective, y=!!group_1), color="blue", size=3) +
    geom_point(aes(x=!!objective, y=!!group_2), color="red", size=3) +
    coord_flip()+
    labs(title=title_string,subtitle=subtitle_string,x="Objectives",y="Scores",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
          title = element_text (size = 15,margin = margin(b = 10)),
          legend.position="bottom") +
    expand_limits(x=0,y=0)
  save_yo_file_png_take_file_name(plot,paste0(title_string,"_plot_cleveland_graph"))
}



# * TODO * ----------------------------------------------------------------
# * Add table Stakes Flag to Opportunity Score Calculation ----------------






# SAVE --------------------------------------------------------------------

# Print Data Table --------------------------------------------------------
# * Print Data Table: OLD -------------------------------------------------
print_data_table_old <- function(your_data_frame,deparsed_column_name){
  png(paste0(project_name,"-",deparsed_column_name,"-",lubridate::today(),'.png'), height = 23*nrow(your_data_frame), width = 220*ncol(your_data_frame))
  grid.table(your_data_frame)
  dev.off()
  png(paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",deparsed_column_name,"-",lubridate::today(),'.png'), height = 23*nrow(your_data_frame), width = 220*ncol(your_data_frame))
  grid.table(your_data_frame)
  dev.off()
}


# Clean data format
# Fresh
make_opps_columns_legible <- function(your_data_frame){
  your_data_frame %>%
    rename(Opportunity=opportunity_score,
           "Job Step"=job_step,
           Objective = objective,
           Imp=imp,
           Sat=sat)
}

# Fresh
remove_weird_text_formatting <- function(your_data_frame){
  your_data_frame  %>%
    mutate_if(is.character,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.character,~str_to_sentence(.)) %>%
    mutate_if(is.factor,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.factor,~str_to_sentence(.)) %>%
    rename_all(.,~str_replace_all(.,"_"," ")) %>%
    rename_all(.,~str_to_title(.))
}

# Print Data Table: Neutral -----------------------------------------------
print_data_table <- function(your_data_frame,deparsed_column_name){
  modified_data_frame <- your_data_frame %>%
    mutate_if(is.character,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.character,~str_to_sentence(.)) %>%
    mutate_if(is.factor,~str_replace_all(.,"_"," ")) %>%
    mutate_if(is.factor,~str_to_sentence(.)) %>%
    rename_all(.,~str_replace_all(.,"_"," ")) %>%
    rename_all(.,~str_to_title(.)) %>%
    gt() %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short)),
      # subtitle = md("Data US Alibaba users find most helpful when selecting a supplier")
    ) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width= px(3),
      heading.align = "left"
    ) %>%
    data_color(
      columns = c(last_col(1):last_col()),
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    fmt_number(
      columns = c(last_col(1):last_col()),
      decimals = 2 )# decrease decimal places)
  #   ) %>%
  #   tab_style(
  #     style = cell_text(color = "black", weight = "bold"),
  #     locations = list(
  #       cells_row_groups(),
  #       cells_column_labels(everything())))
  # file_name <- paste0(project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  # file_name_backup <- paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  # gtsave(modified_data_frame,file_name)
  # gtsave(modified_data_frame,file_name_backup)
}

# * Print Data Table: General Population ----------------------------------
print_data_table_general_population <- function(your_data_frame,deparsed_column_name){
  modified_data_frame <- your_data_frame %>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           job_step=str_to_title(str_replace_all(job_step,"_"," "))) %>%
    rename(`Opportunity Score`=opportunity_score_total_population,
           `Importance`=imp_total_population,
           `Satisfaction`=sat_total_population,
           `Job Step`=job_step,
           `Objective`=objective) %>%
    select(-rank_total_population) %>%
    gt(groupname_col = "Sourcing Stage",
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
  gtsave(modified_data_frame,file_name)
}

# * Print Data Table: Compare 2 -------------------------------------------
print_data_table_compare_2 <- function(your_data_frame,deparsed_column_name,file_type,sample_size_factor_a,sample_size_factor_b){
  columns_of_interest <- c(1:9)
  row_count<- nrow(your_data_frame)
  segement_name <- str_to_title(str_replace_all(deparsed_column_name,"_"," "))

  modified_data_frame <- your_data_frame %>%
    select(all_of(columns_of_interest)) %>%
    rename(pctDiff=pct_diff)%>%
    mutate(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
           job_step=str_to_title(str_replace_all(job_step,"_"," ")),
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
    rename(`Job Step`=job_step,
           `Objective`=objective,
           !!factor_a_opp:=factor_a_opp,
           !!factor_b_opp:=factor_b_opp) %>%
    dplyr::relocate(pctDiff,.after = everything())  %>%
    gt(groupname_col = "Job Step",
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
  file_name_backup <- paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",deparsed_column_name,"-",file_type,"-",lubridate::today(),'.png')
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
}




# *Print: Rename: UTTER FAILURE -------------------------------------------
# I SPENT 3 HOURS TRYING TO RENAME COLUMNS SO ULTIMATELY THEY COULD HAVE THE SAME NAME>>>> WHICH ISN"T ALLOWED
print_data_table_compare_2_rename <- function(your_data_frame,deparsed_column_name){
  columns_of_interest <- c(1:8)
  segement_name <- str_to_title(str_replace_all(deparsed_column_name,"_"," "))


  modified_data_frame <- modified_data_frame <- your_data_frame %>%
    select(columns_of_interest) %>%
    transmute(objective=str_to_sentence(str_replace_all(objective,"_"," ")),
              job_step=str_to_title(str_replace_all(job_step,"_"," ")),
              # factor_a=.[[7]],
              # factor_b=.[[8]],
              factor_a_imp=.[[3]],
              factor_b_imp=.[[4]],
              factor_a_sat=.[[5]],
              factor_b_sat=.[[6]],
              factor_a_opp=.[[7]],
              factor_b_opp=.[[8]])

  factor_a_imp <- names(your_data_frame[3]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_a_imp)
  factor_b_imp <- names(your_data_frame[4]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_b_imp)
  factor_a_sat <- names(your_data_frame[5]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_a_sat)
  factor_b_sat <- names(your_data_frame[6]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_b_sat)
  factor_a_opp <- names(your_data_frame[7]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_a_opp)
  factor_b_opp <- names(your_data_frame[8]) %>%
    str_split_fixed(.,"_",n=2) %>%
    .[1,2] %>%
    make.names()
  print(factor_b_opp)
  modified_data_frame <- modified_data_frame %>%
    rename(`Job Step`=job_step,
           `Objective`=objective,
           !!factor_a_imp:=factor_a_imp,
           !!factor_b_imp:=factor_b_imp,
           !!factor_a_sat:=factor_a_sat,
           !!factor_b_sat:=factor_b_sat,
           !!factor_a_opp:=factor_a_opp,
           !!factor_b_opp:=factor_b_opp) %>%
    gt(groupname_col = "Job Step",
       rowname_col = "Objective") %>%
    tab_header(
      title = md(paste0('Top Opportunities: ',project_name_short, " - ",segement_name)),
      # subtitle = md("Data US Alibaba users find most helpful when selecting a supplier")
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
      columns = 7:8
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
          columns = 7,
          rows = factor_a >= 10
        ))) %>%
    tab_style(
      style = cell_text(color = "green",weight = "normal"),
      locations = list(
        cells_body(
          columns = 8,
          rows = factor_b >= 10
        ))) %>%
    tab_style(
      style = cell_text(color = "darkgreen",weight = "bolder"),
      locations = list(
        cells_body(
          columns = 7,
          rows = factor_a_opp >= 12
        ))) %>%
    tab_style(
      style = cell_text(color = "darkgreen",weight = "bolder"),
      locations = list(
        cells_body(
          columns = 8,
          rows = factor_b_opp >= 12
        ))) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = list(
        cells_row_groups(),
        cells_column_labels(everything()))) #%>%
  # cols_hide(
  #   columns = vars(factor_a,factor_b))
  file_name <- paste0(project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  file_name_backup <- paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",deparsed_column_name,"-table-",lubridate::today(),'.png')
  gtsave(modified_data_frame,file_name)
  gtsave(modified_data_frame,file_name_backup)
}

# * Save CSV --------------------------------------------------------------
save_yo_file_csv <- function(data_frame,project_name){
  file_name <- deparse(substitute(data_frame))
  write.csv(data_frame,paste0(project_name,"-",file_name,"-",lubridate::today(),".csv"))
  write.csv(data_frame,paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".csv"))
}


# * Save Current Survey Data for Historical Analysis ----------------------
save_yo_file_historical_csv <- function(data_frame,historical_data_frame){
  file_name <- "current_survey_data"
  write.csv(data_frame,paste0(project_name,"-",file_name,"-",lubridate::today(),".csv"))
  write.csv(data_frame,paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".csv"))
  write.csv(data_frame,paste0("/Users/charlesrogers/Documents/Work-Analysis/Seller Analysis/Seller Satisfaction Analysis/Previous_Survey_Findings/",project_name,"-",file_name,"-",lubridate::today(),".csv"))
  merged_data_frame <- rbind(data_frame,historical_data_frame)
  write.csv(merged_data_frame,paste0("/Users/charlesrogers/Documents/Work-Analysis/Seller Analysis/Seller Satisfaction Analysis/Previous_Survey_Findings/","previous_survey_data",".csv"))
  return(merged_data_frame)
}
# * Save - Plot -----------------------------------------------------------
save_yo_file_png <- function(your_data_frame){
  file_name <- deparse(substitute(your_data_frame))
  ggsave(paste0(project_name,"-",file_name,"-",lubridate::today(),".png"),your_data_frame, width=15, height=10)
  ggsave(paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".png"),your_data_frame, width=15, height=10)
}

# Save - Plot: Manual Name ------------------------------------------------
save_yo_file_png_manual_name <- function(data_frame,file_name){
  ggsave(paste0(project_name,"-",file_name,"-",lubridate::today(),".png"),data_frame, width=15, height=10)
  ggsave(paste0("/Users/charlesrogers/Documents/Work-Analysis_dep/",project_name,"-",file_name,"-",lubridate::today(),".png"),data_frame, width=15, height=10)
}

# Save - Plot: Internal ---------------------------------------------------
save_yo_file_png_take_file_name <- function(plot,file_name){
  ggsave(paste0(project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)
  # ggsave(paste0("/Users/charlesrogers/Documents/work/analysis/all_exports",project_name,"-",file_name,"-",lubridate::today(),".png"),plot, width=15, height=10)
}

# Sample Code -------------------------------------------------------------
# PCT by Categorical Factors ----------------------------------------------
# df_by_num_shipments <- df %>%
#   select(s_num_sample_orders,factor_end_use,factor_experience_level,factor_shipping_method,factor_shipping_orgin,factor_logistics_specialization) %>%
#   mutate(factor_n_orders_3=case_when(s_num_sample_orders %in% c("We are in the process of placing our first order",'1','2-5','6-10')~"sub_10",
#                                      s_num_sample_orders %in% c('11-25',"26-50")~"10_to_50",
#                                      s_num_sample_orders %in% c('51-100',">100")~"51+"))

# https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
# df_by_num_shipments %>%
#   pivot_longer(factor_end_use:factor_logistics_specialization,names_to = "stat",values_to = "value") %>%
#   na.omit() %>%
#   mutate(factor_n_orders_3=fct_relevel(factor_n_orders_3,"10_to_50","51+","sub_10")) %>%
#   ggplot(aes(x=value,group=factor_n_orders_3,fill=factor_n_orders_3)) +
#   geom_bar(aes(y = ..prop..), stat="count") +
#   facet_wrap(~stat,scales = "free_y",nrow=2) +
#   coord_flip() +
#   geom_text(aes( label = scales::percent(..prop..),
#                  y= ..prop..), stat= "count", position = position_stack(vjust = 0.5)) +
#   scale_y_continuous(labels = scales::percent) +
#   #+ scale_x_discrete(drop=FALSE)
#   labs(title="Categorical Factors X Number of Shipments",subtitle="",x="",y="",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
#   theme(text=element_text(family = "Roboto"),
#         panel.grid.major = element_line(color = "#DAE1E7"),
#         panel.background = element_blank(),axis.text = element_text(size = 12),
#         axis.text.x = element_text(margin = margin(t = 5)),#hjust = 1,angle=90
#         axis.text.y = element_text(margin = margin(r = 5)),
#         axis.title = element_text (size = 15),
#         axis.line = element_line(),
#         axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
#         axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
#         plot.caption = element_text(size = 8,
#                                     margin = margin(t = 10),
#                                     color = "#3D4852"),
#         title = element_text (size = 15,margin = margin(b = 10)),) +
#   expand_limits(x=0,y=0)

# Explanatory Analys ------------------------------------------------------




# Stylistic Changes -------------------------------------------------------

# * Title Case ------------------------------------------------------------
# transform_strings_to_title_case <- function(your_data_frame){
#   your_data_frame  <- your_data_frame %>%
#     rename_all(.,~str_replace_all(.,"_"," ")) %>%
#     rename_all(.,~str_to_title(.))
#   return(your_data_frame)
# }




## Save

get_imp_sat_opp_scores_total_population <- function(your_data_frame){

  opportunity_columns_group_1 <- find_imp_sat_columns(your_data_frame)
  opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
  opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
  opportunity_score_group_1 <- calculate_opportunity_score(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
    arrange(desc(opportunity_score)) %>%
    mutate(segment_name="total_population",
           rank=rank(desc(opportunity_score)))
  # print(opportunity_score_group_1)
  opportunity_score_group_1 <- opportunity_score_group_1 %>%
    mutate(opp_index = paste0(round(opportunity_score/median(opportunity_score),2)*100,"%"))

  importance_satisfaction_opportunity <- opportunity_score_group_1 %>%
    pivot_wider(objective,
                names_from = c(segment_name),
                values_from = c(imp,sat,opportunity_score,rank,opp_index)) %>%
    mutate(objective=as_factor(objective)) %>%
    separate(objective,sep="([.])",into= c("job_step","objective")) %>%
    # mutate(max_opportunity=rank()) %>%
    # arrange(max_opportunity) %>%
    mutate_if(is.numeric,round,1) %>%
    mutate(objective=factor(objective, levels=objective))
  deparsed_column_name <- "total_opportunity"

  # importance_satisfaction_opportunity %>%
  #   print_data_table_general_population(.,deparsed_column_name)

  return(importance_satisfaction_opportunity)
}

#
#
# # Recoding Columns --------------------------------------------------------
# recode_qualitative_to_quantitative <- function(your_data_frame){
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely likely"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely useful"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely useful"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely useful"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely useful"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very useful"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very useful"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very useful"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very useful"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately useful"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately useful"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="useful"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that useful"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that useful"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat useful"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat useful"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all useful"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all useful"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all useful"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all useful"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely likely"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely interested"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely interested"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely likely"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely important"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely important"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely satisfied"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Extremely satisfied"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely important"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely important"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely satisfied"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="extremely satisfied"] <- 5; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very likely"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very likely"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very important"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very important"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very important"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very important"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very interested"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very interested"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very satisfied"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Very satisfied"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very interested"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very interested"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very satisfied"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="very satisfied"] <- 4; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately important"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately important"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately satisfied"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately interested"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="moderately interested"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately interested"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="moderately interested"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately satisfied"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Likely"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately likely"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Moderately likely"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Satisfied"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="satisfied"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="important"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat satisfied"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat satisfied"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Slighlty important"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat important"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="somewhat important"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Slightly important"] <- 3; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that important"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that important"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that interested"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that interested"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that important"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that likely"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that likely"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that satisfied"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not that satisfied"] <- 2; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all Important"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all important"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all important"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all important"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all important"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all interested"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all interested"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all interested"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all satisfied"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all satisfied"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all satisfied"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all likely"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all likely"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="Not at all satisfied"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
#   your_data_frame <- apply(your_data_frame,2,function(x){x[x=="not at all satisfied"] <- 1; x})
#   your_data_frame <- as.data.frame(your_data_frame)
# }


# * Load & Clean Data - Master Function -----------------------------------

load_and_rename_data_survey_monkey <- function(your_data_path_as_string){
  raw_data_frame <- read_csv(your_data_path_as_string)
  col_names <- get_column_names_survey_monkey(raw_data_frame)
  list_of_row_2_names <- get_row_1_as_column_names_survey_monkey(raw_data_frame)
  list_of_index_numbers <- get_index_of_cols_to_rename(col_names)
  working_row_names <- list_of_row_2_names[list_of_index_numbers]
  your_data_frame <- clean_names_survey_monkey(raw_data_frame)
  your_data_frame <- rename_vague_columns(your_data_frame,working_row_names)
  return(your_data_frame)
}

# * Get Column Names for Manual Renaming ----------------------------------
get_column_names_for_manual_renaming <- function(your_data_path_as_string){
  raw_data_frame <- read_csv(your_data_path_as_string)
  column_names <- raw_data_frame %>%
    clean_names() %>%
    colnames()
  row_1_column_names <- raw_data_frame %>%
    row_to_names(.,1) %>%
    clean_names() %>%
    colnames()
  merged_column_names <- cbind(column_names,row_1_column_names)
  return(merged_column_names)
}
# * Get Index of Column Names that Need Renaming --------------------------
get_index_of_cols_to_rename <- function(data_frame_of_column_names){
  columns_to_rename_by_index <- data_frame_of_column_names %>%
    rownames_to_column() %>%
    filter(str_detect(value,"^x")) %>%
    transmute(viable_rownames=rowname) %>%
    pull() %>%
    as.numeric()
  return(columns_to_rename_by_index)
}


# * Rename Vauge, 'xX' Columns --------------------------------------------
rename_vague_columns<- function(your_data_frame,working_row_names){
  your_data_frame <- your_data_frame %>%
    rename_with(.,~ working_row_names,starts_with("x"))
  return(your_data_frame)
}


# * Clean Names of Loaded Data --------------------------------------------
clean_names_survey_monkey <- function(your_data_frame){
  your_data_frame <- your_data_frame %>%
    janitor::clean_names()
  your_data_frame <- your_data_frame[-c(1), ]
  return(your_data_frame)
}

# * Get Row Names for Recoding --------------------------------------------
get_row_1_as_column_names_survey_monkey <- function(raw_data_frame){
  your_data_frame <- raw_data_frame %>%
    row_to_names(.,1) %>%
    clean_names(.) %>%
    colnames(.) %>%
    as.tibble(.) %>%
    pull(.)
  return(your_data_frame)
}

# * Get Column Names from Survey Monkey -----------------------------------
get_column_names_survey_monkey <- function(raw_data_frame){
  your_data_frame <- raw_data_frame %>%
    clean_names(.) %>%
    colnames(.) %>%
    as.tibble(.)
  return(your_data_frame)
}


# Data Prep ---------------------------------------------------------------

# * Load Packages ---------------------------------------------------------
library(tidyverse)
library(magrittr)
library(janitor)
library(lubridate)
# library(proportion)


# Load Data ---------------------------------------------------------------

# * Load Previous Survey Data ---------------------------------------------
load_previous_survey_data <- function(){
  previous_survey_data <- read_csv('/Users/crogers/Work-Analysis/Seller Analysis/Seller Satisfaction Analysis/Previous_Survey_Findings/previous_survey_data.csv') %>%
    select(-X1)
  return(previous_survey_data)
}

# * Create Current Survey Data Frame --------------------------------------
add_to_current_survey_data <- function(your_output_column,column_name){
  current_survey_data$column_name <- your_output_column
  return(current_survey_data)
}



# * Recoding Data ---------------------------------------------------------
## Create names: split every name section with with two underscores: "__"... "inventory_receiving__mt_to_select_supplier"
# PROBABILITY -------------------------------------------------------------
# * Likelihood to: --------------------------------------------------------
# ** Likelihood to Renew --------------------------------------------------
get_likelihood_to_renew <- function(your_data_frame){
  probability_of_renewal_df <- your_data_frame %>%
    filter(!is.na(likelihood_flag)) %>%
    mutate(likelihood_flag=as.numeric(likelihood_flag)) %>%
    dplyr::summarise(ltr_pct=mean(likelihood_flag),n=n())
  print(probability_of_renewal_df)
  probability_of_renewal_df <- probability_of_renewal_df %>%
    mutate(ltc_pct=1-ltr_pct,
           ltr_count=round(ltr_pct*n),
           ltc_count=n-ltr_count)
  print(probability_of_renewal_df)
  ltr_score<- round((probability_of_renewal_df$ltr_pct)*100,0)
  ltr_sample_size <- probability_of_renewal_df$n

  plot_ltr(your_data_frame,ltr_score,ltr_sample_size)
  return(ltr_score)
}

# ** Likelihood to Renew - BY FACTOR --------------------------------------
get_likelihood_to_renew_by_factor <- function(your_data_frame,segmentation_column){
  df_likelihood_to_renew <- your_data_frame %>%
    filter(!is.na(likelihood_flag)) %>%
    filter(!is.na(!!as.name(segmentation_column))) %>%
    group_by(!!as.name(segmentation_column)) %>%
    mutate(likelihood_flag=as.numeric(likelihood_flag)) %>%
    filter(!reason_for_churn%in%c('Haven\'t been on the platform long enough to feel comfortable saying "very" or "extremely likely to renew"')) %>%
    dplyr::summarise(ltr_by_bt_pct=mean(likelihood_flag),n=n()) %>%
    mutate(ltc_by_bt_pct=1-ltr_by_bt_pct,
           ltr_by_bt_ct=round(ltr_by_bt_pct*n),
           ltc_by_bt_ct=n-ltr_by_bt_ct)
  return(df_likelihood_to_renew)
}
# get_likelihood_to_renew <- function(your_data_frame){
#   probability_of_renewal_df <- your_data_frame %>%
#     filter(!is.na(likelihood_flag)) %>%
#     mutate(likelihood_flag=as.numeric(likelihood_flag)) %>%
#     dplyr::summarise(ltr_pct=mean(likelihood_flag),n=n())
#   print(probability_of_renewal_df)
#   probability_of_renewal_df <- probability_of_renewal_df %>%
#     mutate(ltc_pct=1-ltr_pct,
#            ltr_count=round(ltr_pct*n),
#            ltc_count=n-ltr_count)
#   print(probability_of_renewal_df)
#   ltr_score<- round((probability_of_renewal_df$ltr_pct)*100,0)
#   ltr_sample_size <- probability_of_renewal_df$n
#
#   plot_ltr(your_data_frame,ltr_score,ltr_sample_size)
#   return(ltr_score)
# }
# ** Likelihood to Renew: By Group ----------------------------------------
# get_likelihood_to_renew_by_group <- function(your_data_frame,binary_factor_column,grouping_column){
#   probability_of_renewal <- your_data_frame %>%
#     filter(!is.na(likelihood_flag)) %>%
#     group_by(grouping_column) %>%
#     mutate(binary_factor_column=as.numeric(binary_factor_column)) %>%
#     dplyr::summarise(ltr_pct=mean(binary_factor_column),n=n()) %>%
#     mutate(ltc_pct=1-ltr_pct,
#            ltr_count=round(ltr_pct*n),
#            ltc_count=n-ltr_count)
#   return(probability_of_renewal)
# }
# ** Likelihood to Adopt --------------------------------------------------


# # ** COMPARE 3 IMP SAT - WIP ----------------------------------------------
# get_imp_sat_opp_scores_compare_3 <- function(your_data_frame,column_to_split_on,factor_a,factor_b,factor_c) {
#   opportunity_calc_group_1 <- your_data_frame %>%
#     filter(column_to_split_on==factor_a)
#
#   opportunity_columns_group_1 <- find_imp_sat_columns(opportunity_calc_group_1)
#
#   opportunity_score_group_1 <- calculate_pop_pct_score(opportunity_columns_group_1)
#
#   opportunity_score_group_1 <- split_imp_sat_columns(opportunity_score_group_1)
#
#   opportunity_score_group_1<- calculate_opportunity_score(opportunity_score_group_1) %>%
#     # mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
#     # arrange(desc(opportunity_score)) %>%
#     mutate(segment_name=factor_a)
#
#   opportunity_calc_group_2 <- your_data_frame %>%
#     filter(column_to_split_on==factor_b)
#
#   opportunity_columns_group_2 <- find_imp_sat_columns(opportunity_calc_group_2)
#
#   opportunity_score_group_2 <- calculate_pop_pct_score(opportunity_columns_group_2)
#
#   opportunity_score_group_2 <- split_imp_sat_columns(opportunity_score_group_2)
#
#   opportunity_score_group_2<- calculate_opportunity_score(opportunity_score_group_2) %>%
#     # mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
#     # arrange(desc(opportunity_score)) %>%
#     mutate(segment_name=factor_b)
#
#   opportunity_calc_group_3 <- your_data_frame %>%
#     filter(column_to_split_on==factor_c)
#
#   opportunity_columns_group_3 <- find_imp_sat_columns(opportunity_calc_group_3)
#
#   opportunity_score_group_3 <- calculate_pop_pct_score(opportunity_columns_group_3)
#
#   opportunity_score_group_3 <- split_imp_sat_columns(opportunity_score_group_3)
#
#   opportunity_score_group_3<- calculate_opportunity_score(opportunity_score_group_3) %>%
#     mutate(opportunity_score=if_else(imp<sat,imp,imp+imp-sat)) %>%
#     arrange(desc(opportunity_score)) %>%
#     mutate(segment_name=factor_c)
#
#   merged_opportunity_data_frame <- rbind(opportunity_score_group_1,opportunity_score_group_2,opportunity_score_group_3)
#
#   importance_satisfaction_opportunity <- merged_opportunity_data_frame %>%
#     pivot_wider(objective,
#                 names_from = c(segment_name),
#                 values_from = c(imp,sat,opportunity_score)) %>%
#     mutate(objective=as_factor(objective))
#
#   importance_satisfaction_opportunity <- importance_satisfaction_opportunity %>%
#     mutate(#group_ave=mean(.[[8]],.[[9]],.[[10]]),
#       Opportunity_Group=case_when(.[[8]]>=10&.[[9]]>=10&.[[10]]>=10~paste0(factor_a,"__",factor_b,"__",factor_c),
#                                   .[[8]]>=10&.[[9]]<10&.[[10]]<10~factor_a,
#                                   .[[9]]>=10&.[[8]]<10&.[[10]]<10~factor_b,
#                                   .[[10]]>=10&.[[9]]<10&.[[8]]<10~factor_c,
#                                   .[[8]]>=10&.[[9]]>=10&.[[10]]<10~paste0(factor_a,"__",factor_b),
#                                   .[[9]]>=10&.[[10]]>=10&.[[8]]<10~paste0(factor_b,"__",factor_c),
#                                   .[[8]]>=10&.[[10]]>=10&.[[9]]<10~paste0(factor_a,"__",factor_c),
#                                   TRUE~"None")) %>%
#     separate(objective,sep="([.])",into= c("job_step","objective"))
#   return(importance_satisfaction_opportunity)
# }

# * Distribution Graph ----------------------------------------------------
get_distribution_graph <- function(data,relevant_column,title_string,subtitle_string) {
  data %>%
    mutate(relevant_column = fct_rev(fct_reorder(relevant_column, n))) %>%
    filter(!is.na(relevant_column)) %>%
    ggplot(aes(relevant_column,fill=relevant_column))+geom_bar() +
    coord_flip() +
    labs(title=title_string,subtitle=subtitle_string,x="",y="Count",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
}

get_distribution_graph_with_fill <- function(data,relevant_column,fill_column,title_string,subtitle_string) {
  data %>%
    ggplot(aes(relevant_column,fill=fill_column))+geom_bar() +
    coord_flip() +
    labs(title=title_string,subtitle=subtitle_string,x="Factor",y="Count",color="",fill="", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
}


# * Get NPS Score Graph ---------------------------------------------------
plot_nps_score <- function(your_data_frame,nps_score,nps_sample_size){
  plot <- your_data_frame %>%
    filter(!is.na(net_promoter_score)) %>%
    ggplot(aes(net_promoter_score,fill=nps_groups)) + geom_bar() +
    scale_fill_manual(values=c("red", "yellow","green")) +
    labs(title=paste0("US GGS Net Promoter Score = ",nps_score),subtitle=paste0("Sample size= ",nps_sample_size),x="Net Promoter Score",y="Number of Sellers",color="",fill="NPS Group", size="")+#,caption=paste0("Data as of ",today()) ,caption="NYC Research Team" ,caption=paste0("Created ",today())
    theme(text=element_text(family = "Roboto"),
          panel.grid.major = element_line(color = "#DAE1E7"),
          panel.background = element_blank(),axis.text = element_text(size = 12),
          axis.text.x = element_text(margin = margin(t = 5),hjust = 1),#angle=90
          axis.text.y = element_text(margin = margin(r = 5)),
          axis.title = element_text (size = 15),
          axis.line = element_line(),
          axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
          plot.caption = element_text(size = 8,
                                      margin = margin(t = 10),
                                      color = "#3D4852"),
          title = element_text (size = 15,margin = margin(b = 10)),) +
    guides(color=FALSE) +
    expand_limits(x=0,y=0)
  save_yo_file_png_take_file_name(plot,"US_GGS_NPS_Score")
}

# * Likelihood to Renew Graph ---------------------------------------------
plot_ltr <- function(your_data_frame,ltr_score,ltr_sample_size){
  plot_ltr <-  your_data_frame %>%
    filter(!is.na(likelihood_to_renew)) %>%
    ggplot(aes(likelihood_to_renew,fill=likelihood_to_renew))+geom_bar() +
    labs(title=paste0("Seller Likelihood to Renew: ",ltr_score,"%"),subtitle=paste0("Sample Size = ",ltr_sample_size),x="Likelihood to Renew",y="Number of Sellers",color="",fill="", size="",caption=paste0("Data as of ",today()))+
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
    guides(fill=FALSE) +
    expand_limits(x=0,y=0)
  #
  save_yo_file_png(plot_ltr)#,"US_GGS_Likelihood_to_Renew")
}
