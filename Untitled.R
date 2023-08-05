source('analyze.R')
library(plotly)
library(patchwork)
library(reactable)

# List of clients ----
clients <- list.files('data/clients')
client_name <- clients[1]
fileindex <- 1
raw_results <- get_raw_data(client_name, fileindex)
#Get the list of years when the survey was done
get_years_list <- function(client_name) {
  years_list <- c()
  files_list <-
    list.files(paste0('data/clients/', client_name, '/years/'))
  for (x in 1:length(files_list)) {
    years_list <- c(gsub('.xlsx', '', files_list[x]), years_list)
  }
  years_list <- sort(years_list)
  return(years_list)
}


# Categories ----
categories_df <- get_categories_df(client_name, fileindex)
categories_desc <- categories_df[, 2]
categories <- unique(categories_df[, 1])

# Profiles ----
profile_dataset <- get_profiles(client_name, 1, categories_desc)
participants_list <- profile_dataset[, 'Participants']
colnames(raw_results) <-
  c('Question_number',
    'Question_category',
    'Question_desc',
    participants_list)
participants_sorted <- sort(participants_list, decreasing = FALSE)

# Survey Questions ----
questions_table <- raw_results[, 1:3] %>%
  mutate(question = paste0('Question ', Question_number))
#mutate(question = paste0('Question_',Question_number))
questions_list <- c('All Questions', questions_table$Question_desc)
questions_category <-
  c('All Questions', unique(questions_table$Question_category))
get_questions_category <- function() {
  questions_category
}
profile_revised <-
  modify_profile(profile_dataset, categories, participants_list)

# Departments ----
department_list <-
  na.omit(dplyr::distinct(data.frame(profile_revised), Department))[, 'Department']
formatted_results <-
  generate_formatted_results(raw_results, participants_list)

# Color palettes  ----
get_color_palette <- function(type = 'main') {
  cp <- c('#2B2E4A', '#e84545', '#903749', '#709fb0')
  if (type == 'blue') {
    cp <- c('#413c69', '#4a47a3', '#709fb0', '#a7c5eb')
  }
  if (type == 'second') {
    cp <- c('#e84545', '#E9EF29', '#032761')
  }
  return(cp)
}
color_palette <- get_color_palette()
color_palette2 <- get_color_palette(type = 'blue')

#Summary

get_summary_objects <- function(client_name, report_year) {
  results_scored_by_questions <-
    generate_scores_per_question(formatted_results)
  scored_results <- generate_scores_per_participant(formatted_results)
  scores <- summarize_score(formatted_results, questions_table)
  highest_scores <- scores[[2]]
  lowest_scores <- scores[[1]]
  summarize_favorability_plot <-
    generate_favorability_doughnut_plot(formatted_results, color_palette2)
  summarize_participation_plot <-
    generate_participation_pie_chart(formatted_results)
  objects_list <- list(
    lowest_scores = lowest_scores,
    highest_scores = highest_scores,
    summarize_favorability_plot = summarize_favorability_plot,
    summarize_participation_plot = summarize_participation_plot
  )
  return(objects_list)
}




# Demographic visualization ----

## demographic - tables
demographics_table <-
  generate_demographics_ds(profile_dataset, formatted_results)
summary_demographics_table <-
  generate_summary_demographics_ds(demographics_table)

## demographic - functions
get_number_of_cat_elements <- function(attribute, categories_df) {
  get_column_name <- c()
  categories_df2 <-
    cbind(categories_df, names(profile_dataset)[2:length(profile_dataset)])
  names(categories_df2) <-
    c('category', 'category_desc', 'category_desc2')
  for (x in 1:nrow(categories_df2)) {
    if (attribute == categories_df2[x, 'category']) {
      get_column_name <-
        c(get_column_name, categories_df2[x, 'category_desc2'])
    }
  }
  number_of_elements <- length(get_column_name)
  return(number_of_elements)
}
demographics_participant_stats_plot <-
  function(profile_revised, attribute) {
    number_of_elements <- ncol(profile_revised)
    subset_data <- data.frame(profile_revised[, attribute])
    names(subset_data) <- 'attribute'
    summary_subset <- subset_data %>%
      dplyr::group_by(attribute) %>%
      summarise(count_attribute = n())
    fig <- plot_ly(
      summary_subset,
      labels = ~ attribute,
      values = ~ count_attribute,
      type = 'pie',
      name = attribute,
      #domain=list(row=0, column=i),
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~ paste(count_attribute, ' participants'),
      marker = list(
        colors = color_palette,
        line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = FALSE
    )
    fig <- fig %>%  layout(
      # title='Demographic Breakdown of Survey',
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
    )
    fig
    #fig<-patchwork::wrap_plots(plots_list,ncol=4)
    return(fig)
  }
analyze_demographics <-
  function(demographics_table,
           attribute,
           query_type,
           profile_dataset) {
    headers <- colnames(demographics_table)
    df1 <- round_number(demographics_table, 1)
    get_column_name <- c()
    temp_df <- matrix(0, nrow = nrow(df1), ncol = 1)
    rownames(temp_df) <- rownames(df1)
    categories_df2 <-
      cbind(categories_df, names(profile_dataset)[2:length(profile_dataset)])
    names(categories_df2) <-
      c('category', 'category_desc', 'category_desc2')
    for (x in 1:nrow(categories_df2)) {
      if (attribute == categories_df[x, 'category']) {
        get_column_name <-
          c(get_column_name, categories_df2[x, 'category_desc2'])
      }
    }
    for (y in 1:length(get_column_name)) {
      temp_df <- cbind(temp_df, df1[, get_column_name[y]])
    }
    if (ncol(temp_df) > 1) {
      temp_df <- temp_df[, 2:ncol(temp_df)]
    }
    colnames(temp_df) <- get_column_name
    temp_df <- data.frame(temp_df)
    temp_df <- tibble::rownames_to_column(temp_df)
    plot_list <- list()
    for (n in 2:ncol(temp_df)) {
      #df<-data.frame(temp_df[,n])
      df <- cbind(temp_df[, 1],
                  temp_df[, n])
      parameter <- colnames(temp_df)[n]
      colnames(df)[2] <- 'parameter'
      df2 <- data.frame()
      # library(tibble)
      if (query_type == 'question') {
        df2 <- cbind(questions_table$merged_question, df)
        colnames(df2)[1] <- 'question'
      }
      if (query_type == 'category') {
        df2 <- df
        colnames(df2)[1] <- 'question'
      }
      df3 <- data.frame(df2)
      df3$parameter <- as.numeric(df3$parameter)
      df4 <- df3[with(df3, order(parameter, decreasing = TRUE)), ]
      df4$question <- factor(df4$question, levels = df4$question)
      g <- ggplot(df4, aes(x = question, y = parameter, label = parameter)) +
        geom_segment(aes(yend = 0, xend = question),
                     linewidth = 2,
                     color = color_palette[4]) +
        geom_point(color = color_palette[4],
                   #fill=color_palette[4],
                   size = 11) +
        coord_flip() +
        scale_x_discrete()  +
        #ylim(c(0,75))+
        geom_text(color = 'white', fontface = 'bold') +
        labs(title = paste(parameter),
             y = 'Average Score',
             caption = attribute) +
        theme(
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = color_palette[5]),
          plot.title = element_text(
            size = 18,
            face = "bold",
            family = "Arial",
            color = color_palette[1],
            hjust = 0.5,
            lineheight = 1.2
          ),
          # title
          axis.title.y = element_blank(),
          # X axis title
          axis.title.x = element_text(
            size = 12,
            family = "Arial",
            color = color_palette[1]
          ),
          # Y axis title
          axis.text.x = element_text(
            size = 10,
            family = "Arial",
            color = color_palette[3],
            vjust = .5
          ),
          # X axis text
          axis.text.y = element_text(
            size = 15,
            family = "Arial",
            lineheight = 40,
            hjust = 1,
            color = color_palette[1]
          ),
          axis.ticks.y = element_blank()
        )
      
      
      g
      plot_list[[n - 1]] <- g
    }
    plot_length <- length(plot_list)
    multi_plot <-
      patchwork::wrap_plots(plot_list, nrow = plot_length, ncol = 1)
    #multi_plot
    return(multi_plot)
  }
get_demographics_objects <-
  function(client_name,
           report_year,
           demographics_attribute) {
    num_cat_elements <-
      get_number_of_cat_elements(demographics_attribute, categories_df)
    
    demographic_breakdown_pie <-
      demographics_participant_stats_plot(profile_revised,
                                          demographics_attribute)
    
    # demographics_attribute_plot<- analyze_demographics(demographics_table,
    #                                                    demographics_attribute,
    #                                                    'question',
    #                                                    profile_dataset)
    #summary_demographics_table_scored<-generate_summary_demographics_scored(summary_demographics_table)
    attribute_category_plot <-
      analyze_demographics(summary_demographics_table,
                           demographics_attribute,
                           'category',
                           profile_dataset)
    objects_list <- list(
      'pie chart' = demographic_breakdown_pie,
      # demographics_attribute_plot,
      'attribute plot' = attribute_category_plot,
      'cat elements' = num_cat_elements
    )
    return(objects_list)
  }

# Downloads ----
get_downloadable_objects <- function(client_name, report_year) {
  download_demographics_table <-
    generate_demographics_ds_scored(demographics_table)
  summary_demographics_table_scored <-
    generate_summary_demographics_scored(summary_demographics_table)
  objects_list <- list(download_demographics_table,
                       summary_demographics_table_scored)
  return(objects_list)
}

#Detailed Analysis ----
get_score_distribution_plot <- function(df) {
  df2 <- cbind(df[, 1], df[, 3:6])
  df3 <- data.frame(
    Favorable = mean(df$Favorable),
    Neutral = mean(df$Neutral),
    'Not Favorable' = mean(df$Not_Favorable),
    "Don't Know" = mean(df$DNK)
  )
  df4 <- rownames_to_column(data.frame(t(df3)), var = "Distribution")
  names(df4)[2] <- 'score'
  df4 <- df4[order(df4[, 'score'], decreasing = T), ]
  # df4[,1]<-c('Favorable','Neutral', 'Not_Favorable','DNK')
  g <- ggplot(df4, aes(x = reorder(Distribution, -score), y = score)) +
    geom_bar(stat = 'identity',
             width = 0.5,
             fill = color_palette) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(vjust = 0.5),
      plot.title = element_text(hjust = 0.5, color = color_palette[2]),
      panel.grid = element_blank(),
      panel.background = element_blank()
      # panel.border = element_rect(color = color_palette[4])
    ) +
    labs(title = 'OVERALL SCORE DISTRIBUTION',
         y = 'Percentage')
  # g
  output_list <- list(g)
  names(df2)[1] <- 'Category'
  df5 <-
    pivot_longer(df2, !Category, names_to = 'Distribution', values_to = 'Score')
  df6 <- dplyr::arrange(df5, Distribution, desc(Score))
  # df6$Distribution<-factor(df6$Distribution,levels = c('Favorable','Neutral',
  #                                                      'Not_Favorable','DNK'))
  
  # df6$Distribution<-fct_relevel(df6$Distribution,levels = c('Favorable','Neutral',
  #                                                      'Not_Favorable','DNK'))
  df6_cumsum <-
    plyr::ddply(df6, 'Category', transform, label_ypos = cumsum(Score))
  df6$Distribution <-
    factor(df6$Distribution,
           levels = c('DNK', 'Not_Favorable', 'Neutral', 'Favorable'))
  df6_cumsum$Distribution <-
    factor(df6$Distribution,
           levels = c('DNK', 'Not_Favorable', 'Neutral', 'Favorable'))
  g <- ggplot(df6, aes(x = Category, y = Score,
                       fill = Distribution)) +
    #guides(fill=guide_legend('Distribution'))+
    geom_bar(stat = 'identity', width = 0.30) +
    #geom_text(aes(y=label_ypos,label=Score))+
    coord_flip() +
    theme(
      legend.position = 'bottom',
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()
    )
  g
  output_list[[2]] <- g
  return(output_list)
}
generate_Question_category_plots <- function(df, question_type) {
  list_of_graphs <- list()
  fav_labels <-
    c('Favourable', 'Neutral', 'Not Favourable', 'Does Not Know')
  data <- data.frame(t(df))
  #data<-data.frame(t(convert_scores(df)))
  data <- cbind(questions_table$Question_category, data)
  names(data)[1] <- 'Question_category'
  #names(data)[2]<-'question'
  data_filtered <-
    data %>% filter(Question_category == question_type) %>%
    select(-Question_category)
  
  for (x in 1:nrow(data_filtered)) {
    individual_matrix <- data_filtered[x, ]
    individual_matrix <- data.frame(t(individual_matrix))
    question_number <- names(individual_matrix)[1]
    question_selected <-
      filter(questions_table, question == question_number)
    merged_question <-
      paste(question_selected$question,
            question_selected$question_desc,
            sep = ' | ')
    names(individual_matrix)[1] <- 'question'
    labels <- unique(individual_matrix$question)
    data <- individual_matrix %>%
      dplyr::group_by(question) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count))
    g <- ggplot(data, aes(
      x = factor(question),
      y = percentage * 100,
      fill = factor(question)
    )) + geom_bar(width = 0.5, stat = 'identity') +
      theme(
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = color_palette[5]),
        plot.title = element_text(
          size = 16,
          face = "bold",
          family = "Arial",
          color = color_palette[1],
          hjust = 0.5,
          lineheight = 1.2
        ),
        # title
        axis.title.x = element_text(
          size = 12,
          family = "Arial",
          color = color_palette[1]
        ),
        # X axis title
        axis.title.y = element_text(
          size = 12,
          family = "Arial",
          color = color_palette[1]
        ),
        # Y axis title
        axis.text.x = element_text(
          size = 10,
          family = "Arial",
          color = color_palette[3],
          vjust = .5
        ),
        # X axis text
        axis.text.y = element_text(
          size = 10,
          family = "Arial",
          color = color_palette[1]
        )
      ) +
      coord_flip() +
      scale_fill_manual(values = color_palette,
                        aesthetics = c("colour", "fill")) +
      labs(
        title = strwrap("Score Distribution"),
        x = "Response",
        y = "Percentage",
        subtitle = question_number,
        caption = wrapper(merged_question)
      )
    g
    list_of_graphs[[x]] <- g
  }
  # return(patchwork::wrap_plots(list_of_graphs,ncol=3))
  return(list_of_graphs)
}
generate_Question_category_plots2 <- function(df, question_type) {
  #df<-formatted_results
  data <- data.frame(t(df))
  data <- cbind(questions_table$Question_category, data)
  names(data)[1] <- 'Question_category'
  data_filtered <-
    data %>% filter(Question_category == question_type) %>%
    select(-Question_category)
  data_converted <- convert_scores(data_filtered)
  questions_table2 <- questions_table %>%
    mutate(
      Question_number = as.character(Question_number),
      Question_merged = paste(Question_desc, Question_number, sep = ' | ')
    )
  new_df <- data.frame()
  for (x in 1:nrow(data_filtered)) {
    data_calculated <- calculate_favorability2(data_converted[x, ])
    data_calculated$question <- rownames(data_filtered)[x]
    data_calculated$percent_score <- data_calculated$percent_score * 100
    merged <- inner_join(data_calculated,
                         questions_table2,
                         by = c('question' = 'Question_number'))
    #data_calculated<- data_calculated %>%
    # mutate(lab_ypos = cumsum(percent_score) + 0.5 * percent_score)
    #mutate(cumsum = cumsum(percent_score),
    #   lab_ypos = cumsum(percent_score) + 0.5 * percent_score)
    new_df <- rbind(new_df, merged)
    
  }
  # View(new_df)
  axis_questions <-
    questions_table2 %>% filter(Question_category == question_type)
  #new_df=new_df %>% arrange(desc(question))
  new_df = new_df %>% arrange(question)
  # questions<-unique(new_df$question)
  #new_df$question<-factor(new_df$question, levels=sort(new_df$question, decreasing = TRUE))
  g <-
    ggplot(new_df,
           aes(
             x = question,
             y = percent_score,
             label = percent_score,
             # fill=factor(description,levels=c('DNK','Unfavorable','Neutral','Favorable'))
             fill = description
           )) +
    geom_bar(width = 0.8, stat = 'identity') +
    geom_text(
      size = 4,
      position = position_stack(vjust = 0.5),
      color = 'white',
      fontface = 'bold'
    ) +
    theme_void() +
    #scale_x_discrete(labels = sort(stringr::str_wrap(new_df$Question_merged )))+
    #scale_x_discrete(labels = stringr::str_wrap(sort(axis_questions$Question_merged,decreasing = T)))+
    scale_x_discrete(labels = sort(as.integer(axis_questions$Question_number,decreasing = T)))+
    scale_y_continuous() +
    theme(
      legend.position = 'bottom',
      axis.text.x = element_text(
        face = 'bold',
        color = "#993333",
        size = 15
      ),
      legend.title = element_blank(),
      #legend.text = element_text(sort)
      axis.text.y = element_text(
        face = 'bold',
        color = "#993333",
        size = 15,
        hjust = 0.95,
        vjust = 0.5,
        lineheight = 15
      )
      #  axis.line.y = element_line(color = 'black', linewidth = 1.0)
    ) +
    coord_flip() +
    #scale_fill_manual(values=color_palette)+
    labs(title = 'PERCENT SCORE DISTRIBUTION', caption = question_type)
  g
  # ggsave('horizontal_bar_plot.png')
  return(g)
}
get_detailed_objects <-
  function(client_name, report_year, question_type) {
    #detailed_questions_plot<-get_detailed_analysis(convert_scores(formatted_results),question_type)
    detailed_questions_plot2 <-
      get_detailed_analysis_2(formatted_results,
                              question_type,
                              summary_demographics_table)
    detailed_questions_table <-
      get_detailed_analysis_2(formatted_results,
                              question_type,
                              summary_demographics_table)
    
    questions_filtered_table <- get_questions(question_type)
    distribution_scores_plots <- get_score_distribution_plot(calculate_all_questions(formatted_results))
    objects_list <- list(
      #detailed_questions_plot,
      'questions plot' = detailed_questions_plot2,
      'questions table' =  detailed_questions_table,
      'filtered table' = questions_filtered_table,
      'distribution scores' = distribution_scores_plots
    )
    return(objects_list)
  }
