library(tidyverse)
library(data.table)
#library(janitor)

source('import.R')

# Modify datasets
string_replace_headers<-function(df){
  #names(df) <- gsub("\\.", "_", names(df))
  names(df) <- gsub("[[:punct:]]", "_", names(df))
  return(df)
}

modify_profile<-function(profile_dataset,categories,participants_list){
  profile_valid<- tidyr::gather(profile_dataset,
                                 key='category_desc', 
                                 value='score',-Participants) %>% 
    filter(score==1) %>% 
    select(!score) %>% 
    mutate(category = NA)

  for (p in 1:nrow(profile_valid)) {
    for (n in 1:nrow(categories_df)){
      if(categories_df[n,'category_desc'] == profile_valid[p,'category_desc']) {
        profile_valid[p,'category']<-categories_df[n,'category']
      }
    }
  }
  
  #create a revised profile 
  profile_revised<- matrix(data=NA,
                           ncol=length(categories),
                           nrow=length(participants_list))
  colnames(profile_revised)<-categories
  rownames(profile_revised)<-participants_list
  
  #Populate profile_revised
  for (p in participants_list) {
    profile_individual<-profile_valid %>% filter(Participants==p)
    for (c in 1:nrow(profile_individual)) {
      profile_revised[p,profile_individual[c,'category']]<-profile_individual[c,'category_desc']
    }
  }
  return(profile_revised)
}

# Generate datasets ----
generate_formatted_results<-function(raw_results, participants_list){
  results<-raw_results %>% 
    select(!c(Question_number,Question_category,Question_desc)) %>% 
    t() %>% data.table() %>% as.matrix()
  rownames(results)<-participants_list
  colnames(results)<-as.character(questions_table$Question_number)
  return(results)
}

# Summary analysis #----
convert_scores<-function(ds){
  for (i in 1:nrow(ds)){
    ds[i,]<- gsub(c(0),'DNK',ds[i,])
    ds[i,]<- gsub(1,'Unfavorable',ds[i,])
    ds[i,]<- gsub(2,'Unfavorable',ds[i,])
    ds[i,]<- gsub(3,'Neutral',ds[i,])
    ds[i,]<- gsub(4,'Favorable',ds[i,])
    ds[i,]<- gsub(5,'Favorable',ds[i,])
  }
  return(ds)
}
generate_scores_per_question<-function(df){
  add_score<-matrix(0,nrow=1,ncol=ncol(df))
  rownames(add_score)<-'overall_question_score'
  colnames(add_score)<- colnames(df)
  df_scored<-rbind(df, add_score)
  for (p in 1:ncol(df_scored)){
    df_scored['overall_question_score',p]=round(sum(as.numeric(df_scored[,p]),na.rm=TRUE),digits=0)
  }
  
  return(df_scored)
}
generate_scores_per_participant<-function (raw_results_transposed){
  add_score<-matrix(0,nrow=nrow(raw_results_transposed),ncol=1)
  colnames(add_score)<-'overall_score'
  rownames(add_score)<- rownames(raw_results_transposed)
  raw_results_scored<-cbind(raw_results_transposed, add_score)
  for (p in 1:nrow(raw_results_scored)){
    raw_results_scored[p,'overall_score']=sum(as.numeric(raw_results_scored[p,]),na.rm=TRUE)
  }
  return(raw_results_scored)
}
summarize_score<-function(df,questions_table){
  t<-convert_scores(t(df))
  total_number_participants<-ncol(t)
  scored_df<-data.frame(rownames(t))
  for (n in 1:nrow(t)){
    favorability_counter<-0
    for(m in 1:ncol(t)){
      if(t[n,m]=='Favorable'){
        favorability_counter<-favorability_counter + 1
      }
    }
    scored_df[n,'favorability_score']<-favorability_counter
    scored_df[n,'Percent_Favorability']<-100*favorability_counter/total_number_participants
  }
  scored_df2<-cbind(scored_df,questions_table)
  results_finalized<-scored_df2 %>% 
    select(c(Question_number,Question_desc,Percent_Favorability))
  t_asc<- results_finalized %>% arrange(Percent_Favorability)
  t_desc<- results_finalized %>% arrange(desc(Percent_Favorability))
  #t_asc<-results_finalized[with(results_finalized,order(Percent_Favorability)), ]
 # t_desc<-results_finalized[with(results_finalized,order(-Percent_Favorability)), ] 
  lowest_results<-t_asc %>% slice(1:5)  
  lowest_results$Percent_Favorability= paste0(lowest_results$Percent_Favorability,'%')
  highest_results<-t_desc %>% slice(1:5) 
  highest_results$Percent_Favorability=paste0(highest_results$Percent_Favorability,'%')
  names(lowest_results)<-c("Question Number",'Question Description','Favorability Score')
  names(highest_results)<-c("Question Number",'Question Description','Favorability Score')
  resultset<-list(lowest_results,highest_results)
  return(resultset)
}
calculate_favorability2<-function(df){
  favorable_score=0
  neutral_score=0
  non_favorable_score=0
  dont_know_score=0
  description=c('Favorable', 'Neutral', 'Unfavorable','DNK')
  for (m in 1:nrow(df)){
    for (n in 1:ncol(df)){
      if(df[m,n]=='DNK'){
        dont_know_score = dont_know_score + 1
      }
      if(df[m,n]=='Unfavorable'){
        non_favorable_score=non_favorable_score+1
      }
      if(df[m,n]=='Favorable'){
        favorable_score = favorable_score + 1
      }
      if(df[m,n]=='Neutral'){
        neutral_score = neutral_score + 1
      }
    }
  }
  # all datapoints
  summary_df<-data.frame(description,
                         score = c(favorable_score, neutral_score,non_favorable_score,dont_know_score)
  )
  for (n in 1:nrow(summary_df)){
    summary_df$percent_score[n]<-summary_df$score[n]/sum(summary_df$score)}
  return(summary_df)
}
generate_favorability_doughnut_plot<-function(df,color_palette2){
  summary_df<-calculate_favorability2(convert_scores(df))
  fig <- plot_ly(summary_df, 
                 labels = ~description, 
                 values = ~score,
                 #text=~description, 
                 textfont=list(color='white')
  ) %>%
    add_pie(hole=0.6) %>%
    layout(title = "",  showlegend = FALSE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations=list(x=0, 
                            y=0,
                            xref='x',
                            yref='y',
                            text=paste0(round((100*summary_df[summary_df$description=='Favorable','percent_score']),digits=0),'%'),
                            showarrow=F,
                            font=list(size=50, 
                                      color=color_palette2[1])))
  return(fig)
}
generate_participation_pie_chart<-function(df){
  total_number_participants<-as.numeric(nrow(df))
  abscence<-0
  for (i in 1:nrow(df)){
    sum_of_row<-sum(as.numeric(df[i,]))
    if(sum_of_row == 0){
      abscence<-abscence+1
    }
  }
  participation_rate<-(1-(abscence/total_number_participants))*100
  abscence_rate<-abscence/total_number_participants*100
  engagement_df<-data.frame(items=c('abscence','Participation'),
                            engagement=c(abscence, (total_number_participants-abscence)),
                            rate=c(abscence_rate,participation_rate))
  fig<-plot_ly(engagement_df, labels = ~items, values = ~engagement, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               insidetextorientation='radial',
               hoverinfo = 'text',
               text = ~paste(engagement, 'responders'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
  fig <- fig %>% 
    layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  fig
  return(fig)
}




## demographic analysis ----
round_number<- function(x,max_digits){
  if(is.numeric(x)){
    if(is.integer(x)){
      x
    }
    else{
      x<-round(x,digits=max_digits)
    }
  }
  else {x}
  return(x)
}
add_overall_score<-function(columnnames,summary_matrix) {
  add_score<-matrix(0, nrow=1,ncol=length(columnnames))
  colnames(add_score)<-columnnames
  rownames(add_score)<-'Overall Score'
  for ( s in columnnames){
    add_score['Overall Score',s]<-sum(summary_matrix[,s])
  }
  summary_ds<-rbind(add_score, summary_matrix)
  return(summary_ds)
}

generate_demographics_ds<-function(profile_dataset, formatted_results){
  analysis<-matrix(0,ncol=length(categories_desc),nrow=nrow(questions_table))
  colnames(analysis)<-categories_desc
  rownames(analysis)<-questions_table$Question_number
  for (x in categories_desc){
    temp_df<- profile_dataset %>%  select('Participants',all_of(x)) 
    temp_df[,x]<-as.numeric(temp_df[,x])
    temp_df<-filter(temp_df, temp_df[,x]==1)
    temp_names<-temp_df['Participants']
    if(nrow(temp_names)==0){
      analysis[r,x]<-0
    }
    else {
      for (r in rownames(analysis)){
        ave_per_question<-0
        sum_per_question<-0
        for(p in (temp_names[,'Participants'])){
          sum_per_question<-sum_per_question + as.numeric(formatted_results[p,r])
        }
        ave_per_question<-round(sum_per_question/nrow(temp_names),3)
        analysis[r,x]<-ave_per_question
      }
    }
    
  }
  return(analysis)
}
generate_demographics_ds_scored<-function(analysis){
  columnnames<-colnames(analysis)
  summary_ds<-add_overall_score(columnnames,analysis)
  for (k in colnames(summary_ds)){
    for (j in rownames(summary_ds)){
      summary_ds[j,k]<-round_number(summary_ds[j,k],1)
    }
  }
  #add_questions<-data.frame('Questions Description' = c('All Questions', questions_list))
  add_questions<-data.frame('Questions Description' = questions_list)
  new_ds<-cbind(rownames(summary_ds),add_questions, summary_ds)
  names(new_ds)<-c('Question Number', 'Question Description', columnnames)
  return(new_ds)
}
generate_summary_demographics_ds<-function(ds){
  columnnames<-colnames(ds)
  new_questions_category<-questions_category[2:length(questions_category)]
  summary_matrix<-matrix(0,ncol=length(columnnames),nrow=length(new_questions_category))
  colnames(summary_matrix)<-columnnames
  rownames(summary_matrix)<-sort(new_questions_category)
  for(col in columnnames){
    for(q in new_questions_category){
      get_questions_number<-c()
      for (x in 1:nrow(questions_table)){
        if (questions_table[x,'Question_category']==q){
          get_questions_number<-c(get_questions_number,questions_table[x,'Question_number'])
        }
      }
      questions_category_score<-0
      for (r in get_questions_number){
        questions_category_score<-questions_category_score+ds[r,col]
      }
      summary_matrix[q,col]<-questions_category_score
    }
  }
  return(summary_matrix)
}
generate_summary_demographics_scored<-function(summary_matrix){
  columnnames<-colnames(summary_matrix)
  summary_ds<-add_overall_score(columnnames, summary_matrix)
  return(summary_ds)
}

#Detailed analysis ----

make_reactable_df5<-function(df,summary_demographics_table){
  df2<-data.frame(df[,1])
  df2[,2]<-c(1:nrow(df2))
  names(df2)<-c('Question Category','Favorability')
  plot_list<-list()
  for(i in 1:nrow(df2)){
    subset_df <- df[i,3:ncol(df)]
   # subset_df <- df[i,1:ncol(df)]
    subset_df<-tibble::rownames_to_column(data.frame(t(subset_df)))
    subset_df[,3]<-df[i,1]
    names(subset_df)<-c('Category','Score','Question_category')
    subset_df2<-subset_df %>% mutate(lab_ypos = cumsum(Score)-0.5*Score)
    g<-
      #ggplotly(
      ggplot(subset_df2, aes(x=Question_category,y=Score,fill=factor(Category,
                                                                     levels=c('Unfavorable',
                                                                              'DNK',
                                                                              'Neutral',
                                                                              'Favorable'
                                                                     ))))+
        geom_bar(position='stack',stat='identity')+ 
        coord_flip()+
        geom_text(aes(y=lab_ypos, label=Score),color='white',linewidth = 3) +
        scale_y_continuous(expand = c(0, 0)) + scale_x_discrete(expand = c(0, 0))+ 
        scale_color_manual(color_palette2)+
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid=element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.line=element_blank(),
              legend.position = 'none',
              axis.ticks.y = element_blank(),
        )
  #     height=20, 
  #     width = 240
  #   ) %>%
  #     config(displayModeBar = FALSE) %>% 
  # layout(autosize=F, margin = list(l=0,r=0,b=0,t=0,pad=-20)
    #xaxis=list(zeroline = FALSE,showline = FALSE,showticklabels = FALSE,showgrid = FALSE)
     # )
    plot_list[[i]]<-g 
    }
  subset_df<-generate_category_subset_df(df=summary_demographics_table,'Leadership',categories_df)
  subset_df<-subset_df[2:nrow(subset_df),]
  #subset_df
  # subset_df2<-t(subset_df)
  subset_df3<-subset_df %>% 
    mutate (Favorability = NA) %>% 
    tibble::rownames_to_column("Question Category") %>%
    as.data.frame()
  p<-reactable( 
    #subset_df3,
    df2,
                 defaultSorted = "Question Category",
                columns = list(
                'Question Category' = colDef(
                  details = colDef(
                    #name='Description',
                                   details = function(index){
                                    htmltools::div(
                                      "Category Breakdown (%)",
                                       htmltools::tags$pre(paste(
                                        capture.output(
                                           print(subset_df[index,])
                                       ),
                                        ),
                                         collapse = '\n')
                                   )
                },
                                   align='left', width = 400, html=T
                  )
                ),
                 Favorability = 
                   colDef(cell = function(value,index){
                 #    htmltools::plotTag(plot_list[[index]], alt="plots")
                  # plot_list[[index]]
                 }, width=260,align = "left")
               ), borderless = T, outlined = T,compact=T)
  p
  print(subset_df)
  # p <- reactable(
  #   df2,
  #   defaultSorted = "Question Category",
  #   columns = list(
  #     'Question Category' = colDef(
  #       details = colDef(
  #         name = 'Description',
  #         details = function(index) {
  #           temp_category<-df2[index,`Question Category` ]
  #          temp_table<-subset_df3[subset_df3$`Question Category` == temp_category,]
  #            htmltools::div(style = "padding: 16px",
  #             #"Category Breakdown (%)",
  #                          reactable::reactable(temp_table)
  #                          )
  #         })
  #     ),
  #     Favorability =
  #       colDef(
  #         cell = function(value, index) {
  #       #    htmltools::plotTag(plot_list[[index]], alt = "plots")
  #           # plot_list[[index]]
  #         })
  #   ),
  #   borderless = T,
  #   outlined = T,
  #   compact = T
  # )
  # p
  return(p)
}
generate_category_subset_df<-function(df,selected_category, categories_df){
  #demographics_variables<-colnames(df)
  subset_categories<-categories_df %>% filter(category == selected_category)
  subset_df<-data.frame(df)
  subset_df<-subset_df %>% select(all_of(subset_categories$category_desc))
  return(subset_df)
}
calculate_all_questions<-function(df){
  data<-data.frame(t(convert_scores(df)))
  num_participants<-nrow(df)
  fav_labels<-c('Favourable', 'Neutral', 'Unfavorable', 'DNK')
  y_axis_label<-questions_table$Question_desc
  data<-cbind(questions_table$Question_category,
              data)
  names(data)[1]<-'Question_category'
  cat<-data %>% select(Question_category) %>% distinct(Question_category)
  new_df<-data.frame(Category=NA, Response=NA,Favorable=NA, Neutral=NA,
                     Not_Favorable=NA, DNK=NA)
  subset_df<-data.frame() #subset data 
  for (q in 1:nrow(cat)){
    temp_df<-data.frame(Category=NA, Response=NA,Favorable=NA, Neutral=NA,
                        Not_Favorable=NA, DNK=NA)
    data_filtered<-data %>% filter(Question_category==cat[q,1]) %>%
      select(-Question_category)
    total<-num_participants*nrow(data_filtered)
    summary_df<-calculate_favorability2(data_filtered)
    temp_df[q,'Category']<-cat[q,1]
    temp_df[q,'Favorable']<-round(summary_df$score[1]/total*100,digits=1)
    temp_df[q,'Neutral']<-round(summary_df$score[2]/total*100,digits=1)
    temp_df[q,'Not_Favorable']<-round(summary_df$score[3]/total*100,digits=1)
    temp_df[q,'DNK']<-round(summary_df$score[4]/total*100,digits=1)
    temp_df[q,'Response'][[1]]<-list(summary_df$percent_score)
    new_df<-rbind(new_df,temp_df)
    new_df2<-distinct(new_df)
    new_df3<-new_df2[2:nrow(new_df2),]
    #data_converted<-convert_scores(data_filtered)
  }
  return(new_df3)
}
get_detailed_analysis <- function(df, question_type) {
  fav_labels <-
    c('Favourable', 'Neutral', 'Not Favourable', 'Does Not Know')
  questions_table <- questions_table %>%
    mutate(merged_question = paste0(Question_number, '. ',
                                    Question_desc))
  y_axis_lablel <- questions_table$merged_question
  g <- list()
  if (question_type == 'All Questions') {
    data <- data.frame(df)
    data_items <- data %>% 
      gather(key = question, value = score) %>%
      mutate(score = factor(score),
             question =
               factor(question))
    data_items2 <- data_items %>% dplyr::count(question, score) %>%
      mutate(y_pos = cumsum(n) / nrow(data) - (0.5 * n / nrow(data)),
             y_cumsum = cumsum(n)) %>%
      mutate(items_num = question, 
             items_num = gsub('X','Question ',items_num))
    data_merged <-
      inner_join(data_items2,
            questions_table,
            by = c('items_num' = 'question'))
    g <- ggplot(data_merged, aes(x = merged_question)) +
      geom_bar(aes(fill = score),
               #position = position_stack(reverse = TRUE))
               position = 'fill') +
      coord_flip() +
      scale_fill_manual(values = color_palette) +
      labs(title = "DISTRIBUTION OF FAVORABILITY SCORE", y = "Percentage") +
      theme(
        legend.position = 'bottom',
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
        axis.title.y = element_blank(),
        axis.text.x = element_text(
          size = 10,
          family = "Arial",
          color = color_palette[3],
          vjust = .5
        ),
        # X axis text
        axis.text.y = element_text(
          size = 11,
          family = "Arial",
          color = color_palette[1]
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
      )
    #g
    #list_of_graphs[[1]]<-g
  } else{
    g <- generate_Question_category_plots(df, question_type)
  }
  return(g)
}
get_detailed_analysis_2<-function(df,question_type,summary_demographics_table){
  g<-list()
  if(question_type=='All Questions'){
    g<-make_reactable_df5(df=calculate_all_questions(df),summary_demographics_table)
    g
  }
  else{
    g<-generate_Question_category_plots2(df,question_type)
  }
  return(g)
  
}
get_questions<-function(question_type){
  data<-questions_table %>% 
    filter(Question_category==question_type) %>%
    arrange(desc(Question_number)) %>%
    mutate(merged_question2=paste(question,Question_desc,sep = ' | ')) %>%
    select(merged_question2) 
  return(data)
}
# generate_Question_category_plots<-function(df,question_type){
#   list_of_graphs<-list()  
#   fav_labels<-c('Favourable', 'Neutral', 'Not Favourable', 'Does Not Know')
#   data<-data.frame(t(df))
#   #data<-data.frame(t(convert_scores(df)))
#   data<-cbind(questions_table$Question_category,data)
#   names(data)[1]<-'Question_category'
#   #names(data)[2]<-'question'
#   data_filtered<-data %>% filter(Question_category==question_type) %>%
#     select(-Question_category)
#   
#   for (x in 1:nrow(data_filtered)){
#     individual_matrix<-data_filtered[x,]
#     individual_matrix<-data.frame(t(individual_matrix)) 
#     question_number<-names(individual_matrix)[1]
#     question_selected<-filter(questions_table,question==question_number)
#     merged_question<-paste(question_selected$question,question_selected$question_desc,sep=' | ')
#     names(individual_matrix)[1]<-'question'
#     labels<-unique(individual_matrix$question)
#     data<-individual_matrix %>% 
#       dplyr::group_by(question) %>%
#       summarise(count=n()) %>% 
#       mutate(percentage=count/sum(count))
#     g<-ggplot(data, aes(x=factor(question),
#                         y=percentage*100,
#                         fill=factor(question))
#     ) + geom_bar(width = 0.5, stat='identity') +
#       theme(legend.position = 'none',
#             panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(), 
#             panel.background = element_rect(fill = color_palette[5]),
#             plot.title=element_text(size=16, 
#                                     face="bold", 
#                                     family="Arial",
#                                     color=color_palette[1],
#                                     hjust=0.5,
#                                     lineheight=1.2),  # title
#             axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
#             axis.title.y=element_text(size=12,family="Arial",color=color_palette[1]),  # Y axis title
#             axis.text.x=element_text(size=10,
#                                      family="Arial",
#                                      color=color_palette[3],
#                                      vjust=.5),  # X axis text
#             axis.text.y=element_text(size=10,
#                                      family="Arial",
#                                      color=color_palette[1])
#       )+
#       coord_flip()+
#       scale_fill_manual(values=color_palette,aesthetics = c("colour", "fill"))+
#       labs(title=strwrap("Score Distribution"), x="Response", y="Percentage", subtitle =Question_number, caption = wrapper(merged_question))
#     g
#     list_of_graphs[[x]]<-g
#   }
#   return(patchwork::wrap_plots(list_of_graphs,ncol=3))
# }
# generate_Question_category_plots2<-function(df,question_type){
#   #df<-formatted_results
#   data<-data.frame(t(df))
#   data<-cbind(questions_table$Question_category,data)
#   names(data)[1]<-'Question_category'
#   data_filtered<-data %>% filter(Question_category==question_type) %>%
#     select(-Question_category)
#   data_converted<-convert_scores(data_filtered)
#   new_df<-data.frame()
#   for (x in 1:nrow(data_filtered)){
#     data_calculated<-calculate_favorability2(data_converted[x,])
#     data_calculated$question<-rownames(data_filtered)[x]
#     data_calculated$percent_score<-data_calculated$percent_score*100
#     data_calculated<- data_calculated %>% mutate(lab_ypos = cumsum(percent_score) + 0.5 * percent_score) 
#     new_df<-rbind(new_df,data_calculated)
#     
#   }
#   
#   new_df=new_df %>% arrange(desc(question))
#   # questions<-unique(new_df$question)
#   #new_df$question<-factor(new_df$question, levels=sort(new_df$question, decreasing = TRUE))
#   g<-ggplot(new_df, aes(x= question,y=percent_score,fill=factor(description,
#                                                                 levels=c('DNK',
#                                                                          'Unfavorable',
#                                                                          'Neutral',
#                                                                          'Favorable'
#                                                                 )))) + 
#     geom_bar(width = 0.6, stat='identity',position='fill') +
#     theme_void()+ 
#     #scale_x_discrete(order(new_df$question,desc=TRUE))+
#     scale_x_discrete(sort(new_df$question, decreasing=TRUE))+
#     scale_y_continuous()+
#     theme(legend.position = 'bottom',
#           legend.title = element_blank(),
#           #legend.text = element_text(sort)
#           axis.text.y=element_text())+
#     coord_flip()+
#     #scale_fill_manual(values=color_palette)+
#     labs(title='PERCENT SCORE DISTRIBUTION',caption =question_type)+
#     geom_label(aes(x=question, y=lab_ypos,label=percent_score),position='fill',color='white')
#   g
#   
#   return(g)
# }



# generate_merged_dataset<-function(scored_results,profile_dataset){
#   merged_df<-cbind(scored_results,profile_revised)
#   return(merged_df)
# }
#wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {
 # paste(strwrap(label, dev_width * dev_scaler), collapse = "\n")
#}
#erase this one at the end
# generate_final_dataset<-function(df){
#   final_dataset<-matrix(0, nrow=nrow(questions_table),ncol=nrow(categories_df))
#   colnames(final_dataset)<-categories_df$category_desc
#   rownames(final_dataset)<-questions_table$question
#   leadership
#   #for(n in 1:ncol(final_dataset)){
#   for (n in categories_df$category_desc){
#     subset_df<-subset(df,)
#     for (i in 1:nrow(final_dataset)){
#       
#     }
#   }
# }
# 
# 
# 
# 
# calculate_favorability<-function(df){
#   favorable_score=0
#   neutral_score=0
#   non_favorable_score=0
#   dont_know_score=0
#   
#   for (m in 2:nrow(df)){
#     for (n in 1:ncol(df)){
#       if(df[m,n]==0){
#         dont_know_score = dont_know_score + 1
#       }
#       if(df[m,n] > 0 & df[m,n] < 3 ){
#         non_favorable_score=non_favorable_score+1
#       }
#       if(df[m,n] > 3 ){
#         favorable_score = favorable_score + 1
#       }
#       if(df[m,n]== 3 ){
#         neutral_score = neutral_score + 1
#       }
#     }
#   }
#   # all datapoints
#   summary_df<-data.frame(score = c(favorable_score, neutral_score,non_favorable_score,dont_know_score),
#                          description=c('Favorable', 'Neutral', 'Unfavorable', 'DNK'))
#   return(summary_df)
# }








# modify_raw_results<-function(raw_results){
#   raw_results2<-rename(raw_results_modified,question=change_column_name[1])
#   for (n in 1:nrow(raw_results)){
#     raw_results[n,'question']<-paste0('Question_',raw_results[n,'question'])
#   }
#   return(raw_results)
# }

# other<-function(){
#   for(b in 1:nrow(temp_df)){
#     for (d in 1:ncol(temp_df)){
#       temp_df[b,d]<-as.numeric(as.character(temp_df[b,d]))
#     }
#   }
# }
