library(tidyverse)
library(openxlsx)

fn_import_data<-function(){
  employees<<-openxlsx::read.xlsx('data/ABC_HR_Dataset.xlsx', 
                                 sheet = 'Employee Directory',
                                 startRow = 3, colNames = TRUE)
  engagement<<-openxlsx::read.xlsx('data/ABC_Engagement_Survey_2025.xlsx', 
                        sheet =  'Survey Raw Data', startRow = 4, 
                        colNames = TRUE)
  survey_questions<<-openxlsx::read.xlsx('data/ABC_Engagement_Survey_2025.xlsx', 
                         sheet =  'Question Reference', startRow = 3, 
                         colNames = TRUE)
  recruitment<<-openxlsx::read.xlsx('data/ABC_Recruitment_Dataset.xlsx', 
                          sheet = 'Recruitment Pipeline', 
                          startRow = 3, colNames = TRUE)
}

fn_import_data()

fn_clean_headers<-function(ds){
    headers<- trimws(gsub('|\\(\\$\\)|#', '', 
                   names(ds), ignore.case = TRUE))
      names(ds)<- toupper(
          gsub('\\.|\\-|\\s+|\\(\\$\\)|#', '_', headers, ignore.case = TRUE)
        )
    print(names(ds))
    return(ds)
  }
fn_clean_datasets<-function(employees, engagement, recruitment){

  
  cleaned_employees <- employees %>%
    fn_clean_headers()
  cleaned_recruitment <- recruitment %>%
    fn_clean_headers()
  cleaned_engagement <- engagement %>%
    fn_clean_headers()
  cleaned_survey_questions <- survey_questions %>%
    fn_clean_headers() %>%
    rename(QUESTION = Q, 
           QUESTION_CATEGORY = CATEGORY, 
           QUESTION_DESC = QUESTION_TEXT)%>%
    select(c(QUESTION, QUESTION_CATEGORY, QUESTION_DESC))
  for(r in 2:nrow(cleaned_survey_questions)){
    if(is.na(cleaned_survey_questions[r,'QUESTION_CATEGORY'])){
             cleaned_survey_questions[r,'QUESTION_CATEGORY'] <- cleaned_survey_questions[(r-1),'QUESTION_CATEGORY']
             } 
    } 
    
  return(list = 
           'employees' = cleaned_employees, 
         'recruitment' = cleaned_recruitment, 
         'engagement' = cleaned_engagement)
}
fn_analyze_recruitment<-function(recruitment){
  
}

fn_analyze_employee_demographics<-function(cleaned_employees){
  active_ds<-cleaned_employees%>%
    filter(STATUS == 'Active')
  total_number_active<-active_ds %>% nrow()
  gender_df<- active_ds %>%
    group_by(GENDER)%>%
    summarise(count = n(), 
              percentage = round(count/total_number_active, 2))
  
  gender_gap_department_df<- active_ds %>%
    group_by(DEPARTMENT, GENDER)%>%
    summarise(count = n(), 
              percentage = round(count/total_number_active, 2))
  gender_gap_leadership_df<- active_ds %>%
    group_by(LEADERSHIP_LEVEL, GENDER)%>%
    summarise(count = n(), 
              percentage = round(count/total_number_active, 2))
ethnicity_df<- active_ds %>%
  group_by(ETHNICITY)%>%
  summarise(count = n(), 
            percentage = round(count/total_number_active, 2))
department_df<- active_ds %>%
  group_by(DEPARTMENT)%>%
  summarise(count = n(), 
            percentage = round(count/total_number_active, 2))

disability_df<- active_ds %>%
  group_by(DISABILITY)%>%
  summarise(count = n(), 
            percentage = round(count/total_number_active, 2))
leadership_df<- active_ds %>%
  group_by(LEADERSHIP_LEVEL)%>%
  summarise(count = n(), 
            percentage = round(count/total_number_active, 2))

hiring_timeline_df<-cleaned_employees%>%
  select(c(EMPLOYEE_ID, STATUS, DATE_HIRED))%>%
  arrange(DATE_HIRED)


  }
fn_analyze_survey_results<-function(cleaned_engagement, cleaned_survey_questions){
  question_categories<-unique(cleaned_survey_questions$QUESTION_CATEGORY)
  
}

