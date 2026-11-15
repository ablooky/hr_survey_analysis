library(openxlsx)



#Import dataset
import_ds <- function(filename,sheetname,startrow) {
  openxlsx::read.xlsx(
    filename,
    sheet = sheetname,
    startRow = startrow,
    colNames = T)
}
#Import client survey data
import_client_data <- function(client_name,fileindex,sheetname,startrow) {
  files_list<-list.files(paste0('data/clients/',client_name,'/years/'))
  filename <- paste0('data/clients/',client_name,'/years/', files_list[fileindex])
  ds <- import_ds(filename,sheetname,startrow)
  return(ds)
}
get_raw_data<-function(client_name, fileindex){
  ds<-import_client_data(client_name,fileindex,'Raw Results',1)
  names(ds)[1:3]<-c('Question_number','Question_category','Question_desc')
  return(ds)
}

# Import survey categories
get_categories_df<-function(client_name, fileindex){
  df<-import_client_data(client_name,fileindex,'Headers',1)
  return(df)
}


#Import participants' list aka profiles
get_profiles<-function(client_name,fileindex,categories_desc){
  profiles_ds<-import_client_data(client_name,fileindex,'Profiles',1)
  colnames(profiles_ds)<-c('Participants',categories_desc)
  return(profiles_ds)
}

