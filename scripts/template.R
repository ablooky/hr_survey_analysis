#Generate survey scores 

library(openxlsx) 
questions <- read.xlsx('data/clientA.xlsx', sheet = 'Raw Results')
departments <- c(
  'Finance',
  'Accounting',
  'Administration',
  'Creative',
  'Marketing',
  'Communications',
  'Human Resources',
  'Operations',
  'IT'
)

profile_categories <- read.xlsx('data/clientA.xlsx', sheet = 'Headers')
first_names <- read.csv('data/new-top-firstNames.csv')
surnames <- read.csv('data/new-top-surnames.csv')

generate_fake_results <- function(df) {
  sample_raw <- df
  col_index <- as.numeric(ncol(sample_raw))
  for (n in 1:nrow(sample_raw)) {
    for (k in 3:col_index) {
      sample_raw[n, k] <- round(runif(1, min = 0, max = 5), 0)
    }
  }
  return(sample_raw)
}


generate_scores <- function(years_list, participants = 60) {
  export_list <- list()
  for (y in 1:length(years_list)) {
    #dir.create(paste0('output/',years_list[y]))
    #generate random name
    Employees_list <- c()
    random1 <- sample(first_names[, 2], participants, replace = FALSE)
    random2 <- sample(surnames[, 2], participants, replace = FALSE)
    for (i in 1:participants) {
      Employees_list <- c(paste0(random1[i], ' ', random2[i]), Employees_list)
    }
    # assign employee names to  score results
    scores_sheet <- cbind(questions[,1:3],as.data.frame(matrix(NA,nrow(questions),participants)))
    colnames(scores_sheet)[4:(participants + 3)] <- Employees_list
    #assign scores
    for (j in 1:nrow(scores_sheet)) {
      t <- sample(1:5, participants, replace = TRUE)
      
      scores_sheet[j, 4:(participants + 3)] <- t
      
    }
    #Non_participation
    n <- sample(1:participants, 4, replace = FALSE)
    scores_sheet[,(n + 3)] <- 0
    # scores_sheet[,c(20,26,38)]<-0
    
    #create profile matrix
    profile_matrix <-
      matrix(0, nrow = participants, ncol = nrow(profile_categories))
    rownames(profile_matrix) <- Employees_list
    colnames(profile_matrix) <- profile_categories[, 2]
    n <- sample(1:participants, participants, replace = FALSE)
    #Leadership #Director
    profile_matrix[n[1:4], 1] <- 1
    #Manager
    profile_matrix[n[5:9], 2] <- 1
    # Team Lead
    profile_matrix[n[10:15], 3] <- 1
    # Individual
    profile_matrix[n[16:participants], 4] <- 1
    n <- sample(1:participants, participants, replace = FALSE)
    #Assigning department
    profile_matrix[n[1:8], 5] <- 1
    profile_matrix[n[9:16], 6] <- 1
    profile_matrix[n[17:24], 7] <- 1
    profile_matrix[n[25:32], 8] <- 1
    profile_matrix[n[33:40], 9] <- 1
    profile_matrix[n[41:participants], 10] <- 1
    #Assigning Gender
    n <- sample(1:participants, participants, replace = FALSE)
    #Male
    profile_matrix[n[1:23], 11] <- 1
    profile_matrix[n[24:participants], 11] <- 0
    #Female
    counter = 1
    for (p in 1:nrow(profile_matrix)) {
      if (profile_matrix[p, 11] == 0) {
        #if not Male
        profile_matrix[p, 12] <- 1 #assign female
        profile_matrix[p, 13] <- 0
        
      }
      if (profile_matrix[p, 11] == 1) {
        # if Male
        profile_matrix[p, 12] <- 0
        profile_matrix[p, 13] <- 0
        
      }
      if (p %% 10 == 0) {
        #assign Other gender
        profile_matrix[p, 11] <- 0
        profile_matrix[p, 12] <- 0
        profile_matrix[p, 13] <- 1
        
      }
      counter = counter + 1
    }
    #Assign Tenure
    n <- sample(1:participants, participants, replace = FALSE)
    profile_matrix[n[1:20], 14] <- 1
    profile_matrix[n[21:30], 16] <- 1
    profile_matrix[n[31:40], 17] <- 1
    profile_matrix[n[41:participants], 18] <- 1
    #profile_matrix_final<- tibble::rownames_to_column(as.data.frame(profile_matrix, "Participants"))
    export_list[[years_list[y]]] <- list(
      'Raw Results' = scores_sheet,
      'Profiles' = profile_matrix,
      'Headers' = profile_categories
    )
   # write.xlsx(export_list[[years_list[y]]], paste0('output/', years_list[y], '.xlsx'), rowNames = T)
    
    wb <- createWorkbook()
    addWorksheet(wb, "Raw Results")
    writeData(wb, sheet = "Raw Results", x = scores_sheet , rowNames = FALSE)
    addWorksheet(wb, "Profiles")
    writeData(wb, sheet = "Profiles",x = profile_matrix,rowNames = T)
    addWorksheet(wb, "Headers")
    writeData(wb, sheet = "Headers",x= profile_categories,rowNames = FALSE)
    saveWorkbook(wb, paste0('output/', years_list[y], '.xlsx'), overwrite = TRUE)
  }
}

generate_scores(years_list = c('2018', '2019'),43)
