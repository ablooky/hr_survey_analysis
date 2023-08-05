#library(shiny)
#library(shinydashboard)
#library(shinydashboardPlus)
library(shinyWidgets)
library(bs4Dash)
library(reactable)
library(fresh)
library(plotly)
#library(tidyverse)
source('scripts/ui.R')
#source('analyze.R')
source('visualize.R')

clients<-list.files('data/clients')


#ui ----

header <- dashboardHeader(fixed=T, 
                          # use_googlefont("Roboto Slab"),
                         # use_googlefont("Open Sans"),
                         use_googlefont("Oswald"),
                      #    use_googlefont("Montserrat"),
                          #skinSelector()
                          title='HR APP',
                          tags$h1('HUMAN RESOURCES SURVEY ENGAGEMENT ANALYSIS',
                                  style = "font-size: 30px ; color: #FF0000 ;")
)
sidebar <- dashboardSidebar(
  width = 220,
  minified = T,
  collapsed = F,
  hr(),br(),
  pickerInput(
    inputId = 'select_client_dropdown',
    label = 'Select Client',
    choices = clients,
    selected = 1, 
    options = list(
      style = "btn-primary")
  ),
  pickerInput(
    inputId = 'select_survey_year_dropdown',
    label ='Select Survey Year',
    choices = get_years_list(clients[1]),
    selected = 1, 
    options = list(style = "btn-primary"),
    choicesOpt = list(
      icon = c("glyphicon-ok-sign","glyphicon-ok-sign","glyphicon-ok-sign"))
  ),
  sidebarMenu(
    id = 'sidebar', 
    hr(),br(),
    menuItem(
      "SUMMARY",
      #selected = T,
      tabName = 'summary',
      icon = icon('file-lines')
    ),
    menuItem(
      "DETAILS",
      selected=TRUE,
      tabName = 'detailed_analysis',
      icon = icon('chart-bar')
      
    ),
    menuItem(
      "DEMOGRAPHIC",
      # selected=TRUE,
      tabName = 'demographic_analysis',
      icon = icon('th-large')
      
    ),
    menuItem("DOWNLOADS",
            # selected=TRUE,
             tabName = 'downloads',
             icon = icon('download')
    )
  )
)

# Detailed tab ui ----
detailed_tab <- tabItem (
  tabName = "detailed_analysis",
  selectInput(
    "selected_question_category",
    label = h4("View Results by Question Category"),
    choices =  get_questions_category(),
    selected = 1
  ),
  hr(),
  conditionalPanel(
    condition = "input.selected_question_category == 'All Questions'",
    #column(4,
    #offset=0,
    #plotOutput('detailed_all_questions_plot1',height=150)),
    column(
      11, offset = 1, reactableOutput('detailed_questions_table'), 
      # plotOutput('detailed_all_questions_plot1'),
      #  plotOutput('detailed_all_questions_plot2')
    )),
  conditionalPanel(
    condition = "input.selected_question_category != 'All Questions'",
   # box(width = 6, 
        # plotOutput('detailed_questions_plot', height = 200),
        plotOutput('detailed_questions_plot2', height = 300)
   # ),
   # box(
   #   width = 6,
   #   tableOutput('questions_filtered_table'),
   #   height = 200
   # )
  )
  
)
#body ----
body <- dashboardBody(width = 900,
                      tabItems(
                        summary_tab,
                        detailed_tab,
                        demographics_tab,
                        downloads_tab
                      ))
# Server ----
server <- function(input, output) {
  client_name<-clients[1]
  report_year<-get_years_list(client_name)[1]
  
  # categories_df<-get_categories_df(client_name,fileindex)
  
  selectedQuestionCategory<-reactive({input$selected_question_category})
  #selectedParticipant<-reactive({input$selected_profile})
  selectedDemographicAttribute<-reactive({input$selected_attribute})
  
  # Summary Analysis ----
  summary_objects_list<-get_summary_objects(client_name, report_year)
  # output$lowest_scores_table<-shiny::renderDT(summary_objects_list[[1]],
  #                                                rownames = FALSE,
  #                                                colnames=FALSE)
  # output$highest_scores_table<-shiny::renderTable(summary_objects_list[[2]],
  #                                                 rownames = FALSE,
  #                                                 colnames = FALSE)
  #print(summary_objects_list[[1]])
  output$lowest_scores_table<-DT::renderDT(summary_objects_list[[1]],
                                           #  colnames = F,
                                           rownames = F,
                                           options = list(
                                             pageLength = 5,
                                             dom = 't'
                                           ))
  output$highest_scores_table<-DT::renderDT(summary_objects_list[[2]],
                                            # colnames = F,
                                            rownames = F,
                                            options = list(
                                              pageLength = 5,
                                              dom = 't'
                                            ))
  output$summarize_favorability_plot<-renderPlotly(summary_objects_list[[3]])
  output$summarize_participation_plot<-renderPlotly(summary_objects_list[[4]])
  
  
  # Demographic Analysis ----
  observe({
    demographics_objects_list <-
      get_demographics_objects(client_name, report_year,
                               selectedDemographicAttribute())
    # })
    
    
    
    
    output$demographics_UI<-renderUI({
      plotheight <- reactive({
        demographics_objects_list[['cat elements']]
      })
      fluidRow(
      box (width=8, status='primary',
        # title = 'Average Favorability Score Per Question Category and Demographic Attribute',
        h3('Average Favorability Score Per Question Category & Demographic Attribute')         ,
        #plotOutput('attribute_category_plot'),collapsible=F)
      #observe({
      # output$demographics_attribute_plot <-
      #   renderPlot(demographics_objects_list[[2]],height = 4*225)
      # observe({
      #print(plotheight())
     # output$attribute_category_plot <-
        renderPlot(demographics_objects_list[['attribute plot']],
                   height = plotheight() *225)
      ),
      box(width=4,  
          h3('Demographic Breakdown of Survey'),
          status='primary',
         # output$demographic_breakdown_pie <-
            renderPlotly(demographics_objects_list[['pie chart']])
          #plotlyOutput('demographic_breakdown_pie',height='100%')
      )
      )
    })

  })
  #Downloads ----
  downloadable_objects <-
    get_downloadable_objects(client_name, report_year)
  output$demographic_analysis_table <-
    DT::renderDataTable(server = F, {
      DT::datatable(
      downloadable_objects[[1]],
      caption = 'Survey Analysis (Average Score by Demographic)',
      extensions = 'Buttons',
      selection = 'none',
      rownames = F,
      options = list(
        #pageLength = 5,
        autoWidth = F,
        dom = 'Brltip',
        scrollX = TRUE,
        buttons = list(
           'copy', 'print',
           list(
            extend = 'collection',
            buttons = list(
              list(extend = "csv", filename = "page",exportOptions = list(
                columns = ":visible",modifier = list(page = "current"))
              ),
              list(extend = 'excel', filename = "page", title = NULL, 
                   exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
            text = 'Download current page'),
          list(
            extend = 'collection',
            buttons = list(
              list(extend = "csv", filename = "data",exportOptions = list(
                columns = ":visible",modifier = list(page = "all"))
              ),
              list(extend = 'excel', filename = "data", title = NULL, 
                   exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
            text = 'Download all data')
        ),
        # add the option to display more rows as a length menu
        lengthMenu = list(c(5,10, 30, 50, -1),
                          c('5','10', '30', '50', 'All'))
      ),
      class = "display"
    )
      }
      )
   
  output$summary_demographics_analysis_table <-
    DT::renderDT(
      downloadable_objects[[2]],
      caption = 'Survey Analysis (Average Score by Question Category)',
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Brt',
        buttons = c('copy','excel', 'print'),
        scrollX = TRUE
      )
    )
  
  ## Detailed analysis ----
  observe({
    detailed_objects<-
      get_detailed_objects(client_name, report_year,selectedQuestionCategory())
    #output$detailed_questions_plot<-renderPlot(detailed_objects[['questions plot']])
    output$detailed_questions_plot2<-renderPlot(detailed_objects[['questions plot']])
    output$detailed_questions_table<-renderReactable({detailed_objects[['questions table']]})
    output$questions_filtered_table<-renderTable(detailed_objects[['filtered table']],
                                                 rownames=FALSE, 
                                                 colnames=FALSE)
    #output$detailed_all_questions_plot1<-renderPlot(detailed_objects[[4]][[1]])
    # output$detailed_all_questions_plot2<-renderPlot(detailed_objects[[4]][[2]])
    
  })
  
}
#----
ui <- dashboardPage(
  #options = list(sidebarExpandOnHover = TRUE),
  header = header,
  sidebar = sidebar,
  body = body,
  #controlbar = dashboardControlbar(),
  title = "HR SURVEY DEMO"
 # skin='red'
  
)
shinyApp(ui = ui,
         server = server)