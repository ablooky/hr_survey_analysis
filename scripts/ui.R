##summary ----
summary_tab<-tabItem (tabName = "summary",
                      tags$br(),tags$br(),tags$br(),
                      fluidRow(
                        box(width=6,
                            title='OVERALL FAVORABILITY',
                            status='primary',
                            solidHEADER=TRUE,
                            plotlyOutput('summarize_favorability_plot',height=280)
                        ),
                        box(width=6,
                            title='SURVEY ENGAGEMENT',
                            status='primary',
                            solidHEADER=TRUE,
                            plotlyOutput('summarize_participation_plot',height=280)
                        )
                      ),
                      fluidRow(box(width=6,
                                   title='LOWEST SCORING QUESTIONS',
                                   status='primary',
                                   solidHeader=TRUE,
                                   DT::dataTableOutput('lowest_scores_table')
                      ),
                      box(width=6,
                          title='HIGHEST SCORING QUESTIONS',
                          status='primary',
                          solidHeader=TRUE,
                          DT::dataTableOutput('highest_scores_table')
                      )
                      )
)

#demographics tab ----
demographics_tab<-tabItem (tabName = "demographic_analysis", 
                           selectInput("selected_attribute", 
                                       label = h4("Select Demographic Attribute"),
                                       choices = categories, 
                                       selected = 1), hr(),
                           fluidRow(
                             uiOutput('demographics_UI')
                           )
                           # fluidRow(box(width=12,
                           #              plotOutput('attribute_plot')
                           #              ))
)
#downloads tab ui ----
downloads_tab<-tabItem(tabName='downloads', 
                       h3(HTML('Analysis by Survey Questions'),
                          style = 'color:blue;font-weight:bold;'),
                       hr(), 
                       DT::dataTableOutput('demographic_analysis_table'),
                       hr(), 
                       h3('Analysis by Survey Job Category'),
                       DT::dataTableOutput('summary_demographics_analysis_table')
)