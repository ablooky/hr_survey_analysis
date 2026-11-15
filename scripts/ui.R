library(plotly)



header <- dashboardHeader(
  title = "My Dashboard"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview", icon = icon("gauge")),
    menuItem("DEMOGRAPHICS", tabName = "demographicsarchived scripts
             ", icon = icon("chart-line")),
    menuItem("ENGAGEMENT", tabName = "engagement", icon = icon("table")),
    menuItem("RECRUITMENT", tabName = "recruitment", icon = icon("gear"))
  )
)

body<-dashboardBody()


##summary ----
summary_tab<-tabItem (tabName = "summary",
                      tags$br(),tags$br(),tags$br(),
                      fluidRow(
                        box(width=6,
                            title='OVERALL FAVORABILITY',
                            status='primary',
                            solidHEADER=TRUE,
                           # plotlyOutput('summarize_favorability_plot',height=280)
                        ),
                        box(width=6,
                            title='SURVEY ENGAGEMENT',
                            status='primary',
                            solidHEADER=TRUE,
                           # plotlyOutput('summarize_participation_plot',height=280)
                        )
                      ),
                      fluidRow(box(width=6,
                                   title='LOWEST SCORING QUESTIONS',
                                   status='primary',
                                   solidHeader=TRUE,
                                  #DT::dataTableOutput('lowest_scores_table')
                      ),
                      box(width=6,
                          title='HIGHEST SCORING QUESTIONS',
                          status='primary',
                          solidHeader=TRUE,
                          #DT::dataTableOutput('highest_scores_table')
                      )
                      )
)

#demographics tab ----
demographics_tab<-tabItem (tabName = "demographic_analysis", 
                           # selectInput("selected_attribute", 
                           #             label = h4("Select Demographic Attribute"),
                           #             choices = categories, 
                           #             selected = 1), 
                           hr(),
                           fluidRow(
                            # uiOutput('demographics_UI')
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
                      # DT::dataTableOutput('demographic_analysis_table'),
                       hr(), 
                       h3('Analysis by Survey Job Category'),
                       # DT::dataTableOutput('summary_demographics_analysis_table')
)
t<-fluidRow(
# ---- Tab 1: Overview -------------------------------------------------
tabItem(
  tabName = "overview",
  fluidRow(
    valueBoxOutput("box_users", width = 4),
    valueBoxOutput("box_sales", width = 4),
    valueBoxOutput("box_visits", width = 4)
  ),
  fluidRow(
    box(
      title = "Welcome", status = "primary", solidHeader = TRUE,
      width = 12,
      p("This is a responsive shinydashboard app. Resize your browser ",
        "window and the layout will reflow automatically thanks to the ",
        "bootstrap grid used by shinydashboard (fluidRow / column / box ",
        "widths)."),
      p("Use the sidebar to navigate between the four tabs, and check ",
        "the bell, envelope, and checklist icons in the top-right of the ",
        "header for the dropdown menus.")
    )
  )
),

# ---- Tab 2: Analytics --------------------------------------------------
tabItem(
  tabName = "analytics",
  fluidRow(
    box(
      title = "Controls", status = "primary", solidHeader = TRUE,
      width = 4,
      sliderInput("n_points", "Number of points:",
                  min = 10, max = 500, value = 100, step = 10),
      selectInput("plot_color", "Point color:",
                  choices = c("steelblue", "darkred", "forestgreen", "purple"))
    ),
    box(
      title = "Scatter Plot", status = "primary", solidHeader = TRUE,
      width = 8,
      #plotOutput("scatter_plot", height = "350px")
    )
  )
),

# ---- Tab 3: Data Table ---------------------------------------------
tabItem(
  tabName = "data",
  fluidRow(
    box(
      title = "Sample Data", status = "primary", solidHeader = TRUE,
      width = 12,
     # DTOutput("data_table")
    )
  )
),

# ---- Tab 4: Settings -------------------------------------------------
tabItem(
  tabName = "settings",
  fluidRow(
    box(
      title = "App Settings", status = "primary", solidHeader = TRUE,
      width = 6,
      textInput("user_name", "Display name:", value = "Guest"),
      checkboxInput("dark_mode", "Enable dark mode (cosmetic only)", value = FALSE),
      actionButton("save_settings", "Save", icon = icon("save"),
                   class = "btn-primary")
    ),
    box(
      title = "Current Settings", status = "info", solidHeader = TRUE,
      width = 6,
      #verbatimTextOutput("settings_output")
    )
  )
)
)