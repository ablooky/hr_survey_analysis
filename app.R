library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

source('scripts/ui.R')

ui  <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)

server <- function(input, output) {

}

shinyApp(ui, server)