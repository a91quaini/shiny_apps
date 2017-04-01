library(shiny)
library(DT)
library(tidyverse)
source("global.R")

ui <- fluidPage(
  cross_sectional_plotting_ui("cross_sectional_plotting", "Cross sectional plotting")
)

server <- function(input, output) {
  time_series_data_reactive <- reactive({
    time_series_data
  })
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(cross_sectional_plotting, "cross_sectional_plotting", time_series_data_reactive, tagging_data_reactive, NULL)
}

shinyApp(ui, server)
