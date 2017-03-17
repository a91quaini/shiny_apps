library(shiny)
library(DT)
library(tidyverse)
source("global.R")

ui <- fluidPage(
  time_series_plotting_ui("time_series_plotting", "Time series plotting")
)
  
server <- function(input, output) {
  time_series_data_reactive <- reactive({
    time_series_data
  })
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(time_series_plotting, "time_series_plotting", time_series_data_reactive, tagging_data_reactive, NULL)
}

shinyApp(ui, server)