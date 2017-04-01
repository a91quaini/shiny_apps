library(shiny)
library(DT)
library(tidyverse)
source("global.R")

ui <- fluidPage(
  predictive_regression_specification_ui("predictive_regression_specification", "Predictive regression specification")
)

server <- function(input, output) {
  time_series_data_reactive <- reactive({
    time_series_data
  })
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(predictive_regression_specification, "predictive_regression_specification", time_series_data_reactive, tagging_data_reactive, NULL)
}

shinyApp(ui, server)
