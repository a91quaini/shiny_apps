library(shiny)
library(DT)
library(tidyverse)
source("global.R")

ui <- fluidPage(
  factor_model_specifiation_multiple_series_ui("factor_model_specification", "Factor model specification")
)

server <- function(input, output) {
  time_series_data_reactive <- reactive({
    time_series_data
  })
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(factor_model_specifiation_multiple_series, "factor_model_specification", time_series_data_reactive, tagging_data_reactive, NULL)
}

shinyApp(ui, server)
