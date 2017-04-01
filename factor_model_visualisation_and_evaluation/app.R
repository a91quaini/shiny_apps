library(shiny)
library(DT)
library(tidyverse)
source("global.R")

ui <- fluidPage(
  factor_model_visualisation_and_evaluation_ui("factor_model_vis_eval", "Factor model visualisation and evaluation")
)

server <- function(input, output) {
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(factor_model_visualisation_and_evaluation, "factor_model_vis_eval", NULL, tagging_data_reactive)
}

shinyApp(ui, server)