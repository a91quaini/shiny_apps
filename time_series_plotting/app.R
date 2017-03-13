library(shiny)
library(DT)
library(tidyverse)
library(plotly)
time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")
source("global.R")

ui <- fluidPage(
  time_series_plotting_ui("plot_this", "Time series plotting")
)
  
server <- function(input, output) {
  time_series_data_reactive <- reactive({
    time_series_data
  })
  
  tagging_data_reactive <- reactive({
    tagging_data
  })
  
  callModule(time_series_plotting, "plot_this", time_series_data_reactive, tagging_data_reactive, NULL)
}

shinyApp(ui, server)