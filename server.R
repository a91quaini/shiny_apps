library(shiny)
library(DT)
library(plotly)
library(readr)
time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")

shinyServer(function(input, output) {
  
  output$list_of_tags <- DT::renderDataTable({
    DT::datatable( time_series_data )
  })
  
  output$list_of_series <- DT::renderDataTable({
    # make sure we have a selection to use
    req(input$list_of_tags_rows_selected)
    # return a data table oject to render
    DT::datatable(time_series_data[input$list_of_tags_rows_selected,])
  })
  
})
