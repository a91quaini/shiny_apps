#' @title Time series plotting module (server)
#' @description Plot time series of potentially multiple categories from tidy \code{data.frame}
#' @param time_series_data tidy \code{data.frame} that is the output of \code{\link{data_presentation_and_selection}}. The standard tidy dataset has to contain a \code{selection} column for gouping of variables / portoflios in the plots.
#' @param tagging_data \code{data.frame} that contains information about the grouping of data for aggregate/cross-sectional plotting. This table is formed at the financial instrument level. It has two columns: \code{id} and \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa. 
#' @param value_type_line_type is a device for determining how to plot which element. \code{data.frame} with columns \code{value_type} and \code{line_type}. We have to determine a mechanism to have it work with the chosen plotting platform.
#' @return The function passes on the \code{time_series_data} structure.
#' @details The plotting should use either the standard device, or \code{ggplot2} or \code{plotly}. The chosen technolgy should allow for picking series from picture on click. Get inspired by \url{https://plot.ly/r/shiny-coupled-events/} and \url{https://shiny.rstudio.com/articles/plot-interaction.html}.
#' @export

time_series_data <- readr::read_csv("time_series_data.csv")
tagging_data <- readr::read_csv("tagging_data.csv")

time_series_plotting <- function(input, output, session, time_series_data, tagging_data, value_type_line_type){
  
  output$list_of_tags <- DT::renderDataTable({
    DT::datatable( time_series_data() )
  })
  
  output$list_of_series <- DT::renderDataTable({
    # make sure we have a selection to use
    req(input$list_of_tags_rows_selected)
    # return a data table oject to render
    DT::datatable(time_series_data()[input$list_of_tags_rows_selected,])
  })
  
  output$ts_plot <- plotly::renderplotly({
    plotly::plot_ly(
      
    )
  })
  
}