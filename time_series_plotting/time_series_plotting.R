#' @title Time series plotting module (server)
#' @description Plot time series of potentially multiple categories from tidy \code{data.frame}
#' @param time_series_data tidy \code{data.frame} that is the output of \code{\link{data_presentation_and_selection}}. 
#' The standard tidy dataset has to contain a \code{selection} column for gouping of variables / portoflios in the plots.
#' @param tagging_data \code{data.frame} that contains information about the grouping of data for aggregate/cross-sectional plotting. 
#' This table is formed at the financial instrument level. It has two columns: \code{id} and \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa. 
#' @param value_type_line_type is a device for determining how to plot which element. \code{data.frame} with columns \code{value_type} and \code{line_type}. 
#' We have to determine a mechanism to have it work with the chosen plotting platform.
#' @return The function passes on the \code{time_series_data} structure.
#' @details The plotting should use either the standard device, or \code{ggplot2} or \code{plotly}. 
#' The chosen technolgy should allow for picking series from picture on click. 
#' Get inspired by \url{https://plot.ly/r/shiny-coupled-events/} and \url{https://shiny.rstudio.com/articles/plot-interaction.html}.
#' @export


time_series_plotting <- function(input, output, session, time_series_data, tagging_data, value_type_line_type){
  
  #### dataTable
  ##############
  
  # reactive (to tagging data) dataframe with single column of distinct tags
  tag_distinct_dataframe <- reactive({
    select(tagging_data(), tag) %>% 
      distinct()
  })
  
  # selectizeInput tags
  observeEvent(tag_distinct_dataframe(), 
               updateSelectizeInput(session, "list_of_tags",
                                    choices = tag_distinct_dataframe()[[1]], 
                                    server = TRUE)
  ) 
  
  # reactive (to tag selected) dataframe with one column of distinct ids corresponding to chosen tags
  id_distinct_dataframe <- reactive({
    # dataframe with one column of chosen tags
    tag_selected_distinct_dataframe <- tag_distinct_dataframe() %>% 
      filter(tag %in% input$list_of_tags)
    
    tagging_data() %>% 
      filter(tag %in% tag_selected_distinct_dataframe[[1]]) %>%
      select(id) %>%
      distinct()
  })
  
  # reactive (to id_selected) dataframe with columns of distinct ids (corresponding to chosen tags), names and currencies
  list_of_series_dataframe <- reactive({
    
    time_series_data() %>% 
      filter(id %in% id_distinct_dataframe()[[1]]) %>% 
      select(id, name, currency) %>% # hash key, id_type and other columns to be added
      distinct()
  })
  
  # render time_series_data_selected in dataTable
  output$list_of_series <- DT::renderDataTable({
    # make sure we have a selection to use
    req(input$list_of_tags)
    # return a data table oject to render
    DT::datatable( list_of_series_dataframe() )
  })
  
  id_selected_distinct_dataframe <- reactive({
    id_distinct_dataframe() %>%
      filter(id %in% (id_distinct_dataframe() %>%
                        .[input$list_of_series_rows_selected, ] %>%
                        .[[1]]
      )
      )
  })
  
  #### plot
  #########
  
  # reactive (to id_selected) dataframe with one column of distinct value_types
  value_type_and_frequency_distinct_dataframe <- reactive({
    time_series_data() %>%
      filter(id %in% id_selected_distinct_dataframe()[[1]]) %>%
      select(value_type, frequency)
  })
  
  # dynamic UI selectInput value_type
  output$value_type <- renderUI({
    # make sure we have a selection to use
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    value_type_distinct_vector <- value_type_and_frequency_distinct_dataframe() %>%
      select(value_type) %>%
      distinct() %>%
      .[[1]]
    selectizeInput("value_type", label = "Choose value types", multiple = TRUE,
                   choices = value_type_distinct_vector)
  })
  
  # dynamic UI selectInput frequency
  output$frequency <- renderUI({
    # make sure we have a selection to use
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    frequency_distinct_vector <- value_type_and_frequency_distinct_dataframe() %>%
      select(frequency) %>% 
      distinct() %>%
      .[[1]]
    selectizeInput("frequency", label = "Choose frequency",
                   choices = frequency_distinct_vector)
  })
  
  # textOutput of selected series + help text
  output$help_clicked_series <- renderUI({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    helpText("Selected series")
  })
  
  output$clicked_series <- renderText({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    paste0(id_selected_distinct_dataframe()[[1]])
  })
  
  # checkBox highlight
  output$highlight <- renderUI({
    req(input$list_of_tags, input$list_of_series_rows_selected,
        input$value_type, input$frequency)
    
    checkboxInput("highlight", "Highlight series")
  })
  
  # reactive (to all previous choices) dataframe for plotting
  plot_dataframe <- reactive ({
    time_series_data() %>%
      filter(id %in% id_selected_distinct_dataframe()[[1]]) %>%
      filter(value_type %in% input$value_type) %>%
      filter(frequency %in% input$frequency)
  })
  
  output$ts_plot <- renderPlot({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    ggplot(data = plot_dataframe()) + 
      geom_line(mapping = aes(x = date, y = value))
  })
  
}