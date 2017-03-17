time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")

#' @title Time series plotting module (UI)
#'   
#' @description Based on a tidy \code{data.frame} of time series data, the 
#'   module plots the series and allows for selecting series / groups of series 
#'   to be plotted.
#'   
#' @param module_header_text \code{character}, defines some local module title.
#'   
#' @details This module shows the user plots of data obtained from 
#'   \code{\link{data_presentation_and_selection}}.
#'   
#'   \cr \cr There are the following static UI elements: \cr
#'   
#'   - \code{textOutput("site_header")} renders the title of the page \cr
#'   
#'   - \code{DT::dataTableOutput("list_of_series")} where from the user picks 
#'   individual series to be plotted. This table is hidden by default (tab?). 
#'   We're not yet sure this is going to be necessary. \cr
#'   
#'   - the plotting area. Time series plots. \code{plotOutput("ts_plot")} or 
#'   \code{plotly::plotlyOutput("ts_plot")}. Or dygraphs 
#'   (\url{https://rstudio.github.io/dygraphs}) \cr
#'   
#'   - \code{textOutput("clicked_series")}: name of series that was selected by 
#'   in in-plot click \cr
#'   
#'   There are the following dynamic UI elements, generated in 
#'   \code{\link{time_series_plotting}}: \cr
#'   
#'   - A \code{selectInput} (multiple) box with \code{tags} which should be 
#'   plotted. Time series sharing the tags selected in this table will be 
#'   plotted. The choice of a single tag plots multiple time series that share 
#'   the tag plus the aggregate series for the tag that was created in 
#'   \code{\link{data_presentation_and_selection}}. The aggregate series is 
#'   marked in a plot in some way to make it stand out from the others: thicker 
#'   line, more intense colour (use \code{alpha} in \code{ggplot}, for example).
#'   Populated from \code{tagging_data}. \cr
#'   
#'   - A \code{selectInput} (single) box with \code{value_types} which should be
#'   plotted, which are populated from the \code{value_type} column of 
#'   \code{time_series_data} in the server function 
#'   \code{\link{time_series_plotting}}.
#'   
#'   - A \code{selectInput} (single) box with \code{value_types} which should be
#'   plotted in the same plot, but on a second y axis. This is populated as the 
#'   one above.
#'   
#'   - A \code{checkBox} highlight option to highlight aggregated series in 
#'   plots, and grey out the individual components. \cr \cr
#'   
#' @seealso \code{\link{time_series_plotting}}
#' @export

time_series_plotting_ui <- function(id, module_header_text="default text"){
  ns <- NS(id)
  
  tagList(
    
    # plot area
    fluidRow(
      tags$h1(module_header_text),
      
      sidebarLayout(
        sidebarPanel(
          # selectizeInput tag
          selectizeInput(ns("tags"), choices = NULL,
                         label = "Choose tags", multiple = TRUE),
          
          # checkBox highlight
          uiOutput(ns("highlight")),
          
          # selectizeInput value type
          selectizeInput(ns("value_type_1"), choices = NULL,
                         label = "Choose value type"),
          
          # selectizeInput value type
          selectizeInput(ns("value_type_2"), choices = NULL,
                         label = "Choose value type"),
          
          # selectizeInput frequency
          selectizeInput(ns("frequency"), choices = NULL,
                         label = "Choose frequency"),
          
          # select date range
          dateRangeInput(ns("date_range"), "Choose date range")
        ),
        
        mainPanel(
          # plot
          plotOutput(ns("ts_plot"), click = ns("plot_click"))
        )
      )
    ),
    
    # data table area
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          # textOutput of selected series
          uiOutput(ns("help_clicked_series")),
          verbatimTextOutput(ns("clicked_series")),
          
          # actionbutton show table
          uiOutput(ns("show_table"))
        ),
        
        mainPanel(
          # dataTable of series
          DT::dataTableOutput(ns("series_table"))
        )
      )
      
      
    )
  )
  
}

#' @title Time series plotting module (server)
#'
#' @description Plot time series of potentially multiple categories from tidy 
#'   \code{data.frame}
#'
#' @param time_series_data tidy \code{data.frame} that is the output of 
#'   \code{\link{data_presentation_and_selection}}.
#'
#' @param tagging_data \code{data.frame} that contains information about the 
#'   grouping of data for aggregate/cross-sectional plotting. This table is 
#'   formed at the financial instrument level. It has two columns: \code{id} and
#'   \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa.
#'
#' @param value_type_line_type is a device for determining how to plot which 
#'   element. \code{data.frame} with columns \code{value_type} and 
#'   \code{line_type}. We have to determine a mechanism to have it work with the
#'   chosen plotting platform.
#'
#' @param ... other arguments that are supposed to be passed unchanged to the
#'   modules that follow this one. The ellipsis is evaluated and written to the
#'   returned named list.
#'
#' @return The function passes on the \code{time_series_data} structure, and the
#'   \code{tagging_data} structure. If the option to
#'   \code{filter_passed_datasets} is chosen, both the aforementioned structures
#'   are restricted to the user choices. Otherwise, the entire datasets are
#'   passed on.
#'
#' @details The plotting should use either the standard device, or 
#'   \code{ggplot2} or \code{plotly}. The chosen technolgy should allow for 
#'   picking series from picture on click. Get inspired by 
#'   \url{https://plot.ly/r/shiny-coupled-events/} and 
#'   \url{https://shiny.rstudio.com/articles/plot-interaction.html}.
#'
#' @export

#### ho aggiunto un value_type selection (che dipende dal primo value type selection)
#### da aggiungere possibilità di selezionare nulla
#### ho aggiornato frequency selection
#### plot ancora da rimaneggiare

time_series_plotting <- function(input
                                 , output
                                 , session
                                 , time_series_data
                                 , tagging_data
                                 , value_type_line_type
                                 , filter_passed_datasets = reactive(FALSE)
                                 , ...) {
  
  ns <- session$ns
  
  #### plot
  ##############
  
  value_type_vector <- reactive({
    validate(need(time_series_data(), "Time series data"))
    time_series_data() %>%
      dplyr::select(value_type) %>% dplyr::distinct() %>% .[[1]]
  })
  
  # selectizeInput tags
  observeEvent({
    tagging_data()
    time_series_data()
  }, {
    tag_vector <- tagging_data() %>%
      dplyr::select(tag) %>%
      dplyr::distinct() %>% .[[1]]
    
    updateSelectizeInput(session, "tags",
                         choices = tag_vector,
                         server = TRUE)
    
    updateSelectizeInput(session, "value_type_1",
                         choices = value_type_vector(),
                         selected = value_type_vector()[1],
                         server = TRUE)
    
    date_vector <- time_series_data() %>%
      dplyr::select(date) %>%
      dplyr::distinct() %>% .[[1]]
    min_date <- min(date_vector)
    max_date <- max(date_vector)
    
    updateDateRangeInput(session, "date_range",
                         start = min_date, end = max_date,
                         min = min_date, max = max_date)
  })
  
  # select value type 2 (from all but value type 1)
  value_type_2_vector <- reactive({
    validate(need(input$value_type_1, "Value type 1"))
    value_type_vector()[value_type_vector() != input$value_type_1]
  })
  
  # if there is no selection, set NULL. If there is a selection, keep the selection except if it already 
  # present in value type 1
  value_type_2_selected <- reactive({
    req(value_type_2_vector)
    if(isTruthy(input$value_type_2) & (input$value_type_2 %in% value_type_2_vector())) {
      input$value_type_2
    } else if (isTruthy(input$value_type_2) & !(input$value_type_2 %in% value_type_2_vector())) {
      value_type_2_vector()[1]
    } else { NULL }
  })
  
  # selectize value type 2
  observeEvent(input$value_type_1, {
    updateSelectizeInput(session, "value_type_2",
                         choices = value_type_2_vector(),
                         selected = value_type_2_selected(),
                         server = TRUE)
  })
  
  # frequency vector (depends on value types choice(s))
  frequency_vector <- reactive({
    req(input$value_type_1)
    
    time_series_data() %>%
      dplyr::filter(value_type %in% c(input$value_type_1, input$value_type_2)) %>%
      tidyr::spread(key = value_type, value = value) %>% tidyr::drop_na() %>%
      dplyr::select(frequency) %>% dplyr::distinct() %>% .[[1]]
  })
  
  # frequency selected. If there is a selection, keep it except if it is not valid. If there
  # is no selection, take the first element of frequency_vector
  frequency_selected <- reactive({
    req(frequency_vector())
    if(isTruthy(input$frequency) & (input$frequency %in% frequency_vector())) {
      input$frequency
    } else {
      frequency_vector()[1]
    }
  })
  
  # update frequency choice
  observeEvent({
    input$value_type_1
    input$value_type_2
  }, {
    req(frequency_vector())
    updateSelectizeInput(session, "frequency",
                         choices = frequency_vector(),
                         selected = frequency_selected(),
                         server = TRUE)
  })

  # checkBox highlight
  output$highlight <- renderUI({
    req(input$tags)
    
    checkboxInput(ns("highlighter"), "Highlight series")
  })
  
  # reactive (to all previous choices) dataframe for plotting
  plot_dataframe <- reactive({
    validate(need(input$tags, "Tags"))
    validate(need(input$value_type_1, "Value type"))
    validate(need(input$date_range, "Date range"))
    validate(need(input$frequency, "Frequency"))
    
    
    # ids corresponding to chosen tags
    id_vector <- tagging_data() %>%
      dplyr::filter(tag %in% input$tags) %>%
      dplyr::select(id) %>% .[[1]]
    
    time_series_data() %>%
      dplyr::filter(id %in% id_vector,
                    value_type %in% input$value_type_1,
                    frequency %in% input$frequency,
                    dplyr::between(date, min(input$date_range), max(input$date_range)))
  })
  
  # plot
  output$ts_plot <- renderPlot({
    ggplot2::ggplot(data = plot_dataframe()) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = value, color = name))
  })
  
  #### data table
  ###############
  
  # reactive (to series selected from plot) vector with one column of names
  name_vector <- reactive({
    req(input$plot_click)
    nearPoints(plot_dataframe(), input$plot_click,
               threshold = 10, maxpoints = 1, addDist = TRUE) %>%
      dplyr::select(name) %>% .[[1]]
  })
  
  # textOutput selected series + help text
  output$help_clicked_series <- renderUI({
    req(name_vector())
    helpText("Selected series")
  })
  
  # textOutput clicked series
  output$clicked_series <- renderText({
    base::paste0(name_vector())
  })
  
  # actionButton show table
  output$show_table <- renderUI({
    req(name_vector())
    
    checkboxInput(ns("table_button"), "Show data table", value = TRUE)
  })
  
  # data table dataframe
  table_dataframe <- reactive({
    req(name_vector())
    validate(need(input$table_button, "Table button"))
    
    plot_dataframe() %>%
      dplyr::filter(name %in% name_vector()) %>%
      dplyr::select(id, name, currency) %>%
      dplyr::distinct()
  })
  
  # data table
  output$series_table <- DT::renderDataTable({
    DT::datatable( table_dataframe() )
  })
  
  # ellipsis <- alphaRlib::store_ellipsis_in_named_list(...)
  # 
  # return_list <- list(time_series_data = time_series_data, tagging_data = tagging_data)
  # return_list <- c(return_list, ellipsis)
  # return(return_list)
  
}