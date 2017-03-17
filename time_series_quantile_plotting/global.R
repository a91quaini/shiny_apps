time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")

#' @title Time series quantile plotting module (UI)
#'   
#' @description Based on a tidy \code{data.frame} of time series data, the 
#'   module generates a fan plot for the chosen grouping aggregates within a 
#'   bigger supplied dataset.
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
#'   - the plotting area. Time series fan plots. \code{plotOutput("ts_plot")} or
#'   \code{plotly::plotlyOutput("ts_plot")}. \cr \cr
#'   
#'   There are the following dynamic UI elements, generated in 
#'   \code{\link{time_series_quantile_plotting}}: \cr
#'   
#'   - A \code{selectInput} (multiple) box with \code{tags} which should be 
#'   plotted. The choice of a single tag plots cross-sectional aggregates of all
#'   series that share the tag the tag. Populated from \code{tagging_data}. \cr
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
#'   \cr \cr
#'   
#' @seealso \code{\link{time_series_plotting}}
#'   
#' @export

time_series_quantile_plotting_ui <- function(id, module_header_text){
  ns <- NS(id)
  
  tagList(
    # plot area
    fluidRow(
      tags$h1(gettext(module_header_text)),
      
      sidebarLayout(
        sidebarPanel(
          # selectizeInput tag
          selectizeInput(ns("tags"), choices = NULL,
                         label = gettext("Choose tags"), multiple = TRUE),
          
          # selectizeInput value type 1
          selectizeInput(ns("value_type_1"), choices = NULL,
                         label = gettext("Choose value type 1")),
          
          # selectizeInput value type 2
          selectizeInput(ns("value_type_2"), choices = NULL,
                         label = gettext("Choose value type 2")),
          
          # selectizeInput frequency
          selectizeInput(ns("frequency"), choices = NULL,
                         label = gettext("Choose frequency")),
          
          # select date range
          dateRangeInput(ns("date_range"), gettext("Choose date range"))
        ),
        
        mainPanel(
          # plot
          plotOutput(ns("ts_plot"), click = ns("plot_click"), height = "650px")
          ,
          # textOutput of selected series
          uiOutput(ns("help_clicked_series"))
          ,
          # actionbutton show table
          verbatimTextOutput(ns("clicked_series"))
        )
      )
    )
  )
}



#' @title Time series quantile plotting module (server)
#'   
#' @description Based on a tidy \code{data.frame} of time series data, the 
#'   module plots the series and allows for selecting series / groups of series 
#'   to be plotted.
#'   
#' @param time_series_data tidy \code{data.frame} that is the output of 
#'   \code{\link{data_presentation_and_selection}}.
#'   
#' @param tagging_data \code{data.frame} that contains information about the 
#'   grouping of data for aggregate/cross-sectional plotting. This table is 
#'   formed at the financial instrument level. It has two columns: \code{id} and
#'   \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa.
#'   
#' @param fan_plot_specifiation is a \code{list} (named) of functions to be used
#'   for calculating summary statisics. The functions should be given as quoted 
#'   expressions, including the package name, such as \code{list(aggr1 = 
#'   "stats::median(value,na.rm=TRUE)", aggr2 =
#'   "stats::mad(value,na.rm=TRUE)")}. This is used inside the dots argument for
#'   standard evaluation use case of \code{\link{dplyr::summarise_}}.
#'   
#' @param ... other arguments that are supposed to be passed unchanged to the 
#'   modules that follow this one. The ellipsis is evaluated and written to the 
#'   returned named list.
#'   
#' @return The function passes on the \code{time_series_data} structure.
#'   
#' @details The plotting should use either the standard device, or 
#'   \code{ggplot2} or \code{plotly}. \cr \cr
#'   
#'   The plotting concentrates on \code{selections} or \code{tags}: for a fan 
#'   plot, one needs a distribution of elements at each date. Fan plots are 
#'   generated \code{tag}-wise, and multiple tags can be selected at the same 
#'   time for plotting. \cr\cr
#'   
#'   We can add a toggle for plotting the under-layer of observations at each 
#'   time point as points.
#'   
#' @export

#### ho aggiunto un value_type selection (che dipende dal primo value type selection)
#### da aggiungere possibilità di selezionare nulla
#### ho aggiornato frequency selection
#### plot ancora da rimaneggiare
#### controlla disposizioni di Andras

time_series_quantile_plotting <- function(input
                                          , output
                                          , session
                                          , time_series_data
                                          , tagging_data
                                          , fan_plot_specification = reactive(list(mean = "base::mean(value)"
                                                                                   , median = "stats::median(value)"
                                                                                   , min = "base::min(value)"
                                                                                   , max = "base::max(value)"
                                                                                   , quartile_1 = "stats::quantile(value,0.25)" 
                                                                                   , quantile_3 = "stats::quantile(value,0.75)"))
                                          , ...)
{
  
  ns <- session$ns
  
  #### plot
  ##############
  
  # vector to choose values for 1 from
  value_type_vector <- reactive({
    validate(need(time_series_data(), gettext("Time series data")))
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
  
  # previous frequency selection
  # observeEvent(input$value_type_1, {
  #   frequency_vector <- time_series_data() %>%
  #     dplyr::filter(value_type %in% input$value_type_1) %>%
  #     dplyr::group_by(frequency) %>%
  #     dplyr::summarise(num = n())
  #   
  #   updateSelectizeInput(session, "frequency",
  #                        choices = frequency_vector$frequency,
  #                        selected = frequency_vector %>% arrange(num) %>% .$frequency %>% .[1],
  #                        server = TRUE)
  # })
  
  # reactive (to all previous choices) dataframe for plotting
  plot_dataframe <- reactive({
    validate(need(input$tags, gettext("Tags")))
    validate(need(input$value_type_1, gettext("Value type")))
    validate(need(input$date_range, gettext("Date range")))
    validate(need(input$frequency, gettext("Frequency")))
    
    
    # ids corresponding to chosen tags
    id_vector <- tagging_data() %>%
      dplyr::filter(tag %in% input$tags) %>%
      dplyr::select(id) %>% .[[1]]
    
    outdata <- time_series_data() %>%
      dplyr::filter(id %in% id_vector,
                    value_type %in% input$value_type_1,
                    frequency %in% input$frequency,
                    dplyr::between(date, min(input$date_range), max(input$date_range)
                    )
      ) %>%
      dplyr::inner_join(tagging_data() %>% dplyr::filter(tag %in% input$tags)) %>%
      dplyr::group_by(date,tag,value_type) %>%
      dplyr::summarise_(.dots = fan_plot_specification()) %>% 
      tidyr::gather_(key_col = "statistic", value_col = "value", gather_cols = names(fan_plot_specification())) %>%
      dplyr::ungroup()
    
    outdata
  })
  
  # plot
  output$ts_plot <- renderPlot({
    ggplot2::ggplot(data = plot_dataframe()
                    , mapping = ggplot2::aes(x = date
                                             , y = value
                                             , col = value_type
                                             , linetype = statistic)
    ) +
      ggplot2::facet_grid(tag ~ .) +
      ggplot2::geom_line()
  })
  
  #### data table
  ###############
  
  # reactive (to series selected from plot) vector with one column of names
  name_vector <- reactive({
    
    req(input$plot_click)
    
    nearPoints(plot_dataframe()
               , input$plot_click
               , threshold = 10
               , maxpoints = 1
               , addDist = TRUE) %>%
      select(statistic) %>% .[[1]]
  })
  
  # textOutput selected series + help text
  output$help_clicked_series <- renderUI({
    
    req(name_vector())
    helpText(gettext("clicked line"))
    
  })
  
  # textOutput clicked series
  output$clicked_series <- renderText({
    
    base::paste0(name_vector())
    
  })
  
  # ellipsis <- alphaRlib::store_ellipsis_in_named_list(...)
  # 
  # return_list <- list(time_series_data = time_series_data, tagging_data = tagging_data)
  # return_list <- c(return_list, ellipsis)
  # 
  # return(return_list)
  
}