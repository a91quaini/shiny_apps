#### UI
#' @title Cross-sectional plotting (UI)
#' @description Plot the cross-section of a given parameter in a panel of data. Compare over time.
#' @param module_header_text \code{character}, defines some local module title.
#' @details The module allows the user to look at snapshots of cross-sectional values of variables across time. \cr \cr
#' There are the following static UI elements: \cr
#' - \code{textOutput("site_header")} renders the title of the page \cr
#' - \code{actionButton("validate_and_proceed")} finishes work on the step and forces a move down the workflow \cr
#' - the plotting area. Time series plots.  \cr
#' - \code{plotOutput("cs_plot")} or \code{plotly::plotlyOutput("cs_plot")}. Multiple plots with link-brushing 
#' (see documentation of \code{brush} argument in \code{\link[shiny]{plotOutput}}).\cr
#' - \code{textOutput("brushed_series")} names of series that are covered by the plot brushing \cr
#' \cr
#' There are the following dynamic UI elements, generated in \code{\link{cross_sectional_plotting}}: \cr
#' - A \code{selectInput} (single) box with \code{value_types} which should be put on the x axis, 
#' which are populated from the \code{value_type} column of \code{time_series_data} in the server function 
#' \code{\link{cross_sectional_plotting}}. \cr
#' - A \code{selectInput} (multiple) box with \code{value_types} which should be put on the y axis, populated as above. \cr
#' - A \code{selectInput} (multiple) box with \code{tags} so that various subsets of the data can be highlighted. 
#' Populated with distinct tags from \code{tagging_data}\cr
#' - A \code{selectInput} (multiple) for dates at which plots will be prepared, we need to max it out at some number, say 5.
#' \cr \cr
#' An alternative to generating is using \code{updateSomeElement} functions.
#' @seealso \code{\link{cross_sectional_plotting}}
#' @export


cross_sectional_plotting_ui <- function(id, module_header_text){
  ns <- NS(id)
  
  fluidRow(
    tags$h1(module_header_text),

    sidebarLayout(
      sidebarPanel(
        # selectizeInput (single) value type for x
        selectizeInput(ns("value_type_x"), choices = NULL,
                       label = "Choose value type for x-axis"),

        # selectizeInput (multiple) value type for y
        selectizeInput(ns("value_types_y"), choices = NULL,
                     label = "Choose (multiple) value types for y-axis",
                     multiple = TRUE),

        # selectizeInput (single) frequency
        selectizeInput(ns("frequency"), choices = NULL,
                       label = "Choose frequency"),

        # selectizeInput tags
        selectizeInput(ns("tags"), choices = NULL,
                       label = "Choose tags",
                       multiple = TRUE),

        # dateInput
        # dateInput(ns("date_input_1"), "Choose date plot 1"),
        # dateInput(ns("date_input_2"), "Choose date plot 2"),
        # dateInput(ns("date_input_3"), "Choose date plot 3"),
        selectizeInput(ns("dates_input"), choices = NULL,
                       label = "Choose dates for plots",
                       multiple = TRUE),

        tags$br(),

        # textOutput of selected series
        helpText("Brushed series"),
        verbatimTextOutput(ns("brushed_series"))
      ),

      mainPanel(
        # dataTable of series
        # plotOutput(ns("cs_plot"))
      )
    )
  )

}

#' @title Cross-sectional plotting (server)
#' @description Plot the cross-section of a given parameter in a panel of data. Compare over time.
#' @param time_series_data tidy \code{data.frame} that is the output of \code{\link{data_presentation_and_selection}}. 
#' The standard tidy dataset has to contain a \code{selection} column for gouping of variables / portoflios in the plots.
#' @param tagging_data \code{data.frame} that contains information about the grouping of data for aggregate/cross-sectional 
#' plotting. This table is formed at the financial instrument level. It has two columns: \code{id} and \code{tag}. 
#' You can have multiple \code{ids} per \code{tag} and vice versa. 
#' @return The function passes a list with the \code{time_series_data} structure and summary tables that were formed for plots.
#' @details The purpose of this module is to visualise the stability of rankings/relationships over time. 
#' The objective is to create graphs in this spirit: \url{http://shiny.rstudio.com/gallery/module-example.html}. \cr\cr 
#' The user selects a number of dates. The number of plot panels filled is 5 at a maximum. \cr\cr
#' For each date, a scatterplot is generated for \code{value_types} chosen by the user on the x and y axis. 
#' Multiple value types can be chosen for the y axis. \cr\cr
#' An extra \code{value_type} called \code{ranking} is added. \code{ranking} is dynamically generated and is simply 
#' the ascending ordering rank of the \code{value_type} which is the first choice of the y axis.
#' Picking the \code{selections} highlights points for entities belonging to that selection in all plots.
#' @export


cross_sectional_plotting <- function(input, output, session, time_series_data, tagging_data, value_type_line_type){
  
  # ns <- session$ns
  
  # Selectize value type for x axis
  observeEvent(time_series_data(), {
    value_types_vector <- time_series_data() %>%
        select(value_type) %>%
        distinct() %>% .[[1]]
    
    updateSelectizeInput(session, "value_type_x",
                         choices = value_types_vector,
                         selected = value_types_vector[1],
                         server = TRUE)
    updateSelectizeInput(session, "value_types_y",
                         choices = value_types_vector,
                         selected = value_types_vector[1],
                         server = TRUE)

    date_vector <- time_series_data() %>%
      select(date) %>% 
      distinct() %>% .[[1]]
    # min_date <- min(date_vector)
    # max_date <- max(date_vector)

    # updateDateInput(session, "date_input_1",
    #                value = max_date,
    #                max = max_date, min = min_date)
    # updateDateInput(session, "date_input_2",
    #                 value = max_date,
    #                 max = max_date, min = min_date)
    # updateDateInput(session, "date_input_3",
    #                 value = max_date,
    #                 max = max_date, min = min_date)
    updateSelectizeInput(session, "dates_input",
                         choices = date_vector,
                         server = TRUE)
  })
  
  observeEvent({
    input$value_type_x
    input$value_types_y
  }, {
    frequency_vector <- time_series_data() %>%
      filter(value_type %in% c(input$value_type_x, input$value_types_y)) %>%
      select(frequency) %>%
      distinct() %>% .[[1]]
    
    updateSelectizeInput(session, "frequency",
                         choices = frequency_vector,
                         selected = frequency_vector[1],
                         server = TRUE)
  })

  observeEvent(tagging_data(), {
    tag_vector <- tagging_data() %>%
      select(tag) %>%
      distinct() %>% .[[1]]

    updateSelectizeInput(session, "tags",
                         choices = tag_vector,
                         server = TRUE)
  })
  
  plot_dataframe <- reactive({
    validate(need(input$value_type_x, "Value type for x-axis"))
    validate(need(input$value_types_y, "Value types for y-axis"))
    validate(need(input$frequency, "Frequency"))
    validate(need(input$dates_input, "Date"))

    time_series_data() %>%
      filter(value_type %in% c(input$value_type_x, input$value_types_y),
             frequency %in% input$frequency,
             date %in% input$dates_input)

  })
  
  # output$cs_plot <- renderPlot({
  #   
  # })
  
}