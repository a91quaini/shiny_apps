
time_series_plotting_ui <- function(id, module_header_text){
  ns <- NS(id)
  
  tagList(
    
    # data tables area
    fluidRow(
      tags$h1(module_header_text),
      
      sidebarLayout(
        sidebarPanel(
          # selectizeInput tag
          selectizeInput(ns("list_of_tags"), choices = NULL, 
                         label = "Choose tags", multiple = TRUE)
        ),
        
        mainPanel(
          # dataTable of series
          DT::dataTableOutput(ns("list_of_series"))
        )
      )
    ),
    
    # plot area
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          # selectizeInput value type
          uiOutput(ns("value_type_selector")),
          
          tags$br(),
          
          # selectizeInput frequency
          uiOutput(ns("frequency_selector")),
          
          tags$br(),
          
          uiOutput(ns("date_range_selector")),
          
          # checkBox highlight
          uiOutput(ns("highlight")),
          
          tags$br(),
          
          # textOutput of selected series
          uiOutput(ns("help_clicked_series")),
          verbatimTextOutput(ns("clicked_series"))
        ),
        
        mainPanel(
          # plot
          plotly::plotlyOutput(ns("ts_plot"))
        )
      )
      
      
    )
  )
  
}


time_series_plotting <- function(input, output, session, time_series_data, tagging_data){ # , value_type_line_type){
  
  ns <- session$ns
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
    validate(need(input$list_of_tags != '', label = "Tags"))
    
    # dataframe with one column of chosen tags
    tag_selected_distinct_dataframe <- tag_distinct_dataframe() %>% 
      filter(tag %in% input$list_of_tags)
    
    tagging_data() %>% 
      filter(tag %in% tag_selected_distinct_dataframe[[1]]) %>%
      select(id) %>%
      distinct()
  })
  
  # reactive (to id_selected) dataframe with columns containing distinct ids (corresponding to chosen tags), names and currencies
  list_of_series_dataframe <- reactive({
    
    time_series_data() %>% 
      filter(id %in% id_distinct_dataframe()[[1]]) %>% 
      select(id, name, currency) %>% # hash key, id_type and other columns to be added
      distinct()
  })
  
  # render time_series_data_selected in dataTable
  output$list_of_series <- DT::renderDataTable({
    # make sure we have a selection to use
    validate(need(input$list_of_tags != '', label = "Tags"))
    # return a data table oject to render
    DT::datatable( list_of_series_dataframe() )
  })
  
  # reactive (to id selected from data table) dataframe with one column of distinct selected ids
  id_selected_distinct_dataframe <- reactive({
    validate(need(input$list_of_series_rows_selected, label = "Series"))
    
    id_distinct_dataframe() %>%
      filter(id %in% (id_distinct_dataframe() %>%
                        .[input$list_of_series_rows_selected, ] %>%
                        .[[1]]
                      )
      )
  })
  
  #### plot
  #########
  
  # reactive (to id_selected) dataframe with two columns of value_types and frequencies
  value_type_and_frequency_dataframe <- reactive({
    time_series_data() %>%
      filter(id %in% id_selected_distinct_dataframe()[[1]]) %>%
      select(value_type, frequency)
  })
  
  # dynamic UI selectInput value_type
  output$value_type_selector <- renderUI({
    # make sure we have a selection to use
    validate(need(input$list_of_tags, label = "Tags"))
    validate(need(input$list_of_series_rows_selected, label = "Series"))
    
    value_type_distinct_vector <- value_type_and_frequency_dataframe() %>%
      select(value_type) %>%
      distinct() %>%
      .[[1]]
    selectizeInput(ns("value_type"), label = "Choose value types",
                   choices = value_type_distinct_vector)
  })
  
  # dynamic UI selectInput frequency
  output$frequency_selector <- renderUI({
    # make sure we have a selection to use
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    frequency_distinct_vector <- value_type_and_frequency_dataframe() %>%
      select(frequency) %>% 
      distinct() %>%
      .[[1]]
    selectizeInput(ns("frequency"), label = "Choose frequency",
                   choices = frequency_distinct_vector)
  })
  
  # dynamic UI data range
  output$date_range_selector <- renderUI({
    req(input$list_of_tags, input$list_of_series_rows_selected)

    date_distinct_vector <- time_series_data() %>%
      select(date) %>%
      distinct() %>%
      .[[1]]
    min_date <- min(date_distinct_vector)
    max_date <- max(date_distinct_vector)
    dateRangeInput(ns("date_range"), "Choose date range",
                start = min_date, end = max_date,
                min = min_date, max = max_date
                )
  })
  
  # textOutput of selected series + help text
  output$help_clicked_series <- renderUI({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    helpText("Selected series")
  })
  
  output$clicked_series <- renderText({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    name_chr_vector <- list_of_series_dataframe() %>%
      filter(id %in% id_selected_distinct_dataframe()[[1]]) %>%
      select(name) %>%
      .[[1]]
    paste0(name_chr_vector)
  })
  
  # checkBox highlight
  output$highlight <- renderUI({
    req(input$list_of_tags, input$list_of_series_rows_selected)
    
    checkboxInput("highlight", "Highlight series")
  })
  
  # reactive (to all previous choices) dataframe for plotting
  plot_dataframe <- reactive ({
    
    
    time_series_data() %>%
      filter(id %in% id_selected_distinct_dataframe()[[1]],
             value_type %in% input$value_type,
             frequency %in% input$frequency,
             date %in% seq.Date(from = input$date_range[1],
                                to = input$date_range[2],
                                by = "day"))
  })
  
  # plot
  output$ts_plot <- plotly::renderPlotly({
    req(input$list_of_tags, input$list_of_series_rows_selected)

    p <- ggplot(data = plot_dataframe()) +
      geom_line(mapping = aes(x = date, y = value, color = name))
    
    ggplotly(p)
  })
  
}