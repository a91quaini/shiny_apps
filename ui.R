shinyUI(
  fluidPage(
    tags$h1("Module header title"),
    actionButton("validate_and_proceed", "Validate and proceed"),
    
    # data tables area
    fluidRow(
      DT::dataTableOutput("list_of_tags")
    ),
    fluidRow(
      DT::dataTableOutput("list_of_series")
    ),
    # plot area
    fluidRow(
      plotly::plotlyOutput("ts_plot")
    ),
    fluidRow(textOutput("clicked_series"))
  )
  
)
