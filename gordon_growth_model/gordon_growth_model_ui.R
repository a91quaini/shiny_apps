gordon_growth_model_ui <- function(id, module_title) {
  
  ns <- NS(id)
  
  tagList(
    
    withMathJax(),
    
    fluidRow(
      
      column(3, 
        uiOutput(ns('stock_selector')),
        uiOutput(ns('date_selector')),
        tags$br(),
        # htmlOutput(ns("text")),
        verbatimTextOutput(ns("text_p")),
        verbatimTextOutput(ns("text_g")),
        verbatimTextOutput(ns("text_m"))
        
      ),
      
      column(7, 
        plotOutput(ns('plot'))
      ),
      
      column(2,
        h5(tags$b('Gordon formula')),
        uiOutput(ns('equation'))
      )
      
    )
    
    
  )  
  
}
  
