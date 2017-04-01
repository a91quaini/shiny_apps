gordon_growth_model_ui <- function(id, module_title) {
  
  ns <- NS(id)
  
  tagList(
    
    withMathJax(),
    
    fluidRow(
      
      column(4, 
        uiOutput(ns('stock_selector')),
        uiOutput(ns('date_selector'))
      ),
      
      column(6, 
        plotOutput(ns('plot')),
        uiOutput(ns("text"))
      ),
      
      column(2,
        h5(tags$b('Gordon formula')),
        uiOutput(ns('equation'))
      )
      
    )
    
    
  )  
  
}
  
