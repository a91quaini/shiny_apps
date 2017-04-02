source("global.R")

ui <- fluidPage(
  gordon_growth_model_ui("gordon_growth_model", module_title = gettext("Gordon growth model"))
)

server <- function(input, output) {
  gordon_data_reactive <- reactive({
    gordon_data
  })

  callModule(gordon_growth_model, "gordon_growth_model", data = gordon_data_reactive)
}

shinyApp(ui, server)
