
gordon_growth_model <- function(input, output, session, data, ...) {
  
  ns <- session$ns
  
  output$stock_selector <- renderUI({
    
    stocks_vec <- data() %>%
      dplyr::select(name) %>%
      dplyr::distinct() %>% .[[1]]
    
    selectInput(
      ns('stock_selected'), 
      label = 'Select stock:',
      choices = stocks_vec
    )
  })
  
  output$date_selector <- renderUI({
    
    date_vec <- data() %>%
      dplyr::select(date) %>%
      dplyr::distinct() %>% .[[1]]
    
    sliderInput(
      ns('date_selected'),
      label = 'Select date:',
      min = min(date_vec),
      max = max(date_vec),
      value = max(date_vec)
    )
  })
  
  data_selected <- reactive({
    
    req(input$stock_selected, input$date_selected)
    
    data_selected <- data() %>%
      dplyr::filter(name %in% input$stock_selected,
        date %in% input$date_selected) %>%
      dplyr::select(data_type, value)
    
    D <- dFun(data_selected, 'dividend_per_share')
    m <- dFun(data_selected, 'expected_return')
    g <- dFun(data_selected, 'dividend_growth_rate')
    P <- dFun(data_selected, 'price')
    
    data_selected <- c(D, m, g, P)
    
  })
  
  
  output$text <- renderText({
    req(input$stock_selected, input$date_selected)

    str1 <- paste("Price: ", data_selected()[4])
    str2 <- paste("Dividend growth rate: ", data_selected()[3])
    str3 <- paste("Expected return:", data_selected()[2])

    HTML(paste(str1, str2, str3, sep = "<br/>"))

  })

  
  data_plot <- reactive({
    req(input$stock_selected, input$date_selected)
    
    data_plot <- data_selected()
    
    g_range <- seq(0, .75 * data_plot[2], by = .005)
    price <- rep(data_plot[4], length(g_range))
    gordon_price <- gordonGrowth(D = data_plot[1], g = g_range, m = data_plot[2])

    data_plot <- tibble::tibble(
      growth = g_range,
      price = price,
      gordon_price = gordon_price
    ) %>% 
      reshape2::melt(id.vars = c("growth"))
    
  })
  

  output$plot <- renderPlot({

    ggPlot(data_plot())

  })

  
  
  output$equation <- renderUI({
    withMathJax(
      helpText("$$ P = \\frac{D \\ (1 + g)}{m - g} $$")
    )
  })
  
}
