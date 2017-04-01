
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
  
  data_plot <- reactive({
    req(input$stock_selected, input$date_selected)
    
    data_plot <- data() %>%
      dplyr::filter(name %in% input$stock_selected,
        date %in% input$date_selected) %>%
      dplyr::select(data_type, value)
    
    dps <- dFun(data_plot, 'dividend_per_share')
    exp_ret <- dFun(data_plot, 'expected_return')
    dgr <- dFun(data_plot, 'dividend_growth_rate')
    g_range <- seq(0, .75 * exp_ret, by = .005)
    price <- dFun(data_plot, 'price')
    price <- rep(price, length(g_range))
    impPrice <- gordonGrowth(dps, g_range, exp_ret)

    data_plot <- tibble::tibble(
      growth = g_range,
      price = price,
      implied_price = impPrice
    ) %>% 
      reshape2::melt(id.vars = c("growth"))
    
    data_plot
    
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
