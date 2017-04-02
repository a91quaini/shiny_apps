
gordon_growth_model <- function(input, output, session, data, ...) {
  
  ns <- session$ns
  
  # select stock
  output$stock_selector <- renderUI({
    
    stocks_vec <- data() %>%
      dplyr::select(name) %>%
      dplyr::distinct() %>% .[[1]]
    
    selectInput(
      ns('stock_selected'), 
      label = gettext('Select stock'),
      choices = stocks_vec
    )
  })
  
  # select date
  output$date_selector <- renderUI({
    
    date_vec <- data() %>%
      dplyr::select(date) %>%
      dplyr::distinct() %>% .[[1]]
    
    sliderInput(
      ns('date_selected'),
      label = gettext('Select date'),
      min = min(date_vec),
      max = max(date_vec),
      value = max(date_vec)
    )
  })
  
  # reactive vector with P, g, m, D selected
  data_selected <- reactive({
    
    req(input$stock_selected, input$date_selected) 
    
    date_selected <- data() %>%
      dplyr::select(date) %>%
      dplyr::filter(date <= input$date_selected) %>%
      .[[1]] %>% max()
    
    data_selected <- data() %>%
      dplyr::filter(name %in% input$stock_selected,
                    date %in% date_selected)

    D <- dFun(data_selected, 'dividend_per_share')
    m <- dFun(data_selected, 'expected_return')
    g <- dFun(data_selected, 'dividend_growth_rate')
    P <- dFun(data_selected, 'price')
    
    data_selected <- c(D, m, g, P)
    
  })
  
  # text P
  output$text_p <- renderText({
    req(input$stock_selected, input$date_selected)
    
    P <- data_selected()[4] %>%
      round(digits = 2)
    
    paste("P = ", P)
  })
  
  # text g
  output$text_g <- renderText({
    req(input$stock_selected, input$date_selected)
    
    g <- data_selected()[3] %>%
      round(digits = 2)
    
    paste("g = ", g)
  })
  
  # text m
  output$text_m <- renderText({
    req(input$stock_selected, input$date_selected)
    
    m <- data_selected()[2] %>%
      round(digits = 2)
    
    paste("m = ", m)
  })
  
  # text P, g, m together
  # output$text <- renderText({
  #   req(input$stock_selected, input$date_selected)
  #   
  #   P <- data_selected()[4] %>% round(digits = 2)
  #   g <- data_selected()[3] %>% round(digits = 2)
  #   m <- data_selected()[2] %>% round(digits = 2)
  #   if (is.na(sum(data_selected()))) P <- g <- m <- "missing"
  #   
  #   str1 <- paste("Price: ", P)
  #   str2 <- paste("Dividend growth rate: ", g)
  #   str3 <- paste("Expected return:", m)
  # 
  #   HTML(paste(str1, str2, str3, sep = "<br/>"))
  # 
  # })

  # wide dataframe for plot
  data_plot <- reactive({
    req(input$stock_selected, input$date_selected)
      
      data_plot <- data_selected()
      
      g_range <- seq(0, .75 * data_plot[2], by = .005)
      price <- rep(data_plot[4], length(g_range))
      implied_price <- gordonGrowth(D = data_plot[1], g = g_range, m = data_plot[2])
      
      data_plot <- tibble::tibble(
        growth = g_range,
        price = price,
        implied_price = implied_price
      ) %>% 
        reshape2::melt(id.vars = c("growth"))
      
  })
  
  # plot
  output$plot <- renderPlot({
      ggPlot(data_plot())
  })

  
  # equation
  output$equation <- renderUI({
    withMathJax(
      helpText("$$ P = \\frac{D \\ (1 + g)}{m - g} $$")
    )
  })
  
}
