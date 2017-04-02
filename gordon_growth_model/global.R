library(shiny)
source('gordon_growth_model_ui.R')
source('gordon_growth_model.R')
load("gordon_data.Rdata")

gordonGrowth <- function(D, g, m) {
  out <- D * (1 + g) / (m - g)
  return(out)
}

dFun <- function(data, dType) {
  if (nrow(data) == 0) {
    return(NA)
  } else {
    out <- data %>% dplyr::filter(data_type %in% dType) %>%
      dplyr::select(value) %>% .[[1]] 
  }
}

ggPlot <- function(data) {
  
  ggplot2::ggplot(data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = growth, y = value, col = variable))
}
