#' @title Predictive regression (UI)
#' @description Choose specification of predictive regression
#' @param module_header_text \code{character}, defines some local mocule title.
#' @details This module allows the user to choose predictors in linear predictive model \cr \cr
#' There are the following static UI elements: \cr
#' - \code{textOutput("site_header")} renders the title of the page \cr
#' - \code{actionButton("validate_and_proceed")} finishes work on the step and forces a move down the workflow \cr
#' - \code{htmlOutput("model_summary")} nicely formatted equation of the model that the user is going to estimate, 
#' information about number of osbervations, frequency, everything else that one would like to know about the model. 
#' Use other than htmlOutput if you need. \cr
#' There are the following dynamic UI elements, generated in \code{\link{cross_sectional_plotting}}: \cr
#' - A \code{selectInput} (single) box with estimation methods to be used. For a start, \code{lm/lmrob} if 
#' \code{gos_2016=FALSE}, this box is not shown.
#' - A \code{selectInput} (multiple) for choice of factors. This is populated from \code{rhs_tagging_data} in the 
#' server function by taking the names of the join of \code{time_series_data} and \code{rhs_tagging_data}}.
#' \cr \cr
#' @seealso \code{\link{predictive_regression_specification}}
#' @export

predictive_regression_specification_ui <- function(id, module_header_text){
  ns <- NS(id)
  
  fluidRow(
    tags$h1(gettext(module_header_text)),
    
    sidebarLayout(
      sidebarPanel(
        
        selectizeInput(ns("estimation_methods"), choices = NULL,
                       label = gettext("Choose estimation method")),
        
        tags$br(),
        
        selectizeInput(ns("factors"), choices = NULL,
                       label = gettext("Choose factors"))
      ),
      
      mainPanel(
        htmlOutput(ns("model_summary"))
      )
    )
  )
}

#' @title Predictive regression (server)
#' @description Choose specification of predictive regression
#' @param time_series_data tidy \code{data.frame} that is the output of \code{\link{data_presentation_and_selection}}. It also contains factors.

#' @param rhs_tagging_data \code{data.frame} as above, but has information about which variables are to be used as 
#' predictors. Special tag \code{lag_x} deals with choosing lagged values of the LHS variable, \code{x} denoting 
#' how many lags are taken.
#' @param multiple_methods \code{logical}. If \code{FALSE} only the user-chosen method is used. If \code{TRUE}, 
#' OLS estimates will be returned alongside anything else.
#' @param volatility_prediction \code{logical}. If \code{TRUE}, options to impose HAR restrictions. 
#' @return The function returns a list with the following fields: \cr
#' - \code{model}, \code{lm}-type object \cr
#' - \code{modelother}, as above, for the relevant other method (e.g. \code{lmrob}, \code{quantreg::rq})
#' - \code{time_series_data}, \cr
#' - \code{rhs_tagging_data}\cr \cr
#' @details The purpose of this module is to specify a linear predictive model. The user specifies RHS variables 
#' from the list, does not specify functional form. If we allow the user to choose the method, we need to be able 
#' to handle this in the following modules. The user also chooses \code{frequency}. \cr\cr
#' For \code{volatility_prediction} refer to below:
#' @references Corsi, F. (2009). A simple approximate long-memory model of realized volatility. Journal of Financial 
#' Econometrics, 7(2), pp. 174–196. \url{https://doi.org/10.1093/jjfinec/nbp001}
#' @export

predictive_regression_specification <- function(input, output, session, time_series_data, factor_tagging_data, multiple_methods = FALSE, volatility_prediction = TRUE){
  
}

