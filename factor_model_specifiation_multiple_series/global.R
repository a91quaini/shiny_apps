time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")

#' @title Factor model specification (UI)
#'   
#' @description Choose specification for factor model for measuring exposures.
#'   
#' @param module_header_text \code{character}, defines some local mocule title.
#'   
#' @details This module allows the user to choose factors for estimating a
#'   linear asset pricing model \cr \cr
#'   
#'   There are the following static UI elements: \cr
#'   
#'   - \code{textOutput("site_header")} renders the title of the page \cr
#'   
#'   - \code{actionButton("validate_and_proceed")} finishes work on the step and
#'   forces a move down the workflow \cr
#'   
#'   - \code{actionButton("toggle_time_varying_betas)} to allow estimation with
#'   time-varying betas. The time variation specification is locked and hidden
#'   from the user.
#'   
#'   - \code{htmlOutput("model_summary")} nicely formatted equation of the model
#'   that the user is going to estimate, information about number of
#'   osbervations, frequency, everything else that one would like to know about
#'   the model. Use other than htmlOutput if you need. \cr
#'   
#'   There are the following dynamic UI elements, generated in
#'   \code{\link{cross_sectional_plotting}}: \cr
#'   
#'   - A \code{selectInput} (single) box with estimation methods to be used. For
#'   a start, \code{lm/lmrob} if \code{gos_2016=FALSE}, this box is not shown.
#'   
#'   - A \code{selectInput} (multiple) for choice of factors. This is populated
#'   from \code{factor_tagging_data} in the server function by taking the names
#'   of the join of \code{time_series_data} and \code{factor_tagging_data}}.
#'   
#'   \cr \cr
#'   
#' @seealso \code{\link{factor_model_specifiation_multiple_series}}
#'   
#' @export

factor_model_specifiation_multiple_series_ui <- function(id, module_header_text) {
  ns <- NS(id)
  
  fluidRow(
    tags$h1(gettext(module_header_text)),
    
    sidebarLayout(
      sidebarPanel(
        
        selectizeInput(ns("estimation_methods"), choices = NULL,
                       label = gettext("Choose estimation method")),
        
        tags$br(),
        
        selectizeInput(ns("factors"), choices = NULL,
                       label = gettext("Choose factors")),
        
        tags$br(),
        
        actionButton(ns("toggle_time_varying_betas"), gettext("Time-varying betas"))
      ),
      
      mainPanel(
        htmlOutput(ns("model_summary"))
      )
    )
  )
}


#' @title Factor model specification (server)
#'   
#' @description Choose specification for linear factor model.
#'   
#' @param time_series_data tidy \code{data.frame} that is the output of 
#'   \code{\link{data_presentation_and_selection}}. It also contains factors.
#'   
#' @param tagging_data \code{data.frame} that contains information about the 
#'   grouping of data for aggregate/cross-sectional plotting. This table is 
#'   formed at the financial instrument level. It has two columns: \code{id} and
#'   \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa.
#'   
#' @param factor_tagging_data \code{data.frame} as above, but has information 
#'   about which variables are factors. Plus, must contain information on 
#'   conditioning variables for time-varying betas. In the current version we 
#'   will propose only one conditioning variable, no choices for the students at
#'   this level. Factor variables are tagged \code{factor}, conditioning 
#'   variables are tagged \code{time_varying_coef}.
#'   
#' @param robust_regression \code{logical}. If \code{FALSE} only the user-chosen
#'   method is used. If \code{TRUE}, OLS estimates will be returned alongside 
#'   robst estimates
#'   
#' @param gos_2016 \code{logical}. Cannot be \code{(gos_2016 = TRUE) & 
#'   (multiple_methods = TRUE)}. If \code{gos_2016=TRUE}, use their method. This
#'   has to be implemented and reflected in the UI.
#'   
#' @return The function returns a list with the following fields: \cr
#'   
#'   - \code{coefficents}: data frame with fields \code{id} (series identifier),
#'   \code{coeff_name} (alpha, beta_1, gamma_1 etc.), \code{coeff_value} numeric
#'   \cr
#'   
#'   - \code{t_statistics}: data frame with fields \code{id} (series 
#'   identifier), \code{coeff_name} (alpha, beta_1), \code{t_stat_value} numeric
#'   \cr
#'   
#'   - \code{evaluation_time_series_data}: data frame with fields \code{id}
#'   (series identifier), \code{date}, \code{value_type} 
#'   (\code{model_residual,model_fitted_value,model_robustness_weight})
#'   
#'   - \code{coefficients_time_series_data}: data frame with fields \code{id}
#'   (series), \code{date}, \code{factor_id} (beta on which factor, this is to store TV betas),
#'   \code{value}.
#'   
#'   - \code{time_series_data}, \cr
#'   
#'   - \code{tagging_data}, \cr
#'   
#'   - \code{factor_tagging_data}\cr \cr
#'   
#' @details The purpose of this module is to specify a factor model for running 
#'   the first-pass regression, equation by equation, with the chosen linear 
#'   method. Given that the output is a multitude of of linear models, the user 
#'   is limited to choosing RHS factor variables in the specification. If we 
#'   allow the user to choose the method, we need to be able to handle this in 
#'   the following modules. The user also chooses \code{frequency}. \cr\cr This 
#'   module needs an action button for estimation, because a large number of 
#'   regressions might need getting run. \cr\cr The estimation method from the 
#'   \code{references} field has to be implemented and its usage is forced via 
#'   input arguments.

#' @references Gagliardini, Ossola and Scaillet, Time-Varying Risk Premium in Large Cross-Sectional Equity Data Sets, \url{http://onlinelibrary.wiley.com/doi/10.3982/ECTA11069/abstract}. Code available from \url{http://www.scaillet.ch/Home_Page_of_Olivier_Scaillet.htm}

#' @export

factor_model_specifiation_multiple_series <- function(input, output, session, time_series_data, tagging_data, factor_tagging_data, multiple_methods = FALSE, gos_2016 = FALSE){
  
}
