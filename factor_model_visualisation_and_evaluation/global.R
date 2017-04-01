time_series_data <- readr::read_csv("data/time_series_data.csv")
tagging_data <- readr::read_csv("data/tagging_data.csv")

#' @title Factor model visualisation and evaluation (server)
#' @description Examine the cross section of factor model estimation results (single specification, multiple assets).
#' @param module_header_text \code{character}, defines some local mocule title.
#' @details This models allows the user to examine the performance of a given factor model specification, 
#' estimated with one or more methods, with respect to a cross-sectional dataset of asset returns. 
#' The user can examine the models on two plots concurrently. The plots are brush-linked scatterplots, 
#' just as in \code{cross_sectional_plotting_ui}. Importantly, anything plotted on the y axis should be 
#' accompanied by a confidence interval, if such information is available from the regression outputs.
#' There are the following static UI elements: \cr
#' - \code{plotOutput("plot_of_metric_1")} similarly to \code{\link{cross_sectional_plotting_ui}}
#' - \code{plotOutput("plot_of_metric_2")} similarly to \code{\link{cross_sectional_plotting_ui}}
#' - \code{textOutput("summary_for_plot_1")}
#' - \code{textOutput("summary_for_plot_2")}
#' - \code{textOutput("site_header")} renders the title of the page \cr
#' - \code{actionButton("validate_and_proceed")} finishes work on the step and forces a move down the workflow \cr
#' - \code{htmlOutput("model_summary")} nicely formatted equation of the model that the user is going to estimate, 
#' information about number of osbervations, frequency, everything else that one would like to know about the model. 
#' Use other than htmlOutput if you need. \cr
#' There are the following dynamic UI elements, generated in \code{\link{cross_sectional_plotting_ui}}: \cr
#' - A \code{selectInput} (single) box with \code{field_names} which should be put on the x axis, which are 
#' populated from selected elements of regression outputs such as alphas, betas (coefficients, in general, so that 
#' we can handle TV-beta models reasonably), idiosyncratic volatilities, R-sq. One for each plot! \cr
#' - A \code{selectInput} (single) box with \code{field_names} which should be put on the y axis, which are 
#' populated from selected elements of regression outputs such as alphas, betas (coefficients, in general, so that 
#' we can handle TV-beta models reasonably), idiosyncratic volatilities, R-sq. One for each plot! \cr
#' - A \code{selectInput} (multiple) box with \code{tags} so that various subsets of the data can be highlighted. 
#' Populated with distinct tags from \code{tagging_data}\cr
#' \cr
#' @seealso \code{\link{factor_model_specifiation_multiple_series}}
#' @export

#### add separation lines between fluidrows

factor_model_visualisation_and_evaluation_ui <- function(id, module_header_text){
  ns <- NS(id)
  
  tagList(
    tags$h1(gettext(module_header_text)),
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          actionButton(ns("model_button"), gettext("Model summary"))
        ),
        
        mainPanel(
          htmlOutput(ns("model_summary"))
        )
      )
    ),
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput(ns("element_x_plot1"), choices = NULL,
                         label = gettext("Choose element x-axis")),
          
          selectizeInput(ns("element_y_plot1"), choices = NULL,
                         label = gettext("Choose element y-axis")),
          
          selectizeInput(ns("tags"), choices = NULL,
                         label = gettext("Choose tags")),
          
          tags$br(),
          
          uiOutput(ns("help_summary_plot_1")),
          verbatimTextOutput(ns("summary_plot_1"))
        ),
        
        mainPanel(
          plotOutput(ns("plot1"))
        )
      )
    ),
    
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectizeInput(ns("element_x_plot2"), choices = NULL,
                         label = gettext("Choose element x-axis")),
          
          selectizeInput(ns("element_y_plot2"), choices = NULL,
                         label = gettext("Choose element y-axis")),
          
          selectizeInput(ns("tags"), choices = NULL,
                         label = gettext("Choose tags")),
          
          tags$br(),
          
          uiOutput(ns("help_summary_plot_2")),
          verbatimTextOutput(ns("summary_plot_2"))
        ),
        
        mainPanel(
          plotOutput(ns("plot2"))
        )
      )
    )
    
  )
  
}

#' @title Factor model visualisation and evaluation (server)
#' @description Examine the cross section of factor model estimation results (single specification, multiple assets).
#' @param model_list \code{list}, output from \code{factor_model_specifiation_multiple_series} or 
#' \code{factor_model_specifiation_single_series}
#' @param tagging_data \code{data.frame} that contains information about the grouping of data for 
#' aggregate/cross-sectional plotting. This table is formed at the financial instrument level. 
#' It has two columns: \code{id} and \code{tag}. You can have multiple \code{ids} per \code{tag} and vice versa.
#' @return list with fields:\cr
#' - \code{selected_model} for further examination of results on a single stock.
#' @details The purpose of this function is to allow for a visual inspection of estimation results in the 
#' cross-section. It should offer graphical overviews and brief summaries of estimated alphas, factor loadings, 
#' R-squareds, idiosyncratic volatilities across assets. We might reuse functions from 
#' \code{link{cross_sectional_plotting}} here a lot.
#' @export

factor_model_visualisation_and_evaluation <- function(input, output, session, model_list, tagging_data){
  
}
