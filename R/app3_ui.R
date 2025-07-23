library(shiny)
library(shinyjs)
#' @export
redirectModuleUI <- function(id) {
  nshiny <- NS(id)
  
  fluidPage(
    titlePanel(tags$h2("If you were not redirected automatically, please ",
                       tags$a("click here", href = "https://sclane.rc.ufl.edu/")))
  )
}