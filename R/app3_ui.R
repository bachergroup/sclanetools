library(shiny)
library(shinyjs)
#' @export
embedwebModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$iframe(src = "https://sclane.rc.ufl.edu/",
                width = "100%", height = "600px",
                frameborder = "0")
  )
}