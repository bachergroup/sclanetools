library(shiny)
source("R/databuilder_server.R")
source("R/visualizer_server.R")

server <- function(input, output, session) {
  sclanetools::visualizerModuleServer("visualizer")
  sclanetools::databuilderModuleServer("databuilder")
}
