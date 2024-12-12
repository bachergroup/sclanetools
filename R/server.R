library(shiny)
server <- function(input, output, session) {
  visualizerModuleServer("visualizer")
  databuilderModuleServer("databuilder")
}
