library(shiny)
options(shiny.maxRequestSize=3000*1024^2)

server <- function(input, output, session) {
  databuilderModuleServer("databuilder")
  visualizerModuleServer("visualizer")
}
