library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("scLANE Tools"),
  tabsetPanel(
    tabPanel("Data Builder", databuilderModuleUI("databuilder")),
    tabPanel("Visualizer", visualizerModuleUI("visualizer"))
  )
)
