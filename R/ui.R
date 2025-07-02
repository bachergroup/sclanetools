library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("scLANE Tools"),
  tabsetPanel(
    tabPanel("Data Builder", databuilderModuleUI("databuilder")),
    tabPanel("scLANE", embedwebModuleUI("sclane")),
    tabPanel("Visualizer", visualizerModuleUI("visualizer"))
  )
)
