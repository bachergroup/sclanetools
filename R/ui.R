library(shiny)

ui <- fluidPage(
  titlePanel("scLANE: Tools for Single-Cell RNA-Seq Analysis"),
  tabsetPanel(
    tabPanel("Data Builder", databuilderModuleUI("databuilder")),
    # tabPanel("Visualizer", visualizerModuleUI("visualizer"))
  )
)
