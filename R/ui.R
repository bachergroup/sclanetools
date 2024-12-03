library(shiny)

source("R/databuilder_ui.R")
source("R/visualizer_ui.R")

ui <- fluidPage(
  titlePanel("scLANE: Tools for Single-Cell RNA-Seq Analysis"),
  tabsetPanel(
    tabPanel("Data Builder", databuilderModuleUI("databuilder")),
    tabPanel("Visualizer", visualizerModuleUI("visualizer"))
  )
)
