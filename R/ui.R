library(shiny)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(includeHTML("google-analytics.html")),
  titlePanel("scLANE Tools"),
  tabsetPanel(id = "mainTabs",
    tabPanel("Data Builder", databuilderModuleUI("databuilder")),
    tabPanel("scLANE", redirectModuleUI("sclane"), value = "sclaneTab"),
    tabPanel("Visualizer", visualizerModuleUI("visualizer"))
  )
)
