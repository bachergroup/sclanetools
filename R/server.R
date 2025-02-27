library(shiny)
library(dplyr)
library(scLANE)
library(DT)
library(ggplot2)
library(shinyjs)
library(tidyr)
library(magrittr)
library(scLANE)
library(Seurat)

options(shiny.maxRequestSize=3000*1024^2)

server <- function(input, output, session) {
  databuilderModuleServer("databuilder")
  visualizerModuleServer("visualizer")
}
