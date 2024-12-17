library(shiny)
library(shinyjs)
#' @export
databuilderModuleUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("Generate Executable File for scLANE Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("upload_file"),
          label = "Upload Expression Matrix / Seurat Object",
          accept = c(".rds")
        ),
        radioButtons(ns("model_type"),
          label = "Select Model Type",
          choices = list("GLM", "GLMM", "GEE")
        ),
        numericInput(ns("n_potential_basis_fns"),
          label = "No. of Basis Functions (optional, defaults to 5, recommended between 3-7)",
          value = 5,
          min = 1
        ),
        numericInput(ns("highly_variable_genes"),
          label = "How many highly variable genes?",
          value = 2000,
          min = 1
        ),
        numericInput(ns("random_seed"),
          label = "Random Seed",
          value = 312
        ),
        textInput(ns("pseudo_time_column"),
          label = "Name of Pseudo Time Column",
          value = ""
        ),
        textInput(ns("cell_offset_column"),
          label = "Cell Offset Column Name (optional)",
          value = ""
        ),
        actionButton(ns("generate"), "Generate"),
        downloadButton(ns("download_file"), "Download File", style = "display: none;")
      ),
      mainPanel(
        h4("Instructions:"),
        p("Fill in the required fields and upload the necessary files to generate the executable file for scLANE analysis."),
        htmlOutput(ns("validation_message"), style = "color: red; font-weight: bold;")
      )
    )
  )
}
