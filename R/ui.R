library(shiny)

ui <- fluidPage(
  titlePanel("Generate Executable File for scLANE Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload_file",
        label = "Upload Expression Matrix / Seurat Object",
        accept = c(".rds")
      ),
      radioButtons("model_type",
        label = "Select Model Type",
        choices = list("GLM", "GLMM", "GEE")
      ),
      numericInput("basis_functions",
        label = "No. of Basis Functions (optional, defaults to 5, recommended between 3-7)",
        value = 5,
        min = 1
      ),
      numericInput("variable_genes",
        label = "How many highly variable genes?",
        value = 2000,
        min = 1
      ),
      numericInput("random_seed",
        label = "Random Seed",
        value = 312
      ),
      textInput("pseudo_time_column",
        label = "Name of Pseudo Time Column",
        value = ""
      ),
      textInput("cell_offset_column",
        label = "Cell Offset Column Name (optional)",
        value = ""
      ),
      actionButton("generate_download", "Generate and Download")
    ),
    mainPanel(
      h4("Instructions:"),
      p("Fill in the required fields and upload the necessary files to generate the executable file for scLANE analysis.")
    )
  )
)
