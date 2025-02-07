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
          label = "Upload Expression Matrix / Seurat Object (requires .rds)",
          accept = c(".rds")
        ),
        radioButtons(ns("model_type"),
          label = "Select Model Type",
          choices = list("GLM", "GLMM", "GEE")
        ),
        numericInput(ns("n_potential_basis_fns"),
          label = "No. of basis functions (optional, defaults to 3, recommended between 2-5)",
          value = 3,
          min = 1
        ),
        numericInput(ns("highly_variable_genes"),
          label = "How many highly variable genes?",
          value = 2000,
          min = 1
        ),
        numericInput(ns("random_seed"),
          label = "Random seed",
          value = 312
        ),
        textInput(ns("pseudo_time_column"),
          label = "Name of pseudotime column in metadata",
          value = ""
        ),
        textInput(ns("cell_offset_column"),
          label = "Name of cell offset column in metadata (optional)",
          value = ""
        ),
        textInput(ns("subject_id"),
          label = "Name of subject id column in metadata (required for GEE or GLMM)",
          value = ""
        ),
        actionButton(ns("generate"), "Generate"),
        downloadButton(ns("download_file"), "Download File", style = "display: none;")
      ),
      mainPanel(
        h4("Instructions:"),
        p("Fill in the required fields to generate the executable file for scLANE analysis.
          
      <br>
Input:

xxAdd examples of model objects screenshot.XX

Model type:

Number of basis functions: 

Number of highly variable genes: Select how many highly variable genes to test using scLANE.
If HVG's have already been identified and saved in the Seurat object, they will be pulled directly. 
Otherwise, HVG calculation will be performed as XX. Alternatively, you may submit a vector of genes for
testing by naming a list object as HVG in your custom RDS upload.

Random seed: Enter a value for reproducibility.

Pseudotime column: This should be provided in the Seurat object metadata or as a vector in a 
named list object (enter the object name in this case).

<p> Cell offset column: If provided, this should be in the Seurat object metadata or as a vector in a 
named list object (enter the object name in this case). If not provided, offsets will be created as counts-per-10k. In general, 
the offset represents the scaling factor used to account for sequencing depth factor differences 
across cells. </p>

Subject ID column: If GEE or GLMM mode are selected, then a Seurat metadata column or named list
object containing the subject IDs is required.
  

"),
        htmlOutput(ns("validation_message"), style = "color: red; font-weight: bold;")
      )
    )
  )
}
