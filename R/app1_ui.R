library(shiny)
library(shinyjs)
#' @export
databuilderModuleUI <- function(id) {
  nshiny <- NS(id)

  fluidPage(
    ## RB: This link will change to webserver once live.
    titlePanel(tags$h2("Generate executable file for running ", tags$a("scLANE",href="https://sclane-tools.rc.ufl.edu/"))),
    sidebarLayout(
      sidebarPanel(
        fileInput(nshiny("upload_file"),
          label = tags$h5("Upload Seurat Object or Custom File (requires .rds)")
        ),
        uiOutput(nshiny("set_model_type")),
        
        uiOutput(nshiny("set_n_potential_basis_fns")),
        
        uiOutput(nshiny("set_highly_variable_genes")),
      
        uiOutput(nshiny("set_random_seed")),
        
        uiOutput(nshiny("select_pseudo_time_column")), 
        uiOutput(nshiny("select_cell_offset_column")), 
        uiOutput(nshiny("select_subject_id")),
        actionButton(nshiny("generate"), "Generate"),
       
        
        width=3
      ),
      mainPanel(
        
      
        ## RB: Add link to Github ReadMe showing screenshots
        tags$h4("Upload data and then fill in the fields to generate an executable
                file required for running scLANE webserver analysis. Additional details are provided below.",
                style="text-align:left; color:black"),
      
        # tags$br(),
        tags$p(tags$b("Model type: "), "For single-subject/sample data use GLM mode. For multi-subject/sample data, choose one of GEE and 
               GLMM mode, which provide population-level dynamics or sample-specific dynamics, respectively.",style="text-align:left; color:black"),
        
        # tags$br(),
        tags$p(tags$b("Number of basis functions: "), "Controls the maximum complexity of the expression dynamics. We recommend 
               three to cover most dynamic patterns. ",style="text-align:left; color:black"),
        
        # tags$br(),
        tags$p(tags$b("Number of highly variable genes: "), "Select how many highly variable genes to test using scLANE. If HVG's
              have already been identified and saved in the Seurat object, they will be pulled directly. Alternatively, you may 
              submit a vector of genes for testing by naming a list object as HVG 
              in your custom RDS upload.",style="text-align:left; color:black"),
          
        # tags$br(),
        tags$p(tags$b("Random seed: "), "Enter a value for reproducibility.",style="text-align:left; color:black"),
        
        # tags$br(),
        tags$p(tags$b("Pseudotime column: "), "This must be provided in the Seurat object metadata or as a vector 
               in a named list object (enter the object name in this case).",style="text-align:left; color:black"),
        
        # tags$br(),
        tags$p(tags$b("Cell offset column: "), "If provided, this should be in the Seurat object metadata or as a vector in a 
              named list object (enter the object name in this case). If not provided, offsets will be created as counts-per-10k.
              The offset represents the scaling factor used to account for sequencing depth factor differences 
              across cells.",style="text-align:left; color:black"),
        
        # tags$br(),
        tags$p(tags$b("Subject ID column: "), "If GEE or GLMM mode are selected, then a Seurat metadata column or named list
          object containing the subject/sample IDs is required.",style="text-align:left; color:black"),
        
 tags$br(),
          htmlOutput(nshiny("validation_message"), style = "color: red; font-weight: bold;"),
            
            h6("Preview of uploaded file or Seurat metadata:",style="text-align:left; color:black"),
            div(style="width:1500px;", verbatimTextOutput(nshiny("geneTable"), placeholder=TRUE)),
       
        br(),
        downloadButton(nshiny("download_file"), "Download File", style = "color: blue; display: none;"),
        br(),
      
          htmlOutput(nshiny("end_message"),style="text-align:left; color:black"),

 
        p("Please report any comments, questions, or issues at ", tags$a("Github",href="https://github.com/bachergroup/sclanetools/issues")),
 
      )
    )
  )
}
