library(shiny)
library(dplyr)
library(scLANE)
library(Seurat)
library(shinyjs)
options(shiny.maxRequestSize=3000*1024^2)


#' @export
databuilderModuleServer <- function(id) {
  nshiny <- NS(id)
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(uploaded_file = NULL, generated_file = NULL)
    

    validation_message <- reactiveVal("")

    observe({
      req(input$upload_file)
      values$uploaded_file <- input$upload_file$datapath
    })
    
     observe({
       shinyjs::hide("generate")
        req(input$upload_file)
        metaObj <- readRDS(input$upload_file$datapath)
        shinyjs::show("generate")
      output$geneTable <- renderPrint({
        if(class(metaObj) != "Seurat")  str(metaObj)
        if(class(metaObj) == "Seurat")  str(metaObj@meta.data)
      })
     
     mdata <- list()
      if(class(metaObj) != "Seurat")  mdata$colNames = names(metaObj)
      if(class(metaObj) == "Seurat")  mdata$colNames = colnames(metaObj@meta.data)
     
     
      output$set_model_type <- renderUI({
       radioButtons(nshiny("model_type"),
                     label = tags$h5("Select Model Type"),
                     choices = list("GLM", "GLMM", "GEE"))
      })
       output$set_n_potential_basis_fns <- renderUI({
         numericInput(nshiny("n_potential_basis_fns"),
                      label = tags$h5("No. of basis functions (recommend 2-5)"),
                      value = 3,
                      min = 1)
      })
        output$set_highly_variable_genes <- renderUI({
      numericInput(nshiny("highly_variable_genes"),
                     label = tags$h5("No. of highly variable genes"),
          value = 2000,
          min = 1)
      })
         output$set_random_seed <- renderUI({
      numericInput(nshiny("random_seed"),
                     label = tags$h5("Random seed"),
          value = 312)
      })

     
     output$select_pseudo_time_column <- renderUI({
      selectizeInput(nshiny("pseudo_time_column"),
                     tags$h5("Select pseudotime variable"),
                     choices = c("select" = "", unique(mdata$colNames)))
      })
     
     output$select_cell_offset_column <- renderUI({
      selectizeInput(nshiny("cell_offset_column"), 
                     tags$h5("Select cell offset data (optional)"),
                     choices = c("select" = "", unique(mdata$colNames)))
      })
     
     output$select_subject_id <- renderUI({
      selectizeInput(nshiny("subject_id"), 
                     tags$h5("Select subject id variable (required for GEE and GLMM)"),
                     choices = c("select" = "", unique(mdata$colNames)))
      })
     
     })
      
   
    
    observeEvent(input$generate, {
      shinyjs::hide("download_file")
      shinyjs::hide("end_message")
  
      validation_errors <- validate_inputs(input)

      if (length(validation_errors) > 0) {
        validation_message(paste(validation_errors, collapse = "<br>"))
        return()
      }

      validation_message("")

      show_processing_modal()
      
      tryCatch(
        {
          returned_list <- process_and_generate_file(input, values)
          
          values$generated_file <- returned_list
          removeModal()
          shinyjs::hide("geneTable")
          shinyjs::show("download_file")
          shinyjs::show("end_message")
        },
        error = function(e) {
          handle_error(e)
        }
      )
      
    })

    output$download_file <- downloadHandler(
      filename = function() {
        original_file_name <- basename(input$upload_file$name)
        paste0("inputObject_", gsub("\\..*$", "", original_file_name), ".rds")
      },
      content = function(file) {
        req(values$generated_file)
        saveRDS(values$generated_file, file)
      }
    )

    output$validation_message <- renderText({
      validation_message()
    })
  
    ## RB: This link will change to webserver once live.
  output$end_message <- renderUI({
    req(values$generated_file)
      HTML(paste0("<h5>", "Next, upload this file to the ", 
                  "<a target = '_blank' href='https://sclane.rc.ufl.edu'>", 
                  "<button style='color:blue'>", "scLANE webserver", "</button></a>"))
    })
    

  })
}

validate_inputs <- function(input) {
  validation_errors <- c()


  if (is.null(input$upload_file)) {
    validation_errors <- c(validation_errors, "Please upload a file.\n")
  }
  if (input$pseudo_time_column == "") {
    validation_errors <- c(validation_errors, "Pseudotime column name is required.\n")
  }
  if (!is_valid_random_seed(input$random_seed)) {
    validation_errors <- c(validation_errors, "Please provide a valid integer for the random seed.")
  }
  if (!is_valid_highly_variable_genes(input$highly_variable_genes)) {
    validation_errors <- c(validation_errors, "Please provide a valid number for highly variable genes.")
  }
  if (!is_valid_column(input$upload_file, input$pseudo_time_column)) {
    validation_errors <- c(validation_errors, paste("Column", input$pseudo_time_column, "not found in the uploaded object."))
  }

  return(validation_errors)
}

is_valid_random_seed <- function(random_seed) {
  if (is.null(random_seed) || random_seed == "" || is.na(random_seed)) {
    return(FALSE)
  }
  random_seed_value <- suppressWarnings(as.integer(random_seed))
  return(!is.na(random_seed_value) && random_seed_value >= -2^31 && random_seed_value <= 2^31 - 1)
}

is_valid_highly_variable_genes <- function(highly_variable_genes) {
  if (is.null(highly_variable_genes) || highly_variable_genes == "" || is.na(highly_variable_genes)) {
    return(FALSE)
  }
  highly_variable_genes_value <- suppressWarnings(as.integer(highly_variable_genes))
  return(!is.na(highly_variable_genes_value) && highly_variable_genes_value > 0)
}




is_valid_column <- function(upload_file, pseudo_time_column) {
  if (is.null(upload_file)) {
    return(FALSE)
  }

    uploaded_object <- tryCatch(
    {
      readRDS(upload_file$datapath)
    },
    error = function(e) {
      return("NULL")
    }
  )

  if (!is.null(uploaded_object) && pseudo_time_column != "") {
    
    column_exists <- tryCatch(
      {
        !is.null(uploaded_object[[pseudo_time_column]])
      },
      error = function(e) {
        FALSE
      }
    )
    return(column_exists)
  }
  return(FALSE)
}


show_processing_modal <- function() {
  showModal(modalDialog(
    title = "Processing",
    "Your file is being processed. This may take a few moments.",
    easyClose = TRUE
  ))
}

handle_error <- function(error) {
  removeModal()
  showModal(modalDialog(
    title = "Error",
    paste("An error occurred:", error$message),
    easyClose = TRUE
  ))
}

process_and_generate_file <- function(input, values) {


  n_potential_basis_fns <- ifelse(is.null(input$n_potential_basis_fns) || input$n_potential_basis_fns == "" || is.na(input$n_potential_basis_fns),
    3,
    as.integer(input$n_potential_basis_fns)
  )
  
  uploaded_object <- tryCatch(
    {
      readRDS(input$upload_file$datapath)
    },
    error = function(e) {
      stop("Uploaded file could not be read. Ensure it's a valid RDS object.\n")
    }
  )

  if (class(uploaded_object) == "Seurat") {
     hvgs <- Seurat::HVFInfo(uploaded_object)
     hvgs <- hvgs[hvgs$mean > 0.05, ]
     hvgs <- hvgs[order(hvgs$variance.standardized, decreasing = TRUE), ]
     hvgs <- rownames(hvgs)[1:pmin(input$highly_variable_genes, nrow(uploaded_object), nrow(hvgs))]
  } else {
    hvgs <- uploaded_object$HVG[1:pmin(input$highly_variable_genes, nrow(uploaded_object$Data), length(uploaded_object$HVG))]
  }


  is_gee <- input$model_type == "GEE"
  is_glmm <- input$model_type == "GLMM"
  

  if(input$model_type != "GLM") {
    if (class(uploaded_object) != "Seurat") {

      sorted_object <- scLANE::sortObservations(uploaded_object,
                                      pt = uploaded_object[[input$pseudo_time_column]],
                                      id.vec = uploaded_object[[input$subject_id]]
                                      )
    } else {    
      sorted_object <- scLANE::sortObservations(uploaded_object,
                                      pt = uploaded_object[[input$pseudo_time_column]][[1]],
                                      id.vec = uploaded_object[[input$subject_id]][[1]]
                                      )
    }
    ## Need to re-order everything here!
    if (class(uploaded_object) == "Seurat") {
         sorted_cols_order <- colnames(sorted_object)
          if(input$cell_offset_column == "") {
            cell_offset <- scLANE::createCellOffset(sorted_object)
          } else cell_offset <- sorted_object[[input$cell_offset_column]][sorted_cols_order,]
          
          pt_df <- data.frame(DPT = sorted_object[[input$pseudo_time_column]][sorted_cols_order,])
          id_vec <- sorted_object[[input$subject_id]][sorted_cols_order,]
          expr_mat <- sorted_object@assays$RNA@counts[hvgs,]
          
     } else if (class(uploaded_object) != "Seurat") {
         sorted_cols_order <- colnames(sorted_object)
         if(input$cell_offset_column == "") {
           cell_offset <- scLANE::createCellOffset(sorted_object)
           } else cell_offset <- uploaded_object[[input$cell_offset_column]][sorted_cols_order]
           
         pt_df <- data.frame(DPT = uploaded_object[[input$pseudo_time_column]][sorted_cols_order])
         id_vec <- uploaded_object[[input$subject_id]][sorted_cols_order]
         expr_mat <- uploaded_object$Data[hvgs,sorted_cols_order]
    }
    
  }

  if(input$model_type == "GLM") {
    if (class(uploaded_object) == "Seurat") {
      if(input$cell_offset_column == "") {
        cell_offset <- scLANE::createCellOffset(uploaded_object)
      } else cell_offset <- uploaded_object[[input$cell_offset_column]][[1]]
      
      pt_df <- data.frame(DPT = uploaded_object[[input$pseudo_time_column]][[1]])
      id_vec <- NULL
      expr_mat <- uploaded_object@assays$RNA@counts[hvgs,]
    } else if (class(uploaded_object) != "Seurat") {
 
      if(input$cell_offset_column == "") {
        cell_offset <- scLANE::createCellOffset(uploaded_object$Data)
        } else  cell_offset <- uploaded_object[[input$cell_offset_column]]
       
      pt_df <- data.frame(DPT = uploaded_object[[input$pseudo_time_column]])
      id_vec <- NULL
      expr_mat <- uploaded_object$Data[hvgs,]

    }
  }

  input_object <- list(
    expr.mat = expr_mat,
    size.factor.offset = cell_offset,
    pt = pt_df,
    genes = hvgs,
    id.vec = id_vec,
    is.gee = is_gee,
    is.glmm = is_glmm,
    n.potential.basis.fns = n_potential_basis_fns,
    random.seed = as.integer(input$random_seed)
  )

  return(input_object)
}
