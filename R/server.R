library(shiny)
library(dplyr)
library(scLANE)
library(Seurat)
library(shinyjs)

server <- function(input, output, session) {
  # Define reactive values
  values <- reactiveValues(uploaded_file = NULL, generated_file = NULL)
  validation_message <- reactiveVal("") # Initialize reactiveVal for validation messages

  # Observe file upload
  observe({
    req(input$upload_file)
    values$uploaded_file <- input$upload_file$datapath
  })

  # Generate the file when the "Generate" button is clicked
  observeEvent(input$generate, {
    shinyjs::hide("download_file")
    validation_errors <- validate_inputs(input) # Validate inputs

    if (length(validation_errors) > 0) {
      validation_message(paste(validation_errors, collapse = "<br>")) # Display validation messages
      return() # Exit if there are validation errors
    }

    validation_message("") # Clear validation message if successful

    show_processing_modal() # Show processing modal

    tryCatch(
      {
        returned_list <- process_and_generate_file(input, values) # Process the file

        # Assign the generated file path to reactive values
        values$generated_file <- returned_list

        # Remove the processing modal and show the download button
        removeModal()
        shinyjs::show("download_file")
      },
      error = function(e) {
        handle_error(e) # Handle error
      }
    )
  })

  # Provide the download handler for the generated RDS file
  output$download_file <- downloadHandler(
    filename = function() {
      # Create a filename based on the original uploaded file name
      original_file_name <- basename(input$upload_file$name)
      paste0("inputObject_", gsub("\\..*$", "", original_file_name), ".rds")
    },
    content = function(file) {
      print(values$generated_file)
      req(values$generated_file)
      saveRDS(values$generated_file, file) # Save the generated RDS file
    }
  )

  # Render validation messages
  output$validation_message <- renderText({
    validation_message() # Display the current validation message
  })
}

# Function to validate inputs
validate_inputs <- function(input) {
  validation_errors <- c()

  # Basic validations
  if (is.null(input$upload_file)) {
    validation_errors <- c(validation_errors, "Please upload a file.\n")
  }
  if (input$pseudo_time_column == "") {
    validation_errors <- c(validation_errors, "Pseudo time column name is required.\n")
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

# Function to validate random seed
is_valid_random_seed <- function(random_seed) {
  if (is.null(random_seed) || random_seed == "" || is.na(random_seed)) {
    return(FALSE)
  }
  random_seed_value <- suppressWarnings(as.integer(random_seed))
  return(!is.na(random_seed_value) && random_seed_value >= -2^31 && random_seed_value <= 2^31 - 1)
}

# Function to validate highly variable genes
is_valid_highly_variable_genes <- function(highly_variable_genes) {
  if (is.null(highly_variable_genes) || highly_variable_genes == "" || is.na(highly_variable_genes)) {
    return(FALSE)
  }
  highly_variable_genes_value <- suppressWarnings(as.integer(highly_variable_genes))
  return(!is.na(highly_variable_genes_value) && highly_variable_genes_value > 0)
}

# Function to validate if the pseudo time column exists in the uploaded RDS object
is_valid_column <- function(upload_file, pseudo_time_column) {
  if (is.null(upload_file)) {
    return(FALSE)
  }

  uploaded_object <- tryCatch(
    {
      readRDS(upload_file$datapath)
    },
    error = function(e) {
      return(NULL)
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

# Function to show the processing modal
show_processing_modal <- function() {
  showModal(modalDialog(
    title = "Processing",
    "Your file is being processed. This may take a few moments.",
    easyClose = TRUE
  ))
}

# Function to handle errors
handle_error <- function(error) {
  removeModal()
  showModal(modalDialog(
    title = "Error",
    paste("An error occurred:", error$message),
    easyClose = TRUE
  ))
}

# Updated process_and_generate_file function with more generic variable names and improved structure
process_and_generate_file <- function(input, values) {
  # Default to 5 if the number of basis functions is not provided
  n_potential_basis_fns <- ifelse(is.null(input$n_potential_basis_fns) || input$n_potential_basis_fns == "" || is.na(input$n_potential_basis_fns),
    5,
    as.integer(input$n_potential_basis_fns)
  )

  # Directly use the reactive value for the uploaded file
  uploaded_object <- tryCatch(
    {
      readRDS(values$uploaded_file) # Using the reactive stored file path
    },
    error = function(e) {
      stop("Uploaded file could not be read. Ensure it's a valid RDS object.\n")
    }
  )

  # Get highly variable genes based on user input
  hvgs <- HVFInfo(uploaded_object) %>%
    filter(mean > 0.05) %>%
    arrange(desc(variance.standardized)) %>%
    mutate(gene = rownames(.))

  num_hvgs <- input$highly_variable_genes
  hvgs <- hvgs[1:num_hvgs, ] # Filter based on the user input
  pseudo_time_column <- input$pseudo_time_column

  # Sort the observations in the uploaded object
  sorted_object <- sortObservations(uploaded_object,
    pt = uploaded_object[[pseudo_time_column]][[1]],
    id.vec = uploaded_object$fetal.ids
  )

  cell_offset <- createCellOffset(sorted_object)
  pt_df <- data.frame(DPT = sorted_object[[pseudo_time_column]][[1]])

  # Determine model type based on user input
  is_geee <- input$model_type == "GEE"
  is_glmm <- input$model_type == "GLMM"

  # Prepare the input object for processing
  input_object <- list(
    expr.mat = sorted_object@assays$RNA@counts,
    size.factor.offset = cell_offset,
    pt = pt_df,
    genes = hvgs$gene,
    id.vec = sorted_object$fetal.ids,
    is.gee = is_geee, # Set based on user model selection
    is.glmm = is_glmm, # Set based on user model selection
    n.potential.basis.fns = n_potential_basis_fns, # Use the input or default value
    random.seed = as.integer(input$random_seed) # Use the input random seed
  )

  return(input_object)
}
