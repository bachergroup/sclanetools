library(dplyr)
library(scLANE)
library(DT)
library(ggplot2)
library(shinyjs)

visualizerModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Disable the upload button initially
    shinyjs::disable("uploadFile")

    # Enable the upload button when a file is uploaded
    observeEvent(input$rdsFileInput, {
      shinyjs::enable("uploadFile")
    })

    # Reactive object for the RDS file
    rdsObject <- reactive({
      req(input$rdsFileInput) # Ensure file is provided
      object <- readRDS(input$rdsFileInput$datapath)
      return(object)
    })

    # Reactive value for the selected gene
    selectedGene <- reactiveVal(NULL)

    # Reactive list of gene names
    genesNameList <- reactive({
      req(rdsObject()) # Ensure RDS object is available
      sim_data <- rdsObject()

      # Simulate gene names from data
      genesNameList <- c(
        sample(rownames(sim_data)[rowData(sim_data)$geneStatus_overall == "Dynamic"], size = 50),
        sample(rownames(sim_data)[rowData(sim_data)$geneStatus_overall == "NotDynamic"], size = 50)
      )
      return(genesNameList)
    })

    # Reactive cell offset
    cellOffset <- reactive({
      req(rdsObject())
      sim_data <- rdsObject()
      set.seed(312)
      cell_offset <- scLANE::createCellOffset(sim_data)
      return(cell_offset)
    })

    # Reactive dataframe for cell time (order)
    orderDF <- reactive({
      req(rdsObject())
      sim_data <- rdsObject()
      set.seed(312)
      order_df <- data.frame(X = sim_data$cell_time_normed)
      return(order_df)
    })

    # Reactive GLM model
    glmModel <- reactive({
      req(rdsObject(), genesNameList())
      shinyjs::show("loadingSpinner")
      sim_data <- rdsObject()
      set.seed(312)
      scLANE_models_glm <- scLANE::testDynamic(sim_data,
        pt = orderDF(),
        genes = genesNameList(),
        size.factor.offset = cellOffset(),
        n.cores = 4L,
        verbose = FALSE
      )
      shinyjs::hide("loadingSpinner")
      return(scLANE_models_glm)
    })

    # Reactive for plot object
    plotInputObject <- reactive({
      req(glmModel(), orderDF(), cellOffset()) # Ensure all inputs are ready
      sim_data <- rdsObject()

      geneToPlot <- genesNameList()[1] # Default to first gene
      if (!is.null(selectedGene())) {
        geneToPlot <- selectedGene() # Use selected gene if available
      }

      req(geneToPlot)

      # Generate the plot for the selected gene
      plot <- scLANE::plotModelCoefs(glmModel(),
        gene = geneToPlot,
        pt = orderDF(),
        expr.mat = sim_data,
        size.factor.offset = cellOffset()
      )

      # Add a title to the plot
      plot + labs(title = "Title")
      # todo, fix why plots and labels are not changins

      # # Generate the plot for the selected gene
      # return(scLANE::plotModelCoefs(glmModel(),
      #   gene = geneToPlot,
      #   pt = orderDF(),
      #   expr.mat = sim_data,
      #   size.factor.offset = cellOffset()
      # ))
    })

    # Observe when a row is selected in the gene table
    observeEvent(input$geneTable_rows_selected, {
      req(genesNameList()) # Ensure gene list is available
      selectedIndex <- input$geneTable_rows_selected
      selectedGene(genesNameList()[selectedIndex]) # Update selected gene
    })

    # Render the plot
    output$genePlot <- renderPlot(
      {
        req(plotInputObject()) # Ensure plot data is ready
        plotInputObject() # Render the plot
      },
      width = "auto",
    )

    output$geneTable <- DT::renderDataTable({
      req(genesNameList())

      # Convert the list to a DataFrame
      geneDF <- data.frame(Gene = genesNameList(), stringsAsFactors = FALSE)

      # Render DataTable with left-aligned column header and values
      DT::datatable(geneDF,
        rownames = FALSE,
        selection = "single",
        options = list(
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-left", targets = "_all") # Left-align both header and values
          )
        )
      ) %>% DT::formatStyle(
        columns = "Gene",
        textAlign = "left" # Left-align the text content
      )
    })

    # Download handler for PDF export
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("gene_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Create the plot
        pdf(file, height = 6, width = 8) # Specify size of PDF
        print(plotInputObject()) # Print the plot to the PDF
        dev.off() # Close the PDF device
      }
    )

    # Download handler for CSV export of the gene table
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("gene_table_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(genesNameList()) # Ensure gene list is available
        # Convert the list to a DataFrame for download
        geneDF <- data.frame(Gene = genesNameList(), stringsAsFactors = FALSE)
        write.csv(geneDF, file, row.names = FALSE)
      }
    )
  })
}
