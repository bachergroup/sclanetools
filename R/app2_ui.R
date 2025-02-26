library(shiny)
library(DT)
library(shinyjs)

#' @export
visualizerModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("

      .top-container {
        background: lightgray;
        margin: 1px;
        border-radius: 1px;
        background-color: #f5f5f5;
      }
      /* Title styling */
      h1 {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-weight: bold;
        color: #4A4A4A;
        text-align: center;
        padding: 1px;
        border-radius: 1px;
      }

      /* Subtitle styling */
      h4 {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: #666666;
        text-align: center;
        padding-bottom: 1px;
      }

      /* Paragraph styling */
      p {
        font-size: 14px;
        color: #888888;
        text-align: center;
        padding: 1px;
      }

      /* Custom styling for file input */
      .shiny-input-container {
        text-align: center;
        margin: 5px auto;
        width: 50%;
        font-size: 16px;
      }

      input[type='file'] {
        border: 2px solid #ddd;
        border-radius: 6px;
        padding: 10px;
        background-color: #f9f9f9;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        transition: all 0.2s ease-in-out;
      }

      /* Custom button styling */
      #visualize {
        width: 300px;
        display: block;
        margin: 0 auto 20px;
        border-radius: 6px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        transition: background-color 0.3s ease;
      }

      #visualize:hover {
        cursor: pointer;
      }

      /* Plot and table styling */
      .shiny-plot-output, .dataTable {
        border-radius: 8px;
      }

      /* Aligning download buttons */
      .download-button {
        display: block;
        margin: 15px auto;
        width: 100%;
        padding: 10px;
      }

      /* Adding padding around the entire app content */
      .app-container {
        padding: 15px;
      }

      .plot-container {
        margin: 10px;
        padding: 10px;
        background: #f5f5f5;
        border-radius: 8px;
      }

      .datatable-container {
        margin: 5px;
        padding: 5px;
        background: #f5f5f5;
        border-radius: 8px;
      }

      #genePlot {
        padding: 5px;
      }

      #geneTable {
        padding: 5px;
        background: white;
      }

      #downloadPlot {
        margin-top: 10px;
        width: 100%;
      }

      #downloadTable {
        margin-top: 10px;
        width: 100%;
      }

      /* Loading Spinner */
      #loadingSpinner {
        display: none;
        text-align: center;
        padding: 10px 0px;
      }
      #loadingSpinner img {
        width: 100px;
      }
    "))
    ),

    # Wrapping all content in a div for padding
      div(
        class = "app-container",
      
          
       column(3,
        div(
          class = "top-container",
          # Title Panel
          tags$h1("scLANE Visualizer"),
          tags$p("Upload both the results .RDS object output from scLANE webserver and the input .RDS used to generate the results."),
  
          # File Selection and Upload Button
          fluidRow(  
            column(10,
              align = "center",
              fileInput(ns("rdsFileInput_Res"), label = "Upload scLANE Model Results", buttonLabel = "Browse"),
              fileInput(ns("rdsFileInput_Orig"), label = "Upload scLANE Input Data", buttonLabel = "Browse"))
          ),
  
          # Loading Spinner
          div(
            id = "loadingSpinner",
            "Loading...",
          )
        )),

      # Main Content Layout
        column(9,
          div(
            class = "plot-container",
            div(
              plotOutput(ns("genePlot"), width = "100%")
            ),
            conditionalPanel(
              condition = "output.genePlot  != NULL",
              downloadButton(ns("downloadPlot"), "Download PDF")
            )
          )
        ),
      fluidRow(  
        column(
          12,
          div(
            class = "datatable-container",
            div(
              DT::dataTableOutput(ns("geneTable"), width = "100%")
            ),
            conditionalPanel(
              condition = "output.geneTable != NULL",
              downloadButton(ns("downloadTable"), "Download CSV")
            )
          )
        )
      )
      )
    )
}
