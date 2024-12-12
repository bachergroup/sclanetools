library(shiny)
library(DT)
library(shinyjs)

#' @export
visualizerModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("

      .top-container {
        background: lightgray;
        margin: 10px;
        border-radius: 8px;
        background-color: #f5f5f5;
      }
      /* Title styling */
      h1 {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-weight: bold;
        color: #4A4A4A;
        text-align: center;
        padding: 20px;
        border-radius: 8px;
      }

      /* Subtitle styling */
      h4 {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: #666666;
        text-align: center;
        padding-bottom: 10px;
      }

      /* Paragraph styling */
      p {
        font-size: 14px;
        color: #888888;
        text-align: center;
        padding: 5px;
      }

      /* Custom styling for file input */
      .shiny-input-container {
        text-align: center;
        margin: 20px auto;
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
        margin: 10px;
        padding: 10px;
        background: #f5f5f5;
        border-radius: 8px;
      }

      #genePlot {
        padding: 10px;
      }

      #geneTable {
        padding: 10px;
        background: white;
      }

      #downloadPlot {
        margin-top: 20px;
        width: 100%;
      }

      #downloadTable {
        margin-top: 20px;
        width: 100%;
      }

      /* Loading Spinner */
      #loadingSpinner {
        display: none;
        text-align: center;
        padding: 20px 0px;
      }
      #loadingSpinner img {
        width: 100px;
      }
    "))
    ),

    # Wrapping all content in a div for padding
    div(
      class = "app-container",
      div(
        class = "top-container",
        # Title Panel
        tags$h1("SCLANE Visualizer"),
        tags$h4("Explore the plots and data of SCLANE"),
        tags$p("Upload the .RData object for simulation data to proceed."),

        # File Selection and Upload Button
        fluidRow(
          column(12,
            align = "center",
            fileInput("rdsFileInput", label = "File Selection Tool", buttonLabel = "Browse"),
            actionButton("visualize", "Visualize", class = "btn-primary")
          )
        ),

        # Loading Spinner
        div(
          id = "loadingSpinner",
          "Loading...",
        ),
      ),

      # Main Content Layout
      fluidRow(
        column(
          6,
          div(
            class = "plot-container",
            div(
              plotOutput("genePlot", height = "600px", width = "100%")
            ),
            conditionalPanel(
              condition = "output.genePlot",
              downloadButton("downloadPlot", "Download PDF")
            )
          )
        ),
        column(
          6,
          div(
            class = "datatable-container",
            div(
              DT::dataTableOutput("geneTable", height = "600px")
            ),
            conditionalPanel(
              condition = "output.geneTable",
              downloadButton("downloadTable", "Download CSV")
            )
          )
        )
      )
    )
  )
}
