library(shiny)

server <- function(input, output, session) {
  observeEvent(input$generate_download, {
    showModal(modalDialog(
      title = "Processing",
      "This feature will be implemented in the future.",
      easyClose = TRUE
    ))
  })
}
