# Run the application
#' Runs the Shiny app.
#'
#' @return This starts up the shiny app interface
#' @import shiny
#' @export
startShinyApp <- function(port = 19901, is_local_development = TRUE) {
  # default port is set to 19901
  # if is_local_development is set to false, then a browser window will
  # not open up. We should use FALSE in production server
  options_args <- list(
    shiny.maxRequestSize = 10000 * 1024^5,
    shiny.launch.browser = FALSE,
    shiny.port = port,
    test.mode = getOption("shiny.testmode", FALSE)
  )

  if (is_local_development == FALSE) {
    options_args$shiny.host <- "0.0.0.0"
  }
  do.call(options, options_args)
  shinyApp(ui, server)
}
