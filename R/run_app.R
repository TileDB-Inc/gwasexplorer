#' GWAS Explorer
#'
#' Run the gwasexplorer application in your browser.
#'
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL) {

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking
  )
}
