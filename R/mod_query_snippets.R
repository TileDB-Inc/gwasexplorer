#' Query Snippets Module UI
#'
#' @param id ID for module
#' @noRd
querySnippetsUI <- function(id) {
  ns <- NS(id)
  shiny::div(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        tags$h4("R", class = "text-right")
      ),
      shiny::column(
        width = 10,
        shiny::verbatimTextOutput(ns("r_snippet"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 2,
        tags$h4("Python", class = "text-right")
      ),
      shiny::column(
        width = 10,
        shiny::verbatimTextOutput(ns("py_snippet"))
      )
    )
  )
}


#' Query Snippets Module Server
#'
#' @param id ID for module
#' @param uri scalar chector vector containing TileDB array URI
#' @param query query list
#' @returns Nothing.
#' @noRd
querySnippetsServer <- function(id, uri, query) {
  moduleServer(id, function(input, output, session) {
    stopifnot(shiny::is.reactive(uri))
    stopifnot(shiny::is.reactive(query))

    output$r_snippet <- shiny::renderText({
      req(uri())
      log_msg("Updating R snippet")

      query_file <- tempfile()
      query_obj <- query()
      dump("query_obj", file = query_file, evaluate = FALSE, control = c("keepNA", "keepInteger", "niceNames"))

      sprintf("
library(tiledb)

gwas_array <- tiledb::tiledb_array(
  uri = \"%s\",
  is.sparse = TRUE,
  as.data.frame = TRUE
)

tiledb::selected_ranges(gwas_array) <- %s
",
        uri(), readLines(query_file)
      )
    })

  })
}

#' Query Snippets App
#'
#' @inheritParams querySnippetsServer
#' @examples
#' \dontrun{
# tbl_gwas <- data.frame(
#   phenotype = rep("height", 100),
#   chr = rep("1", 100),
#   pos = sort(sample(1:1e6, 100)),
#   pval = runif(n = 100)
# )
# uri <- tempdir()
# tiledb::fromDataFrame(uri = array_uri, tbl_gwas)

# querySnippetsApp(
#   tiledb::tiledb_array(uri),
#   list(phenotype = cbind("height", "height"))
# )
#' }
#' @noRd
querySnippetsApp <- function(tiledb_array, tiledb_query) {
  ui <- fluidPage(
    theme = gwasexplorer_theme(),
    querySnippetsUI(id = "snippets")
  )
  server <- function(input, output, session) {
    array <- shiny::reactive(tiledb_array)
    query <- shiny::reactive(tiledb_query)
    querySnippetsServer(id = "snippets", array, query)
  }
  shinyApp(ui, server)
}
