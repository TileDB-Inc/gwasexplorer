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
#' @param array [`tiledb::tiledb_array`] object
#' @param query query list
#' @returns Nothing.
#' @importFrom glue glue
#' @noRd
querySnippetsServer <- function(id, array, query) {
  moduleServer(id, function(input, output, session) {
    stopifnot(shiny::is.reactive(array))
    stopifnot(shiny::is.reactive(query))

    uri <- shiny::reactive({
      stopifnot(inherits(array(), "tiledb_array"))
      array()@uri
    })

    output$r_snippet <- shiny::renderText({
      req(uri())
      log_msg("Updating R snippet")

      glue::glue("
library(tiledb)

gwas_array <- tiledb::tiledb_array(
  uri = \"{uri()}\",
  is.sparse = TRUE,
  as.data.frame = TRUE
)

tiledb::selected_ranges(gwas_array) <- list(
  phenotype = cbind(\"{query()$phenotype[1]}\", \"{query()$phenotype[2]}\"),
  chr = cbind(\"{query()$chr[1]}\", \"{query()$chr[2]}\"),
  pos = cbind({query()$pos[1]}, {query()$pos[2]})
)

gwas_array[]
"
      )
    })

    output$py_snippet <- shiny::renderText({
      req(uri())
      log_msg("Updating Python snippet")

      glue::glue("
import tiledb

gwas_array = tiledb.open(\"{uri()}\")

(gwas_array
  .query(
    dims = [\"{names(query()[1])}\", \"{names(query()[2])}\", \"{names(query()[3])}\"],
  )
  .df[
    \"{query()$phenotype[1]}\":\"{query()$phenotype[2]}\",
    \"{query()$chr[1]}\":\"{query()$chr[2]}\",
    {formatC(query()$pos[1], mode = 'integer')}:{formatC(query()$pos[2], mode = 'integer')},
  ]
)"
      )
    })

  })
}

#' Query Snippets App
#'
#' @inheritParams querySnippetsServer
#' @examples
#' \dontrun{
#'  tbl_gwas <- data.frame(
#'    phenotype = rep("height", 100),
#'    chr = rep("1", 100),
#'    pos = sort(sample(1:1e6, 100)),
#'    pval = runif(n = 100)
#'  )
#'  uri <- tempdir()
#'  tiledb::fromDataFrame(uri = array_uri, tbl_gwas)
#'
#'  querySnippetsApp(
#'    tiledb::tiledb_array(uri),
#'   list(
#'     phenotype = cbind("height", "height"),
#'     chr = cbind("1", "1"),
#'     pos = cbind(500, 600)
#'   )
#' )
#' }
#' @noRd
querySnippetsApp <- function(array, query) {
  ui <- fluidPage(
    theme = gwasexplorer_theme(),
    querySnippetsUI(id = "snippets")
  )
  server <- function(input, output, session) {
    tiledb_array <- shiny::reactive(array)
    tiledb_query <- shiny::reactive(query)
    querySnippetsServer(id = "snippets", tiledb_array, tiledb_query)
  }
  shinyApp(ui, server)
}
