#' TileDB Array Selector Module UI
#'
#' @param id ID for module
#' @importFrom DT DTOutput
#' @noRd

arraySelectorUI <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 11,
      # shiny::br(),
      shiny::textInput(
        inputId = ns("uri"),
        label = "URI",
        value = "/Users/aaronwolen/Dropbox (Personal)/work/tiledb/tiledb-ukbiobank-gwas/data/gwasdbv2",
        # placeholder = "TileDB URI",
        width = "100%"
      )
    ),
    shiny::column(
      width = 1,
      shiny::br(),
      shiny::actionButton(
        inputId = ns("submit"),
        label = "",
        icon = icon("check"),
        class = "btn-primary"
      )
    )
  )
}


#' TileDB Array Selector Module Server
#'
#' @param id ID for module
#' @returns reactive containing a [`tiledb::tiledb_array`] object
#' @noRd
arraySelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    shiny::eventReactive(input$submit, {
      req(input$uri)
      log_msg("Loading array from URI")
      try(
        tiledb::tiledb_array(
          input$uri,
          query_type = "READ",
          as.data.frame = TRUE
        ),
        silent = TRUE
      )
    })

  })
}

#' TileDB Array Selector App
#'
#' @examples
#' \dontrun{
#' arraySelector(tdb)
#' }
#' @export
arraySelector <- function() {

  ui <- fluidPage(
    theme = gwasexplorer_theme(),
    arraySelectorUI("array"),
    verbatimTextOutput("selection")
  )

  server <- function(input, output, session) {
    tdb_array <- arraySelectorServer("array")
    output$selection <- shiny::renderText({
      log_msg(sprintf("tdb_array's class is %s", class(tdb_array())))
      if (inherits(tdb_array(), "tiledb_array")) {
        msg <- sprintf(
          "Successfully loaded TileDB array '%s'",
          basename(tdb_array()@uri)
        )
      } else {
        msg <- attr(tdb_array(), "condition")$message
      }
      log_msg(msg)
      msg
    })
  }
  shinyApp(ui, server)
}
