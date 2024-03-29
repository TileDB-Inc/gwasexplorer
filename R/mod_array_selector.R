#' TileDB Array Selector Module UI
#'
#' @param id ID for module
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @noRd

arraySelectorUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10,
        # shiny::br(),
        shiny::textInput(
          inputId = ns("uri"),
          label = "URI",
          value = "tiledb://TileDB-Inc/ukbiobank-gwasdb",
          # placeholder = "TileDB URI",
          width = "100%"
        )
      ),
      shiny::column(
        width = 2,
        shiny::br(),
        shiny::actionButton(
          inputId = ns("submit"),
          label = "Load Array",
          class = "btn-primary mt-2"
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 11,
        shinycssloaders::withSpinner(
          verbatimTextOutput(ns("validation_message")),
          type = 1,
          color = "#4d9fff",
          proxy.height = "100px"
        )
      )
    )
  )
}


#' TileDB Array Selector Module Server
#'
#' @param id ID for module
#' @returns reactive containing a [`tiledb::tiledb_array`] object or a
#'    try-error object.
#' @noRd
#' @importFrom shinyjs toggleClass
arraySelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    tdb_array <- shiny::eventReactive(input$submit, {
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

    output$validation_message <- shiny::renderText({
      log_msg("Updating URI input validation status")
      is_valid <- inherits(tdb_array(), "tiledb_array")
      if (is_valid) {
        msg <- sprintf(
          "Successfully loaded TileDB array '%s'",
          basename(tdb_array()@uri)
        )
      } else {
        msg <- attr(tdb_array(), "condition")$message
      }
      shinyjs::toggleClass("uri", "is-valid", condition = is_valid)
      shinyjs::toggleClass("uri", "is-invalid", condition = !is_valid)
      log_msg(msg)
      msg
    })

    # module returns the array
    shiny::reactive({
      log_msg(
        sprintf(
          "Returning '%s' object form arraySelector module",
          class(tdb_array())
        )
      )
      tdb_array()
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
    shinyjs::useShinyjs(),
    theme = gwasexplorer_theme(),
    arraySelectorUI("gwas-array")
  )

  server <- function(input, output, session) {
    arraySelectorServer("gwas-array")
  }
  shinyApp(ui, server)
}
