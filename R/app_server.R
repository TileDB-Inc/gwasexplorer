#' GWAS Explorer Server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}
#' @import shiny
#' @importFrom utils modifyList
#' @noRd

app_server <- function(input, output, session) {

  tdb_array <- arraySelectorServer(id = "gwas_array")
  query_region <- regionSelectorServer(id = "region")

  observe({
    if (inherits(tdb_array(), "tiledb_array")) {
      log_msg("Enabling query/results tab")
      js$enableTab("Query")
      js$enableTab("Results")
      js$enableTab("Plot")
    } else {
      log_msg("Disabling query/results tab")
      js$disableTab("Query")
      js$disableTab("Results")
      js$disableTab("Plot")
      shiny::updateTabsetPanel(session, "main-tabs", selected = "About")
    }
  })

  query_params <- reactive({
    req(query_region)
    log_msg("Updating query with selected phenotype")
    utils::modifyList(
      query_region(),
      list(phenotype = cbind(input$phenotype, input$phenotype))
    )
  })

  tbl_results <- shiny::reactive({
    shiny::req(tdb_array(), query_params())
    log_msg(
      sprintf("Querying array across %i dimensions", length(query_params()))
    )
    query <- query_params()
    tdb <- tdb_array()
    tiledb::selected_ranges(tdb) <- query
    tdb[]
  })

  output$table <- DT::renderDT({
    req(tbl_results())
    DT::datatable(tbl_results())
  })

  manhattanPlotServer("gwas_plot", data = tbl_results)

}
