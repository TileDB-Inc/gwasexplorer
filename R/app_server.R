#' Server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  tdb_array <- arraySelectorServer(id = "gwas_array")

  query_region <- regionSelectorServer(id = "region")

  query_params <- reactive({
    req(query_region)
    log_msg("Updating query with selected phenotype")
    modifyList(
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

}
