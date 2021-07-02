#' Server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  array_object <- shiny::eventReactive(input$submit, {
    req(input$uri)
    log_msg("Loading array from URI")
    tiledb::tiledb_array(
      input$uri,
      query_type = "READ",
      as.data.frame = TRUE
    )
  })

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
    shiny::req(array_object(), query_params())
    log_msg(
      sprintf("Querying array across %i dimensions", length(query_params()))
    )
    query <- query_params()
    tdb <- array_object()
    tiledb::selected_ranges(tdb) <- query
    tdb[]
  })

  output$table <- DT::renderDT({
    req(tbl_results())
    DT::datatable(tbl_results())
  })

}
