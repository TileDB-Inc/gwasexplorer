#' GWAS Explorer Server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}
#' @import shiny
#' @importFrom utils modifyList
#' @noRd

app_server <- function(input, output, session) {
  tiledb::tiledb_stats_enable()
  stat_file <- tempfile()

  tdb_array <- arraySelectorServer(id = "gwas_array")
  query_region <- regionSelectorServer(id = "region")

  observe({
    if (inherits(tdb_array(), "tiledb_array")) {
      log_msg("Enabling query/results tab")
      js$enableTab("Query")
      js$enableTab("Results")
      js$enableTab("Plot")
      js$enableTab("Stats")
      js$enableTab("Snippets")

      log_msg("Updating phenotype choices")
      is_s3 <- grepl("^s3", tdb_array()@uri)
      if (is_s3) {
        # assume it's full array on
        phenos <- .tbl_phenotypes$description
      } else {
        # assume it's a local array created for the UseR tutorial
        phenos <- .tbl_tutorial_phenotypes$description
      }
      shiny::updateSelectizeInput(
        session,
        inputId = "phenotype",
        choices = c("", phenos),
        selected = "",
        server = TRUE
      )
    } else {
      log_msg("Disabling query/results tab")
      js$disableTab("Query")
      js$disableTab("Results")
      js$disableTab("Plot")
      js$disableTab("Stats")
      js$disableTab("Snippets")
      shiny::updateTabsetPanel(session, "main-tabs", selected = "About")
    }
  })

  query_params <- shiny::reactive({
    req(query_region)
    req(input$phenotype)

    log_msg(sprintf("Updating query for phenotype: %s", input$phenotype))
    utils::modifyList(
      query_region(),
      list(phenotype = cbind(input$phenotype, input$phenotype))
    )
  })

  shiny::observeEvent(input$submit_query, {
    req(input$`main-tabs` == "About")
    log_msg("Automatically switching to Results tab")
    shiny::updateTabsetPanel(
      session,
      inputId = "main-tabs",
      selected = "Results"
    )
  })

  tbl_results <- shiny::eventReactive(input$submit_query, {
    shiny::req(tdb_array(), query_params())
    log_msg(
      sprintf("Querying array across %i dimensions", length(query_params()))
    )

    # convert from -log10 scale
    thresh <- 10^-input$threshold
    tdb <- tdb_array()
    tiledb::selected_ranges(tdb) <- query_params()

    # use query condition to filter by pvalue (not available in release version)
    if (thresh < 1) {
      tiledb::query_condition(tdb) <- tiledb::tiledb_query_condition_init(
        attr = "pval",
        value = thresh,
        dtype = "FLOAT64",
        op = "LE"
      )
    }

    tdb[]
  })

  output$table <- DT::renderDT({
    req(tbl_results())
    DT::datatable(tbl_results())
  })

  output$stats <- shiny::renderText({
    req(tbl_results())
    tiledb::tiledb_stats_dump(stat_file)
    tiledb::tiledb_stats_reset()
    paste0(readLines(stat_file), collapse = "\n")
  })

  manhattanPlotServer("gwas_plot", data = tbl_results)
  querySnippetsServer("query_snippets", array = tdb_array, query = query_params)
}
