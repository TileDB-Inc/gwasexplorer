#' GWAS Explorer Server-side logic
#'
#' @param input,output,session Internal parameters for {shiny}
#' @import shiny
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

  output$plot <- shiny::renderPlot({
    # req(tbl_results())

    log_msg(sprintf("Rendering plot with %s rows.", nrow(tbl_results())))
    # xlims <- input$range_slider * 1e6
    # ylims <- c(0, max(tbl_gwas$MaxLog10Pvalue[input$gwas_table_rows_selected]))

    base_p <- ggplot2::ggplot() +
      # scale_y_continuous(limits = ylims) +
      ggplot2::scale_x_continuous("Chromosome location (Mb)")#, limits = xlims, labels = to_mb)

    if (nrow(tbl_results()) == 0) {
      base_p
    } else {
      base_p +
        ggplot2::geom_point(
          data = subset(tbl_results(), -log10(pval) >= 1),
          ggplot2::aes(pos, -log10(pval)),
          alpha = 0.5
        ) +
        ggplot2::facet_grid(. ~ phenotype)
    }
  })

}
