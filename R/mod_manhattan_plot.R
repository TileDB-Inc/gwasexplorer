#' Manhattan Plot Module UI
#'
#' @param id ID for module
#' @noRd
manhattanPlotUI <- function(id) {
  ns <- NS(id)
  shiny::plotOutput(ns("plot"))
}


#' Manhattan Plot Module Server
#'
#' @param id ID for module
#' @param data data.frame containing GWAS summary statistics
#' @returns Nothing.
#' @import ggplot2
#' @noRd
manhattanPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(data))

    output$plot <- shiny::renderPlot({
      log_msg(sprintf("Rendering plot with %s rows.", nrow(data())))

      base_p <- ggplot2::ggplot() +
        ggplot2::scale_x_continuous("Chromosome location (Mb)") +
        ggplot2::scale_y_continuous(expression(-log[10] * " p-value"))

      if (nrow(data()) == 0) {
        base_p
      } else {
        base_p +
          ggplot2::aes(
            x = data()$pos * 1e-6,
            y = -log10(data()$pval)
          ) +
          ggplot2::geom_point(data = data(), alpha = 0.75, color = "#001e73") +
          ggplot2::theme_minimal(18)
      }
    })

  })
}

#' Manhattan Plot App
#'
#' @inheritParams manhattanPlotServer
#' @examples
#' \dontrun{
#' tbl_gwas <- data.frame(
#'   phenotype = rep("height", 100),
#'   chr = rep("1", 100),
#'   pos = sort(sample(1:1e6, 100)),
#'   pval = runif(n = 100)
#' )
#' manhattanPlotApp(data = tbl_gwas)
#' }
#' @noRd
manhattanPlotApp <- function(data) {
  ui <- fluidPage(
    theme = gwasexplorer_theme(),
    manhattanPlotUI(id = "manhattan_plot")
  )
  server <- function(input, output, session) {
    tbl_results <- shiny::reactive(data)
    manhattanPlotServer(id = "manhattan_plot", data = tbl_results)
  }
  shinyApp(ui, server)
}
