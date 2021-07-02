
regionSelectorUI <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(

    shiny::column(
      width = 2,
      shiny::selectizeInput(
        inputId = ns("contig"),
        label = "Chromosome",
        choices = names(.supported_genomes$grch37),
        selected = "chr1"
      )
    ),

    shiny::column(
      width = 10,
      shiny::sliderInput(
        inputId = ns("contig_range"),
        label = "Select Range",
        min = 0L,
        max = 250L,
        value = c(1L, 5L),
        step = 1L,
        dragRange = TRUE,
        post = "Mb",
        width = "100%"
      )
    )
  )
}


# param contigs: vector of named chromosome lengths
regionSelectorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    chr_length <- reactive({
      log_msg(sprintf("Calculating Mb length for chr %s", input$contig))
      to_mb(unname(.supported_genomes$grch37[input$contig]))
    })

    observe({
      req(chr_length())
      log_msg("Updating region slider")
      shiny::updateSliderInput(
        session,
        inputId = "contig_range",
        min = 1L,
        max = chr_length()
      )
    })

    # pull back slider if selected range is outside the new chr's boundary
    observe({
      req(input$contig_range[2] > chr_length())
      slider_range <- c(chr_length() - diff(input$contig_range), chr_length())

      log_msg(glue::glue(
        "Selected range extends beyond chr{input$contig}'s length\n",
        "  - manually adjusting to [{slider_range[1]},{slider_range[2]}]"
      ))

      shiny::updateSliderInput(
        session,
        inputId = "contig_range",
        value = slider_range
      )
    })

    # ensure width of selected range is not zero
    observe({
      req(diff(input$contig_range) == 0)
      slider_range <- input$contig_range
      if (slider_range[1] == 1) {
        slider_range <- c(1, 2)
      } else {
      slider_range[1] <- slider_range[2] - 1
      }

      log_msg(glue::glue(
        "Width of selected range is zero\n",
        "  - manually adjusting to [{slider_range[1]},{slider_range[2]}]"
      ))

      shiny::updateSliderInput(
        session,
        inputId = "contig_range",
        value = slider_range
      )
    })

    reactive({
      req(input$contig_range)
      log_msg("Updating region selection")
      list(
        chr = cbind(input$contig, input$contig),
        pos = cbind(to_bp(input$contig_range[1]), to_bp(input$contig_range[2]))
      )
    })

  })
}

#' Region Selector App
#'
#' @examples
#' \dontrun{
#' tdb <- tiledb::tiledb_array("tiledb://TileDB-Inc/vcf-1kg-phase3-data")
#' schemaViewer(tdb)
#' }
#' @export
regionSelectorApp <- function() {

  ui <- fluidPage(
    theme = gwasexplorer_theme(),
    regionSelectorUI("region"),
    verbatimTextOutput("selection")
  )

  server <- function(input, output, session) {
    selected_region <- regionSelectorServer("region")
    output$selection <- shiny::renderPrint({
      message("Assembling selected region\n")
      selected_region()
    })
  }
  shinyApp(ui, server)
}
