#' GWAS Explorer UI
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @noRd

app_ui <- function() {
  shiny::navbarPage(
    title = "GWAS Explorer",
    windowTitle = "GWAS Explorer",
    fluid = TRUE,
    theme = gwasexplorer_theme(),
    id = "tabs",

    shiny::tabPanel(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        script = "www/js/tabdisable.js",
        functions = c("enableTab", "disableTab")
      ),
      title = "HOME",

      div(
        class = "container",
        style = "min-height:90vh;",

        div(
          style = "width: 100%; position: relative;z-index:-9;",
          div(
            class = "pt-3",
            img(src = "www/images/logo.svg"),
            h3("GWAS Explorer", class = "font-weight-light mt-1")
          )
        ),

        tabsetPanel(
          id = "config-tabs",
          type = "tabs",
          tabPanel(
            title = "Array",
            class = "p-3",
            arraySelectorUI("gwas_array")
          ),
          tabPanel(
            title = "Query",
            class = "p-3",

            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectizeInput(
                  inputId = "phenotype",
                  label = "Phenotype",
                  choices = "",
                  selected = "",
                  width = "100%",
                )
              ),
              shiny::column(
                width = 5,
                shiny::sliderInput(
                  inputId = "threshold",
                  label = "-log10 p-value Threshold",
                  min = 0,
                  max = 10,
                  step = 1,
                  value = 1,
                  width = "100%",
                )
              ),
              shiny::column(
                width = 1,
                shiny::br(),
                shiny::actionButton(
                  inputId = "submit_query",
                  label = "Submit Query",
                  class = "btn-primary mt-2"
                )
              )
            ),
            regionSelectorUI(id = "region")
          )
        ),

        tabsetPanel(
          id = "main-tabs",
          type = "tabs",
          tabPanel(
            title = "About",
            class = "p-3",
            shiny::includeMarkdown(system.file("assets/about.md", package = "gwasexplorer"))
          ),
          tabPanel(
            title = "Results",
            class = "p-3",
            DT::DTOutput("table")
          ),
          tabPanel(
            title = "Plot",
            class = "p-3",
            manhattanPlotUI("gwas_plot")
          ),
          tabPanel(
            title = "Stats",
            class = "p-3",
            shiny::verbatimTextOutput("stats")
          ),
          tabPanel(
            title = "Snippets",
            class = "p-3",
            querySnippetsUI("query_snippets")
          )
        ),

      ) # div.container

    ) # tabPanel
  ) # navbar page
}
