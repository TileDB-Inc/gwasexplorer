app_ui_navbar <- function(request) {
  shiny::navbarPage(
    title = "GWAS Explorer",
    windowTitle = "GWAS Explorer",
    fluid = TRUE,
    theme = gwasexplorer_theme(),
    id = "tabs",

    shiny::tabPanel(
      shinyjs::useShinyjs(),
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
            shiny::div(
              shiny::selectizeInput(
                inputId = "phenotype",
                label = "Phenotype",
                choices = .tbl_phenotypes$description,
                selected = .tbl_phenotypes$description[1]
              ),
              regionSelectorUI(id = "region")
            )
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
            shiny::plotOutput("plot")
          )
        ),

      ) # div.container

    ) # tabPanel
  ) # navbar page
}
