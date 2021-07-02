app_ui_navbar <- function(request) {
  navbarPage(
    title = "GWAS Explorer",
    windowTitle = "GWAS Explorer",
    fluid = TRUE,
    theme = gwasexplorer_theme(),
    id = "tabs",

    shiny::tabPanel(
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

        arraySelectorUI("gwas_array"),

        shiny::fluidRow(
          shiny::selectizeInput(
            inputId = "phenotype",
            label = "Phenotype",
            choices = .tbl_phenotypes$description,
            selected = .tbl_phenotypes$description[1]
          )
        ),

        tabsetPanel(
          id = "main-tabs",
          type = "tabs",
          tabPanel(
            "Schema",
            regionSelectorUI(id = "region"),
            DT::DTOutput("table")
          )
          # tabPanel("Results", app_ui_results(), class = "p-3"),
          # tabPanel("Snippets", app_ui_snippets(), class = "p-3")
        ),

      ) # div.container

    ) # tabPanel
  ) # navbar page
}
