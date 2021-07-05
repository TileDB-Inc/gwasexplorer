# options(repos = BiocManager::repositories())
# rsconnect::deployApp(
#   appName = "gwasexplorer",
#   appFiles = c(
#     ".Renviron",
#     "app.R",
#     "DESCRIPTION",
#     "LICENSE",
#     "LICENSE.md",
#     "man",
#     "NAMESPACE",
#     "R",
#     "inst",
#     "shiny-gwas-explorer.Rproj"
#   )
# )

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
gwasexplorer::run_app()
