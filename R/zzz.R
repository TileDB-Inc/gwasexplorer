.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath("www", system.file("www", package = "gwasexplorer"))
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
}
