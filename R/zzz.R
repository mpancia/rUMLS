#' @importFrom httr POST status_code content GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @import igraph
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rUMLS. This package requires a UMLS username and password. Run auth_UMLS() to authenticate.")
}
