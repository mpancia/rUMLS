#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
restBaseURL <- "https://uts-ws.nlm.nih.gov/rest"

#' Search the UMLS.
#'
#' This searches the UMLS for concepts.
#'
#' @param search
#' @param inputType
#' @param includeObsolete
#' @param includeSuppressible
#' @param sabs
#' @param searchType
#' @param pageNumber
#' @param pageSize
#'
search_UMLS <- function(search, inputType = "sourceUi", includeObsolete = FALSE,
  includeSuppressible = FALSE, sabs = NULL, searchType = 'words', pageNumber = 1,
  pageSize = 25){
  params <- list(ticket = ticket, search = search, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = "search/current", query = params)
  r
}
