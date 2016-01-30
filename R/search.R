#' @importFrom httr GET content
restBaseURL <- "https://uts-ws.nlm.nih.gov/"

#' Search the UMLS.
#'
#' This searches the UMLS for concepts.
#'
#' @param search Input to search.
#' @param inputType
#' @param includeObsolete
#' @param includeSuppressible
#' @param sabs Source vocabularies, comma delimited.
#' @param searchType
#'
#' @export
#'
search_UMLS <- function(search, inputType = "sourceUi", includeObsolete = FALSE,
                        includeSuppressible = FALSE, sabs = NULL, searchType = "words")
{
  exhaust_search(search_UMLS_page, search = search, inputType = inputType, includeObsolete = includeObsolete,
                includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType)
}

#' @rdname search_UMLS
search_UMLS_page <- function(search, inputType = "sourceUi", includeObsolete = FALSE,
                        includeSuppressible = FALSE, sabs = NULL, searchType = "words", pageNumber = 1,
                        pageSize = 25)
{
  ticket <- get_service_ticket(get_TGT())
  params <- list(ticket = ticket, string = search, includeObsolete = includeObsolete,
    includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType,
    pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = "rest/search/current", query = params)
  r
}

#' @rdname search_UMLS
parse_search <- function(result)
{
  resContent <- content(result)
  results <- resContent$result$results
  if(results[[1]]$ui == "NONE")
  {
    NULL
  } else
  {
    results
  }
}
