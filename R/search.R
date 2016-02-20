#' Search the UMLS.
#'
#' @param search The search input.
#' @param inputType The type of input. Can be one of
#' @param includeObsolete Whether or not to include obsolete results.
#' @param includeSuppressible Whether or not to include suppressible results.
#' @param sabs Source vocabularies to search.
#' @param searchType Type of search.
#' @param response_type
#' @return If \code{response_type == 'list'}, then a list of results. Otherwise, returns a list of concepts with the\code{response_type} info included.
#' @export
search_UMLS <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words", 
    response_type = "list") {
    results <- exhaust_search(FUN = search_UMLS_page, PARSER = parse_search, search = search, inputType = inputType, includeObsolete = includeObsolete, 
        includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType)
    if (response_type == "list") {
        results
    } else {
        lapply(results, function(x) get_concept(x, info_ret = response_type))
    }
}

search_UMLS_page <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words", 
    pageNumber = 1, pageSize = 25) {
    ticket <- get_service_ticket(get_TGT())
    params <- list(ticket = ticket, string = search, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, sabs = sabs, 
        searchType = searchType, pageNumber = pageNumber, pageSize = pageSize, inputType = inputType)
    r <- GET(restBaseURL, path = "rest/search/current", query = params)
    r
}

parse_search <- function(result) {
    resContent <- content(result)
    results <- resContent$result$results
    if (results[[1]]$ui == "NONE") {
        NULL
    } else {
        results
    }
} 
