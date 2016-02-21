#' Search the UMLS.
#'
#' This allows for searching of the UMLS. The API parameters correspond to those on the \href{https://documentation.uts.nlm.nih.gov/rest/search/#query-parameters}{NLM website.}
#'
#' @param search The search input.
#' @param inputType The type of input. Can be one of \code{'atom', 'code','sourceConcept','sourceDescriptor','sourceUi'} to obtain a valid response.
#' @param includeObsolete Whether or not to include obsolete results.
#' @param includeSuppressible Whether or not to include suppressible results.
#' @param sabs Source vocabularies to search. A list of elements from the \href{http://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/source_vocabularies.html}{valid source vocabularies.}
#' @param searchType Type of search. Can be one of \code{'exact','words','leftTruncation', 'rightTruncation','approximate', 'normalizedString'}.
#' @param response_type What sort of information to obtain. This can be one of \code{'list', }
#' @return If \code{response_type == 'list'}, then a list of results. Otherwise, returns a list of \linkS4class{Concept} objects with the\code{response_type} info included.
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
