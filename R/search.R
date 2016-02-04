
#' @export
search_UMLS <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words", response_type = "list") {
    results <- exhaust_search(FUN = search_UMLS_page, PARSER = parse_search, search = search, inputType = inputType, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible,
        sabs = sabs, searchType = searchType)
    if(response_type == "list") {
      results
    } else {
      lapply(results, function(x) get_concept(x, info_ret = response_type ))
    }
}

search_UMLS_page <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words",
    pageNumber = 1, pageSize = 25) {
    ticket <- get_service_ticket(get_TGT())
    params <- list(ticket = ticket, string = search, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType, pageNumber = pageNumber, pageSize = pageSize, inputType= inputType)
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
