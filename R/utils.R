authBaseURL <- "https://utslogin.nlm.nih.gov"
authEndpoint <- "/cas/v1/tickets"
restBaseURL <- "https://uts-ws.nlm.nih.gov/"

#' @importFrom stringr str_split

# Exhaust the search result over all pages.
exhaust_search <- function(FUN = searchFunction, PARSER = parseFunction, ...) {
    results <- list()
    curPage <- 1
    keepSearching <- TRUE
    while (keepSearching == TRUE) {
        curResult <- PARSER(FUN(..., pageNumber = curPage))
        if (!is.null(curResult)) {
            results <- c(results, list(curResult))
            curPage <- curPage + 1
        } else {
            keepSearching <- FALSE
        }
    }
    unlist(results, recursive = F)
}

# Parser functions for various response types.
parse_results <- function(result) {
    if(status_code(result) != 200){
      NULL
    } else {
    resContent <- content(result)
    results <- resContent$result
    if (length(results) == 0) {
        NULL
    } else {
        results
    }
    }
}
