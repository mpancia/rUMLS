#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
#' @importFrom rlist list.filter
NULL
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
#' @return A list of results.
#' @export
search_UMLS <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words") {
    results <- exhaust_search(FUN = search_UMLS_page, PARSER = parse_search, search = search, inputType = inputType, includeObsolete = includeObsolete,
        includeSuppressible = includeSuppressible, sabs = sabs, searchType = searchType)
    results
}

#' @rdname search_UMLS
search_UMLS_page <- function(search, inputType = "sourceUi", includeObsolete = FALSE, includeSuppressible = FALSE, sabs = NULL, searchType = "words",
    pageNumber = 1, pageSize = 25) {
    ticket <- get_service_ticket(get_TGT())
    params <- list(ticket = ticket, string = search, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, sabs = sabs,
        searchType = searchType, pageNumber = pageNumber, pageSize = pageSize, inputType = inputType)
    r <- GET(restBaseURL, path = "rest/search/current", query = params)
    r
}

#' @rdname search_UMLS
parse_search <- function(result) {
    resContent <- content(result)
    results <- resContent$result$results
    if (results[[1]]$ui == "NONE") {
        NULL
    } else {
        results
    }
}

#' Obtain source vocabularies with a given licensing restriction level.
#'
#' Various UMLS source vocabularies have licensing restrictions, ranging from none (0) to level 4. See \href{https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/license_agreement_appendix.html}{UMLS} for details.
#'
#' @param level The maximum restriction level.
#' @param us Whether or not you are looking at US or non-US restriction levels.
#'
#' @return A list of source vocabularies that have restriction levels less than the entered level.
#' @export
#'
#' @examples
#' # Get all restriction-less US source vocabularies
#' source_level(0)
source_level <- function(level, us = TRUE){
  data("restrictions")
  if(us == TRUE){
    umls_restrictions[umls_restrictions$restrictions_us <= level,]$rootSource
  } else {
    umls_restrictions[umls_restrictions$restrictions_non_us <= level,]$rootSource
  }
}


#' Filter results to include source vocabularies with a particular restriction level.
#'
#' See also \code{\link{source_level}}.
#'
#' @param results Search results. Should be a list of things with a \code{rootSource} attribute.
#' @param level The maximum restriction.
#' @param us Whether or not you are looking at US or non-US restriction levels.
#'
#' @return The results, filtered to include only those in source vocabularies with an appropriate restriction level.
#' @export
#'
#' @examples
#' # Get diabetic retinopathy results that are from source vocabularies with restriction level at most 1.
#' search <- search_UMLS("diabetic retinopathy")
#' filteredSearch <- search %>% filter_source_level(1)
filter_source_level <- function(results, level, us = TRUE){
  goodVocabs <- source_level(level, us)
  results %>% list.filter(rootSource %in% goodVocabs)
}
