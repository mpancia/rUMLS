#' Get information on all UMLS subsets.
#'
#' @return A list of information about all subsets. These are of UMLS class \code{Subset}.
#' @export
#'
get_all_subset_info <- function() {
    exhaust_search(FUN = get_all_subset_info_page, PARSER = parse_results)
}

#' @rdname get_subset_info
get_all_subset_info_page <- function(pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
    r <- GET(restBaseURL, path = paste0("rest/content/current/subsets"), query = params)
    r
}

#' Get the info on a UMLS subset.
#'
#' @param subset
#' @param language
#' @param sourceVocab
#'
#' @return A list of information. This is of UMLS class \code{Subset}.
#' @export
#'
get_subset_info <- function(sourceVocab, subset, language = NULL) {
  params <- list(ticket = get_service_ticket(get_TGT()), language = language)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", sourceVocab, "/", subset), query = params)
  parse_results(r)
}

#' Get the attributes of a UMLS subset.
#'
#' @param subset
#' @param source
#' @param language
#'
#' @return A list of attributes. These are of UMLS class \code{Attribute}.
#' @export
#'
get_subset_attr <- function(source, subset, language = NULL) {
  params <- list(ticket = get_service_ticket(get_TGT()), language = language)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/", subset, "/attributes"), query = params)
  parse_results(r)
}

#' Get members of a given subset.
#'
#' @param subset The subset.
#' @param source The source.
#' @param language The desired language.
#'
#' @return A list of results. These are of UMLS class \code{SourceConceptSubsetMember}.
#' @export
#'
get_subset_mems <- function(source, subset, language = NULL) {
    exhaust_search(FUN = get_subset_mems_page, PARSER = parse_results, subset = subset, language = language, source = source)
}

#' @rdname get_subset_mems
get_subset_mems_page <- function(subset, source, language = NULL, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()), language = language, pageNumber = pageNumber, pageSize = pageSize, source = source)
    r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/", subset, "/members"), query = params)
    r
}

#' Get information on a member of a given subset.
#'
#' @param subset The subset.
#' @param source The source.
#' @param member The member.
#' @param language The desired language.
#'
#' @return A list of attributes. These are of UMLS class \code{Attribute}.
#' @export
#'
get_mem_info <- function(subset, source, member, language = NULL) {
  params <- list(ticket = get_service_ticket(get_TGT()), language = language, source = source)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/", subset, "/members/", member, "/attributes"), query = params)
  parse_results(r)
}
