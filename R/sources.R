#' @include generics.R
NULL
#' Get UMLS source info for a concept.
# 
#'
#' @param source Source vocabulary.
#' @param id Source ID.
#' @return
#' @export
#'
get_source_info <- function(source, id) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/source/", source, "/", id), query = params)
    parse_results(r)
}

#' Get UMLS source info for a concept.
# 
#'
#' @param source Source vocabulary.
#' @param id Source ID.
#' @return
#' @export
#'
get_source_atoms <- function(source, id, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE, preferred = FALSE) {
    exhaust_search(FUN = get_source_atoms_page, source = source, id = id, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete, 
        includeSuppressible = includeSuppressible, preferred = preferred)
}

#' @rdname get_source_atoms
get_source_atoms_page <- function(source, id, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE, pageNumber = 1, 
    pageSize = 25, preferred = FALSE) {
    params = list(ticket = get_service_ticket(get_TGT()), sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete, includeSuppressible = includeSuppressible, 
        pageNumber = pageNumber, pageSize = pageSize)
    if (preferred == TRUE) {
        r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/atoms/preferred"), query = params)
    } else {
        r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/atoms"), query = params)
    }
    r
}

#' Get parents of source object.
#'
#' @param source
#' @param id
#'
#' @return
#' @export
#'
get_source_parents <- function(source, id) {
    exhaust_search(FUN = get_source_parents_page, source = source, id = id)
}

#' @rdname get_source_parents
get_source_parents_page <- function(source, id, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/parents"), query = params)
    r
}

#' Get children of source object.
#'
#' @param source
#' @param id
#'
#' @return
#' @export
#'
get_source_children <- function(source, id) {
    exhaust_search(FUN = get_source_children_page, source = source, id = id)
}

#' @rdname get_source_children
get_source_children_page <- function(source, id, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/children"), query = params)
    r
}

#' Get descendants of source object.
#'
#' @param source
#' @param id
#'
#' @return
#' @export
#'
get_source_descendants <- function(source, id) {
    exhaust_search(FUN = get_source_descendants_page, source = source, id = id)
}

#' @rdname get_source_descendants
get_source_descendants_page <- function(source, id, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/descendants"), query = params)
    r
}

#' Get ancestors of source object.
#'
#' @param source
#' @param id
#'
#' @return
#' @export
#'
get_source_ancestors <- function(source, id) {
    exhaust_search(FUN = get_source_ancestors_page, source = source, id = id)
}

#' @rdname get_source_ancestors
get_source_ancestors_page <- function(source, id, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/ancestors"), query = params)
    r
}

#' Get relations for source object.
#'
#' @param source
#' @param id
#' @param includeRelationLabels
#' @param includeAdditionalRelationLabels
#' @return
#' @export
#'
get_source_rels <- function(source, id, includeRelationLabels = NULL, includeAdditionalRelationLabels = NULL) {
    if (!is.null(includeRelationLabels)) {
        includeRelationLabels <- paste(includeRelationLabels, collapse = ",")
    }
    if (!is.null(includeAdditionalRelationLabels)) {
        includeAdditionalRelationLabels <- paste(includeAdditionalRelationLabels, collapse = ",")
    }
    exhaust_search(FUN = get_source_rels_page, source = source, id = id, includeRelationLabels = includeRelationLabels, includeAdditionalRelationLabels = includeAdditionalRelationLabels)
}

#' @rdname get_source_rels
get_source_rels_page <- function(source, id, includeRelationLabels = NULL, includeAdditionalRelationLabels = NULL, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()), includeRelationLabels = includeRelationLabels, includeAdditionalRelationLabels = includeAdditionalRelationLabels)
    r <- GET(restBaseURL, path = paste0("rest/content/current/", source, "/", id, "/relations"), query = params)
    r
}

#' Get source-asserted attributes.
#'
#' @param AUI
#'
#' @return
#' @export
#'
#' @examples
get_source_attrs <- function(aui) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/AUI/", aui, "/attributes"), query = params)
    parse_results(r)
} 
