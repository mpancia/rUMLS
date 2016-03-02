#' @include generics.R
#' @import data.tree
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

get_source_rels_page <- function(source, id, includeRelationLabels = NULL, includeAdditionalRelationLabels = NULL, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()), includeRelationLabels = includeRelationLabels, includeAdditionalRelationLabels = includeAdditionalRelationLabels)
    r <- GET(restBaseURL, path = paste0("rest/content/current/source/", source, "/", id, "/relations"), query = params)
    r
}

#' Get source-asserted attributes.
#' @export
get_source_attrs <- function(source, id) {
    params <- list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/source/", source, "/", id, "/attributes"), query = params)
    parse_results(r)
}

#' @export
get_source_family <- function(source, id, type) {
  exhaust_search(FUN = get_source_family_page, PARSER = parse_results, source = source, id = id, type = type)
}

get_source_family_page <- function(source, id, pageNumber = 1, pageSize = 25, type) {
  params <- list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber,pageSize = pageSize )
  r <- GET(restBaseURL, path = paste0("rest/content/current/source/", source, "/", id, "/", type), query = params)
  r
}


#' @export
get_all_parents <- function(source, id){
  curNode <- Node$new(id)
  get_all_parents_rec(source, id, curNode)
  curNode
}

get_all_parents_rec <- function(source, id, curNode ){
  results <- get_source_family(source, id, type = "parents")
  if(!is.null(results) ){
  uis <- sapply(results, function(x) x$ui)
  nodes <- lapply(1:length(uis), function(x) curNode$AddChild(uis[[x]], desc = results[[x]]$name ))
  sapply(1:length(nodes), function(x) get_all_parents_rec(source, uis[[x]], nodes[[x]]))
  }
}

#' @export
get_all_children <- function(source, id){
  curNode <- Node$new(id)
  get_all_children_rec(source, id, curNode)
  curNode
}

get_all_children_rec <- function(source, id, curNode ){
  results <- get_source_family(source, id, type = "children")
  if(!is.null(results) ){
    uis <- sapply(results, function(x) x$ui)
    nodes <- lapply(1:length(uis), function(x) curNode$AddChild(id = uis[[x]], desc = results[[x]]$name ))
    sapply(1:length(nodes), function(x) get_all_children_rec(source, uis[[x]], nodes[[x]]))
  }
}

#kids <- get_concept_atoms("C0519273") %>% list.filter(rootSource == 'CPT') %>% list.select(code) %>% list.apply(function(x) rev(str_split(x, "/")[[1]])[[1]]) %>% list.apply(function(x) get_all_children("CPT", x)) %>% list.apply(function(x) x$Get('id', filterFun = isLeaf)) %>% list.apply(function(x) data.frame(proc_code = x, desc = names(x))) %>% bind_rows()
