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
    exhaust_search(FUN = get_source_atoms_page,PARSER = parse_results, source = source, id = id, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
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
    exhaust_search(FUN = get_source_rels_page, PARSER = parse_results, source = source, id = id, includeRelationLabels = includeRelationLabels, includeAdditionalRelationLabels = includeAdditionalRelationLabels)
}

get_source_rels_page <- function(source, id, includeRelationLabels = NULL, includeAdditionalRelationLabels = NULL, pageNumber = 1, pageSize = 25) {
    params <- list(ticket = get_service_ticket(get_TGT()), includeRelationLabels = includeRelationLabels, includeAdditionalRelationLabels = includeAdditionalRelationLabels, pageNumber = pageNumber, pageSize = pageSize)
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

#' Get source familial relationships.
#'
#' @param source The source vocabulary.
#' @param id The internal source ID.
#' @param type The type of familial relationship to obtain. Can be one of \code{parents, children, ancestors, descendants}. See the possible \code{/source/id/} endpoints at the UMLS \href{https://documentation.uts.nlm.nih.gov/rest/home.html}{UMLS}.
#'
#' @return A list of results. These are of class \code{SourceAtomCluster} in the UMLS.
#' @export
#'
#' @examples
get_source_family <- function(source, id, type) {
  exhaust_search(FUN = get_source_family_page, PARSER = parse_results, source = source, id = id, type = type)
}

get_source_family_page <- function(source, id, pageNumber = 1, pageSize = 25, type) {
  params <- list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber,pageSize = pageSize )
  r <- GET(restBaseURL, path = paste0("rest/content/current/source/", source, "/", id, "/", type), query = params)
  r
}

#' @rdname get_source_family
#' @export
get_source_parents <- function(source, id){
  get_source_family(source, id, "parents")
}

#' @rdname get_source_family
#' @export
get_source_children <- function(source, id){
  get_source_family(source, id, "children")
}

#' @rdname get_source_family
#' @export
get_source_ancestors <- function(source, id){
  get_source_family(source, id, "ancestors")
}

#' @rdname get_source_family
#' @export
get_source_descendants <- function(source, id){
  get_source_family(source, id, "descendants")
}

#' Get all of the parents of a given source item.
#'
#' @param source The source vocabulary.
#' @param id The internal source ID.
#'
#' @return A \code{data.tree} which contains the tree of parents.
#' @export
#'
#' @examples
get_all_parents <- function(source, id){
  curNode <- Node$new(id)
  get_all_parents_rec(source, id, curNode)
  curNode
}

get_all_parents_rec <- function(source, id, curNode ){
  results <- get_source_parents(source, id)
  if(!is.null(results) ){
  uis <- sapply(results, function(x) x$ui)
  nodes <- lapply(1:length(uis), function(x) curNode$AddChild(uis[[x]], desc = results[[x]]$name ))
  sapply(1:length(nodes), function(x) get_all_parents_rec(source, uis[[x]], nodes[[x]]))
  }
}


#' Get all of the children of a given source item.
#'
#' @param source The source vocabulary.
#' @param id The internal source ID.
#'
#' @return A \code{data.tree} which contains the tree of children.
#' @export
#'
#' @examples
#' # Get all the CPT codes that are children of the CUI \code{C0519273}.
#' conceptAtoms <- get_concept_atoms("C0519273")
#' cptCodes <- conceptAtoms %>% list.filter(rootSource == 'CPT') %>% list.select(code) %>% list.apply(function(x) rev(str_split(x, "/")[[1]])[[1]])
#' cptKids <- cptCodes %>% list.apply(function(x) get_all_children("CPT", x))
#' cptLeaves <- cptKids %>% list.apply(function(x) x$Get('desc', filterFun = isLeaf)) %>% list.apply(function(x) data.frame(proc_code = names(x), desc = x )) %>% bind_rows()
get_all_children <- function(source, id){
  curNode <- Node$new(id)
  get_all_children_rec(source, id, curNode)
  curNode
}

get_all_children_rec <- function(source, id, curNode ){
  results <- get_source_children(source, id)
  if(!is.null(results) ){
    uis <- sapply(results, function(x) x$ui)
    nodes <- lapply(1:length(uis), function(x) curNode$AddChild(uis[[x]], desc = results[[x]]$name ))
    sapply(1:length(nodes), function(x) get_all_children_rec(source, uis[[x]], nodes[[x]]))
  }
}


