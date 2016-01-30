#' Get UMLS concept relations.
#
#'
#' @param CUI
#'
#' @return
#' @export
#'
get_concept_rels <- function(CUI)
{
  exhaust_search(FUN = get_concept_rels_page, CUI = CUI)
}

#' @rdname get_concept_rels
get_concept_rels_page <- function(CUI, pageNumber = 1, pageSize = 25 ){
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/relations"), query = params)
  r
}

#' Get UMLS concept definitions.
#
#'
#' @param CUI CUI of interest.
#' @param sabs Source vocabularies, comma separated.
#'
#' @return
#' @export
#'
get_concept_defs <- function(CUI, sabs = NULL)
{
  exhaust_search(FUN = get_concept_defs_page, CUI = CUI, sabs = sabs)
}

#' @rdname get_concept_defs
get_concept_defs_page <- function(CUI, sabs = NULL, pageNumber = 1, pageSize = 25 ){
  params = list(ticket = get_service_ticket(get_TGT()), sabs = sabs, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/definitions"), query = params)
  r
}

#' Get UMLS concept atoms.
#
#'
#' @param CUI CUI of interest.
#' @param sabs Source vocabularies, comma separated.
#' @param ttys
#' @param language
#' @return
#' @export
#'
get_concept_atoms <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE)
{
  exhaust_search(FUN = get_concept_atoms_page, CUI = CUI, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
                  includeSuppressible = includeSuppressible)
}

#' @rdname get_concept_atoms
get_concept_atoms_page <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE,
                          pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), sabs = sabs, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms"), query = params)
  r
}

#' Get UMLS concept info
#
#'
#' @param CUI CUI of interest.
#' @param sabs Source vocabularies, comma separated.
#' @param ttys
#' @param language
#' @return
#' @export
#'
get_concept_info <- function(CUI)
{
  exhaust_search(FUN = get_concept_defs_page, CUI = CUI )
}

#' @rdname get_concept_info
get_concept_info_page <- function(CUI, pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms"), query = params)
  r
}
