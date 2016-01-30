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
