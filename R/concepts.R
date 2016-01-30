#' Get UMLS concept relations.
#
#'
#' @param CUI
#' @param pageNumber
#' @param pageSize
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

