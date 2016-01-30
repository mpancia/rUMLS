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
get_concept_rels <- function(CUI, pageNumber = 1, pageSize = 25 ){
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("concent/current/CUI/", CUI, "/relations"), query = params)
  r
}
