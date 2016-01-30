#' Get information on all UMLS subsets.
#'
#' @return
#' @export
#'
get_all_subset_info <- function()
{
  exhaust_search(FUN = get_all_subset_info_page, PARSER = parse_all_subsets)
}

#' @rdname get_subset_info
get_all_subset_info_page <- function(pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets"), query = params)
  r
}

parse_all_subsets <- function(result)
{
  if(status_code(result) == 404)
  {
    NULL
  } else
  {
    resContent <- content(result)
    resContent$result
  }
}
#' Get the info on a UMLS subset.
#'
#' @param subset
#' @param language
#' @param source
#'
#' @return
#' @export
#'
get_subset_info <- function(source, subset, language = NULL)
{
  exhaust_search(FUN = get_subset_info_page, PARSER = parse_atoms, subset = subset, language = language, source = source)
}

#' @rdname get_subset_info
get_subset_info_page <- function(source, subset, language = NULL, pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), language = language, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/", subset), query = params)
  r
}

#' Get the attributes of a UMLS subset.
#'
#' @param subset
#' @param source
#' @param language
#'
#' @return
#' @export
#'
get_subset_attr <- function(source, subset, language = NULL)
{
  exhaust_search(FUN = get_subset_attr_page, PARSER = parse_atoms, subset = subset, language = language, source = source)
}

#' @rdname get_subset_attr
get_subset_attr_page <- function(source, subset, language = NULL, pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), language = language, pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/", subset, "/attributes"), query = params)
  r
}

#' Get members of a given subset.
#'
#' @param subset
#' @param source
#' @param language
#'
#' @return
#' @export
#'
get_subset_mems <- function(subset, source, language = NULL)
{
  exhaust_search(FUN = get_subset_mems_page, PARSER = parse_atoms, subset = subset, language = language, source = source)
}

#' @rdname get_subset_mems
get_subset_mems_page <- function(subset, source, language = NULL, pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), language = language, pageNumber = pageNumber, pageSize = pageSize, source = source)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/" , subset, "/members"), query = params)
  r
}

#' Get information on a member of a given subset.
#'
#' @param subset
#' @param source
#' @param member
#' @param language
#'
#' @return
#' @export
#'
get_mem_info <- function(subset, source, member, language = NULL)
{
  exhaust_search(FUN = get_subset_mems_page, PARSER = parse_atoms, subset = subset, language = language, source = source, member = member)
}

#' @rdname get_mem_info
get_subset_mems_page <- function(subset, source, member, language = NULL, pageNumber = 1, pageSize = 25 )
{
  params = list(ticket = get_service_ticket(get_TGT()), language = language, pageNumber = pageNumber, pageSize = pageSize, source = source)
  r <- GET(restBaseURL, path = paste0("rest/content/current/subsets/source/", source, "/" , subset, "/members/", member, "/attributes"), query = params)
  r
}
