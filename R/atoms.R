#' Get familial relationships for a given Atom.
#'
#' @param AUI The AUI of interest.
#' @param type The type of familial relationship. This can be one of \code{ancestors, descendants, parents, children}. See the possible endpoints from the \href{https://documentation.uts.nlm.nih.gov/rest/atoms/}{UMLS}.
#' @return A list of results. These are of UMLS class \code{Atom}.
#' @export
#' @examples
#' # Get parents of atom A8345234
#' parents <- get_atom_parents("A8345234")
get_atom_family <- function(AUI, type) {
  exhaust_search(FUN = get_atom_family_page, PARSER = parse_results, AUI = AUI, type = type)
}

get_atom_family_page <- function(AUI, type, pageNumber = 1, pageSize = 25) {
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/AUI/", AUI, "/", type), query = params)
  r
}

#' @rdname get_atom_family
get_atom_parents <- function(AUI){
  get_atom_family(AUI, "parents")
}

#' @rdname get_atom_family
get_atom_children <- function(AUI){
  get_atom_family(AUI, "children")
}

#' @rdname get_atom_family
get_atom_ancestors <- function(AUI){
  get_atom_family(AUI, "ancestors")
}

#' @rdname get_atom_family
get_atom_descendants <- function(AUI){
  get_atom_family(AUI, "descendants")
}

#' Get information about a given atom.
#' @param AUI The AUI of interest.
#' @return Information about the atom. This is of UMLS class \code{Atom}.
#' @export
#' @examples
#' # Get information about atom A8345234
#' info <- get_atom_info("A8345234")
get_atom_info <- function(AUI){
  params = list(ticket = get_service_ticket(get_TGT()))
  r <- GET(restBaseURL, path = paste0("rest/content/current/AUI/", AUI), query = params)
  r
}
