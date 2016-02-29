#' @include generics.R
#' @include class_defs.R
NULL

# Set generics.

setMethod("source_vocab", signature = (x = "Atom"), function(x) {
    x@rootSource
})

setMethod("ui", signature(x = "Atom"), function(x) {
    x@aui
})

setMethod("descr", signature(x = "Atom"), function(x) {
    x@name
})

#' @export
get_atom_family <- function(AUI, type) {
  exhaust_search(FUN = get_atom_family_page, PARSER = parse_atoms, AUI = AUI, type = type)
}

get_atom_family_page <- function(AUI, type, pageNumber = 1, pageSize = 25) {
  params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
  r <- GET(restBaseURL, path = paste0("rest/content/current/AUI/", AUI, "/", type), query = params)
  r
}

get_atom_info <- function(AUI){
  params = list(ticket = get_service_ticket(get_TGT()))
  r <- GET(restBaseURL, path = paste0("rest/content/current/AUI/", AUI), query = params)
  r
}
