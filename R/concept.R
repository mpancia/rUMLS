#' @importFrom dplyr bind_rows
NULL

#' Get the Metathesaurus Concept-Concept relationships for a given CUI.
#'
#' @param CUI
#'
#' @return A list of Concept-Concept relationships. These are of UMLS class \code{ConceptRelation}.
#' @export
#'
#' @examples
#' # Get relationships for concept C0011884
#' relations <- get_concept_rels("C0011884")
get_concept_rels <- function(CUI) {
    exhaust_search(FUN = get_concept_rels_page, PARSER = parse_results, CUI = CUI)
}

get_concept_rels_page <- function(CUI, pageNumber = 1, pageSize = 25) {
    params = list(ticket = get_service_ticket(get_TGT()), pageNumber = pageNumber, pageSize = pageSize)
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/relations"), query = params)
    r
}

#' Get UMLS concept atoms.
#'
#' Retrieves the atoms that are associated to a given \linkS4class{Concept}.
#'
#' @param CUI CUI of concept to obtain atoms for.
#' @param sabs Source vocabularies, comma separated.
#' @param ttys Term types, any one of the \href{https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html#TTYC}{valid term types.}
#' @param language The language of interest.
#' @param includeObsolete Whether or not to include obsolete atoms.
#' @param includeSuppressible Whether or not to include suppressible atoms.
#' @return A list of Atoms, of UMLS class \code{Atom}.
#' @export
#' @examples
#' # Get atoms for concept C0011884
#' atoms <- get_concept_atoms("C0011884")
get_concept_atoms <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE) {
    exhaust_search(FUN = get_concept_atoms_page, PARSER = parse_results, CUI = CUI, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
        includeSuppressible = includeSuppressible)
}

#' @rdname get_concept_atoms
get_concept_atoms_page <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE, pageNumber = 1,
    pageSize = 25) {
    params = list(ticket = get_service_ticket(get_TGT()), sabs = sabs, pageNumber = pageNumber, pageSize = pageSize)
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms"), query = params)
    r
}

#' Get UMLS concept info.
#
#'
#' @param CUI CUI of interest.
#' @param language
#' @return Information about the CUI. This is of UMLS class \code{Concept}.
#' @export
#' @examples
#' # Get info for concept C0011884
#' info <- get_concept_info("C0011884")
get_concept_info <- function(CUI) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI), query = params)
    parse_results(r)
}

#' Get the preferred atom for a concept.
#'
#' @param CUI CUI of interest.
#'
#' @return The preferred atom of the CUI. This is of UMLS class \code{Atom}.
#' @export
#'
#' @examples
#' # Get the preferred atom of concept C0011884
#' prefAtom <- get_pref_atom("C0011884")
get_pref_atom <- function(CUI) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms/preferred"), query = params)
    parse_results(r)
}

#' Get the internal codes for atoms from a list of vocabularies.
#'
#' The source vocabularies in the UMLS have internal codes for the atoms. This will retrieve the \code{(code, source)} pairs that can be used to obtain internal information about the atoms.
#'
#' @param concept
#'
#' @return A list of `(code, source)` pairs.
#' @export
#'
#' @examples
#' # Get the atoms about concept C0011884
#' concept <- get_concept_atoms("C0011884")
#' codePairs <- get_internal_codes(concept)
get_internal_codes <- function(concept) {
    list.apply(concept , function(x) list(code = rev(str_split(x$code, "/")[[1]])[[1]], rootSource = x$rootSource))
}
