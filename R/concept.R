#' @importFrom dplyr bind_rows
NULL
#' @export
#'
get_concept_rels <- function(CUI) {
    exhaust_search(FUN = get_concept_rels_page, PARSER = parse_rels, CUI = CUI)
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
#' @return A list of \linkS4class{Atom} objects.
#' @export
#'
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
get_concept_info <- function(CUI) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI), query = params)
    parse_results(r)
}

get_pref_atom <- function(CUI) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms/preferred"), query = params)
    atom_raw <- content(r)$result
    atom <- parse_atom(atom_raw)
    atom
}

#' Get the codes for atoms from a list of vocabularies.
#'
#' @param concept
#' @param vocab_list
#'
#' @return A list of `(code, source)` pairs.
#' @export
#'
#' @examples
codes <- function(concept, vocab_list) {
    ats <- concept@atoms
    right_atoms <- ats[sapply(ats, source_vocab) %in% vocab_list]
    codes <- lapply(right_atoms, function(y) list(code = rev(str_split(y@codeURL, "/")[[1]])[[1]], source = source_vocab(y)))
    codes
}
