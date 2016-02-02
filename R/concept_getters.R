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
  exhaust_search(FUN = get_concept_rels_page, PARSER = parse_rels, CUI = CUI)
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
  params = list(ticket = get_service_ticket(get_TGT()), sabs = sabs)
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/definitions"), query = params)
  parse_results(r)
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
  exhaust_search(FUN = get_concept_atoms_page, PARSER = parse_atoms, CUI = CUI, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
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
  params = list(ticket = get_service_ticket(get_TGT()))
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI), query = params)
  parse_results(r)
}


#' Get Concept
#'
#' @param CUI
#'
#' @return
#' @export
#'
get_concept <- function(CUI) {
  info <- get_concept_info(CUI)
  semanticTypes <- info$semanticTypes
  suppressible <- info$suppressible
  dateAdded <- as.Date(info$dateAdded, format = '%m-%d-%y')
  majorRevisionDate <- as.Date(info$majorRevisionDate, format = '%m-%d-%y')
  status <- info$status
  atomCount <- as.numeric(info$atomCount)
  attributeCount <- as.numeric(info$attributeCount)
  cvMemberCount <- as.numeric(info$cvMemberCount)
  atomsURL <- info$atoms
  if(info$definitions == "NONE")
  {
    definitions <- NULL
  } else
  {
    definitions <- info$definitions
  }
  relationsURL <- info$relations
  preferredAtom <- get_pref_atom(CUI)
  relationCount <- as.numeric(info$relationCount)
  name <- info$name
  rels <- get_concept_rels(CUI)
  atoms <- get_concept_atoms(CUI)
  rels <- sapply(rels, function(rel) {attr(rel, "headui") <- CUI; rel} )
  concept <- new("Concept", cui = CUI, suppressible = suppressible, dateAdded = dateAdded,
                 majorRevisionDate = majorRevisionDate, status = status, atomCount = atomCount,
                 attributeCount = attributeCount, cvMemberCount = cvMemberCount, atomsURL = atomsURL,
                 definitionsURL = definitions, relationsURL = relationsURL, semanticTypes= semanticTypes,
                 preferredAtom = preferredAtom, relationCount = relationCount, name = name,
                 relations = rels,  atoms = atoms)
}

get_pref_atom <- function(CUI)
{
  params = list(ticket = get_service_ticket(get_TGT()))
  r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms/preferred"), query = params)
  atom_raw <- content(r)$result
  atom <- parse_atom(atom_raw)
  atom
}
