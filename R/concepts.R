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

parse_rels <- function(result){
  initialParse <- result_parser(result)
  if(!is.null(initialParse))
  {
    parsed <- lapply(initialParse, parse_rel)
  }
  else
  {
    parsed <- initialParse
  }
  parsed
}

parse_rel <- function(rel){
  rui <- rel$ui
  suppressible <- as.logical(rel$suppressible)
  sourceui <- rel$sourceui
  obsolete <- as.logical(rel$obsolete)
  sourceOriginated <- as.logical(rel$sourceOriginated)
  rootSource <- rel$rootSource
  relationLabel <- rel$relationLabel
  if(rel$groupId == "NONE")
  {
    groupId <- NULL
  } else
  {
    groupId <- rel$groupId
  }
  if(rel$additionalRelationLabel == "")
  {
    additionalRelationLabel <- NULL
  } else
  {
    additionalRelationLabel <- rel$additionalRelationLabel
  }
  attributeCount <- as.numeric(rel$attributeCount)
  classStr <- rel$classType
  if(classStr == "ConceptRelation")
  {
    relatedConceptURL <- rel$relatedConcept
    relObj <- new("ConceptRelation", rui = rui, suppressible = suppressible, sourceui = sourceui,
                   obsolete = obsolete, sourceOriginated = sourceOriginated, rootSource = rootSource,
                   relationLabel = relationLabel, groupId = groupId, relatedConceptURL = relatedConceptURL
                   , additionalRelationLabel = additionalRelationLabel, attributeCount = attributeCount)
  } else
  {
    relatedAtomURL <- rel$relatedAtom
    relObj <- new("AtomRelation", rui = rui, suppressible = suppressible, sourceui = sourceui,
                   obsolete = obsolete, sourceOriginated = sourceOriginated, rootSource = rootSource,
                   relationLabel = relationLabel, groupId = groupId, relatedAtomURL = relatedAtomURL
                   , additionalRelationLabel = additionalRelationLabel, attributeCount = attributeCount)
  }
  relObj
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
  parse_atoms(r)
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

#' @rdname get_concept_atoms
result_parser <- function(result)
{
  resContent <- content(result)
  results <- resContent$result
  if(length(results) == 0)
  {
    NULL
  } else
  {
    results
  }
}

parse_atoms <- function(result)
{
  initialParse <- result_parser(result)
  if(!is.null(initialParse))
  {
    parsed <- lapply(initialParse, parse_atom)
  }
  else
  {
    parsed <- initialParse
  }
  parsed
}

parse_atom <- function(atom)
{
  aui <- atom$ui
  suppressible <- as.logical(atom$suppressible)
  obsolete <- as.logical(atom$obsolete)
  rootSource <- atom$rootSource
  termType <- atom$termType
  codeURL <- atom$code
  conceptURL <- atom$concept
  sourceConceptURL <- atom$sourceConcept
  if(atom$sourceDescriptor == "NONE")
  {
    sourceDescriptor <- NULL
  } else
  {
    sourceDescriptor <- atom$sourceDescriptor
  }
  if(atom$attributes == "NONE")
  {
    attributesURL <- NULL
  } else
  {
    attributesURL <- atom$attributes
  }
  if(atom$parents == "NONE")
  {
    parentsURL <- NULL
  } else
  {
    parentsURL <- atom$parents
  }
  if(atom$children == "NONE")
  {
    childrenURL <- NULL
  } else
  {
    childrenURL <- atom$children
  }
  if(atom$relations == "NONE")
  {
    relationsURL <- NULL
  } else
  {
    relationsURL <- atom$relations
  }
  if(atom$definitions == "NONE")
  {
    definitionsURL <- NULL
  } else
  {
    definitionsURL <- atom$definitions
  }
  name <- atom$name
  language <- atom$language
  parsed <- new("Atom", name = name, language = language, definitionsURL = definitionsURL,
                relations = relationsURL, childrenURL = childrenURL, parentsURL = parentsURL,
                attributesURL = attributesURL, sourceDescriptor = sourceDescriptor,
                sourceConceptURL = sourceConceptURL, conceptURL = conceptURL, codeURL = codeURL,
                termType = termType, suppressible = suppressible, rootSource = rootSource, aui = aui)
  parsed
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
  result_parser(r)
}

get_concept <- function(CUI)
{
  info <- get_concept_info(CUI)
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
  concept <- new("Concept", cui = CUI, suppressible = suppressible, dateAdded = dateAdded,
                 majorRevisionDate = majorRevisionDate, status = status, atomCount = atomCount,
                 attributeCount = attributeCount, cvMemberCount = cvMemberCount, atomsURL = atomsURL,
                 definitionsURL = definitionsURL, relationsURL = relationsURL,
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
