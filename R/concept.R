#' @include generics.R
#' @include class_defs.R
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

#' Get UMLS concept definitions.
#
#'
#' @param CUI CUI of interest.
#' @param sabs Source vocabularies, comma separated.
#'
#' @return
#' @export
#'
get_concept_defs <- function(CUI, sabs = NULL) {
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
get_concept_atoms <- function(CUI, sabs = NULL, ttys = NULL, language = NULL, includeObsolete = FALSE, includeSuppressible = FALSE) {
    exhaust_search(FUN = get_concept_atoms_page, PARSER = parse_atoms, CUI = CUI, sabs = sabs, ttys = ttys, language = language, includeObsolete = includeObsolete,
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


#' Get Concept
#'
#' @param x
#'
#' @return
#' @export
#'
setMethod("get_concept", signature(x = "character"), function(x, info_ret = "none") {
    info <- get_concept_info(x)
    semanticTypes <- info$semanticTypes
    suppressible <- info$suppressible
    dateAdded <- as.Date(info$dateAdded, format = "%m-%d-%y")
    majorRevisionDate <- as.Date(info$majorRevisionDate, format = "%m-%d-%y")
    status <- info$status
    atomCount <- as.numeric(info$atomCount)
    attributeCount <- as.numeric(info$attributeCount)
    cvMemberCount <- as.numeric(info$cvMemberCount)
    atomsURL <- info$atoms
    if (info$definitions == "NONE") {
        definitions <- NULL
    } else {
        definitions <- info$definitions
    }
    relationsURL <- info$relations
    preferredAtom <- get_pref_atom(x)
    relationCount <- as.numeric(info$relationCount)
    name <- info$name
    if (info_ret == "none") {
        rels <- NULL
        atoms <- NULL
    } else if (info_ret == "atoms") {
        rels <- NULL
        atoms <- get_concept_atoms(x)
    } else if (info_ret == "rels") {
        atoms <- NULL
        rels <- get_concept_rels(x)
        rels <- sapply(rels, function(rel) {
            attr(rel, "headui") <- x
            rel
        })
    } else {
        atoms <- get_concept_atoms(x)
        rels <- get_concept_rels(x)
        rels <- sapply(rels, function(rel) {
            attr(rel, "headui") <- x
            rel
        })
    }

    concept <- new("Concept", cui = x, suppressible = suppressible, dateAdded = dateAdded, majorRevisionDate = majorRevisionDate, status = status,
        atomCount = atomCount, attributeCount = attributeCount, cvMemberCount = cvMemberCount, atomsURL = atomsURL, definitionsURL = definitions,
        relationsURL = relationsURL, semanticTypes = semanticTypes, preferredAtom = preferredAtom, relationCount = relationCount, name = name, relations = rels,
        atoms = atoms)
})


get_pref_atom <- function(CUI) {
    params = list(ticket = get_service_ticket(get_TGT()))
    r <- GET(restBaseURL, path = paste0("rest/content/current/CUI/", CUI, "/atoms/preferred"), query = params)
    atom_raw <- content(r)$result
    atom <- parse_atom(atom_raw)
    atom
}

#' Title
#'
#' @param x Concept.
#'
#' @return
#' @export
#'
#' @examples
setMethod("ui", signature(x = "Concept"), function(x) {
    x@cui
})

setMethod("related", signature(x = "Concept"), function(x) {
    relationships <- relations(x)
    otherTails <- sapply(relationships, headui)
    otherHeads <- sapply(relationships, tailui)
    otherUIs <- unique(c(otherTails, otherHeads))
    otherConcepts <- lapply(otherUIs, get_concept)
    otherConcepts
})

setMethod("relations", signature(x = "Concept"), function(x) {
    x@relations
})

setMethod("relations", signature(x = "list"), function(x) {
    lapply(x, relations)
})

setMethod("synonyms", signature(x = "Concept"), function(x, ...) {
    atoms <- x@atoms
    if(hasArg("language")){
      args <- list(...)
      language <- args$language
      atoms <- atoms[sapply(atoms, function(x) x@language == language)]
    }
    unique(sapply(atoms, function(atom) attr(atom, "name")))
})

setMethod("synonyms", signature(x = "list"), function(x, ...) {
    lapply(x, synonyms, ...)
})

setMethod("desc", signature(x = "Concept"), function(x) {
  x@name
})

setMethod("neighborhood", signature(x = "Concept"), function(x) {
    edges <- relations(x)
    other_node_ids <- sapply(edges, tailui)
    other_nodes <- lapply(other_node_ids, get_concept)
})


setMethod("neighborhood", signature(x = "character"), function(x) {
    conc <- get_concept(x)
    neighborhood(x)
})

setMethod("neighborhood", signature(x = "list"), function(x) {
    lapply(x, neighborhood)
})
diseases <- function(concept) {
    nbhd <- neighborhood(concept)

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
