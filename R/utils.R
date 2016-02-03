authBaseURL <- "https://utslogin.nlm.nih.gov"
authEndpoint <- "/cas/v1/tickets"
restBaseURL <- "https://uts-ws.nlm.nih.gov/"

#' @importFrom stringr str_split
exhaust_search <- function(FUN = searchFunction, PARSER = parseFunction, ...) {
    results <- list()
    curPage <- 1
    keepSearching <- TRUE
    while (keepSearching == TRUE) {
        curResult <- PARSER(FUN(..., pageNumber = curPage))
        if (!is.null(curResult)) {
            results <- c(results, list(curResult))
            curPage <- curPage + 1
        } else {
            keepSearching <- FALSE
        }
    }
    unlist(results, recursive = F)
}

parse_results <- function(result) {
    resContent <- content(result)
    results <- resContent$result
    if (length(results) == 0) {
        NULL
    } else {
        results
    }
}

parse_atoms <- function(result) {
    initialParse <- parse_results(result)
    if (!is.null(initialParse)) {
        parsed <- lapply(initialParse, parse_atom)
    } else {
        parsed <- initialParse
    }
    parsed
}

parse_atom <- function(atom) {
    aui <- atom$ui
    suppressible <- as.logical(atom$suppressible)
    obsolete <- as.logical(atom$obsolete)
    rootSource <- atom$rootSource
    termType <- atom$termType
    codeURL <- atom$code
    conceptURL <- atom$concept
    sourceConceptURL <- atom$sourceConcept
    if (atom$sourceDescriptor == "NONE") {
        sourceDescriptor <- NULL
    } else {
        sourceDescriptor <- atom$sourceDescriptor
    }
    if (atom$attributes == "NONE") {
        attributesURL <- NULL
    } else {
        attributesURL <- atom$attributes
    }
    if (atom$parents == "NONE") {
        parentsURL <- NULL
    } else {
        parentsURL <- atom$parents
    }
    if (atom$children == "NONE") {
        childrenURL <- NULL
    } else {
        childrenURL <- atom$children
    }
    if (atom$relations == "NONE") {
        relationsURL <- NULL
    } else {
        relationsURL <- atom$relations
    }
    if (atom$definitions == "NONE") {
        definitionsURL <- NULL
    } else {
        definitionsURL <- atom$definitions
    }
    name <- atom$name
    language <- atom$language
    
    parsed <- new("Atom", name = name, language = language, definitionsURL = definitionsURL, 
        relations = relationsURL, childrenURL = childrenURL, parentsURL = parentsURL, 
        attributesURL = attributesURL, sourceDescriptor = sourceDescriptor, sourceConceptURL = sourceConceptURL, 
        conceptURL = conceptURL, codeURL = codeURL, termType = termType, suppressible = suppressible, 
        rootSource = rootSource, aui = aui)
    parsed
}

parse_rels <- function(result) {
    initialParse <- parse_results(result)
    if (!is.null(initialParse)) {
        parsed <- lapply(initialParse, parse_rel)
    } else {
        parsed <- initialParse
    }
    parsed
}

parse_rel <- function(rel) {
    rui <- rel$ui
    suppressible <- as.logical(rel$suppressible)
    sourceui <- rel$sourceui
    obsolete <- as.logical(rel$obsolete)
    sourceOriginated <- as.logical(rel$sourceOriginated)
    rootSource <- rel$rootSource
    relationLabel <- rel$relationLabel
    if (rel$groupId == "NONE") {
        groupId <- NULL
    } else {
        groupId <- rel$groupId
    }
    if (rel$additionalRelationLabel == "") {
        additionalRelationLabel <- NULL
    } else {
        additionalRelationLabel <- rel$additionalRelationLabel
    }
    attributeCount <- as.numeric(rel$attributeCount)
    classStr <- rel$classType
    if (classStr == "ConceptRelation") {
        relatedConceptURL <- rel$relatedConcept
        tailui <- tail(str_split(relatedConceptURL, "/")[[1]], 1)
        relObj <- new("ConceptRelation", rui = rui, suppressible = suppressible, 
            sourceui = sourceui, obsolete = obsolete, sourceOriginated = sourceOriginated, 
            rootSource = rootSource, relationLabel = relationLabel, groupId = groupId, 
            relatedConceptURL = relatedConceptURL, additionalRelationLabel = additionalRelationLabel, 
            attributeCount = attributeCount, tailui = tailui, headui = NULL)
    } else {
        relatedAtomURL <- rel$relatedAtom
        tailui <- tail(str_split(relatedAtomURL, "/")[[1]], 1)
        relObj <- new("AtomRelation", rui = rui, suppressible = suppressible, sourceui = sourceui, 
            obsolete = obsolete, sourceOriginated = sourceOriginated, rootSource = rootSource, 
            relationLabel = relationLabel, groupId = groupId, relatedAtomURL = relatedAtomURL, 
            additionalRelationLabel = additionalRelationLabel, attributeCount = attributeCount, 
            tailui = tailui, headui = NULL)
    }
    relObj
} 
