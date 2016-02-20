setClassUnion("charOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))

#' An S4 class to represent atoms.
#'
#' An Atom has slots for the output from the UMLS API, as specified by
#'
#' @slot aui
#' @slot suppressible
#' @slot rootSource
#' @slot termType
#' @slot codeURL
#' @slot conceptURL
#' @slot sourceConceptURL
#' @slot sourceDescriptor
#' @slot attributesURL
#' @slot parentsURL
#' @slot childrenURL
#' @slot relations
#' @slot name
#' @slot language
#' @slot definitionsURL
setClass("Atom", representation(aui = "character", suppressible = "logical", rootSource = "character", termType = "character", codeURL = "character", 
    conceptURL = "character", sourceConceptURL = "charOrNULL", sourceDescriptor = "charOrNULL", attributesURL = "charOrNULL", parentsURL = "charOrNULL", 
    childrenURL = "charOrNULL", relations = "charOrNULL", name = "character", language = "character", definitionsURL = "charOrNULL"))

print.Atom <- function(x, ..., n = NULL, width = NULL) {
    cat(x@name, x@aui, x@rootSource, sep = "\t")
}

#' An S4 class to represent concepts.
#'
#' A Concept has slots for the output from the UMLS API, as specified by
#'
#' @slot cui
#' @slot suppressible
#' @slot dateAdded
#' @slot majorRevisionDate
#' @slot status
#' @slot atomCount
#' @slot attributeCount
#' @slot cvMemberCount
#' @slot atomsURL
#' @slot definitionsURL
#' @slot relationsURL
#' @slot semanticTypes
#' @slot preferredAtom
#' @slot relationCount
#' @slot name
#' @slot atoms
#' @slot relations
setClass("Concept", representation(cui = "character", suppressible = "logical", dateAdded = "Date", majorRevisionDate = "Date", status = "character", 
    atomCount = "numeric", attributeCount = "numeric", cvMemberCount = "numeric", atomsURL = "character", definitionsURL = "charOrNULL", relationsURL = "character", 
    semanticTypes = "list", preferredAtom = "Atom", relationCount = "numeric", name = "character", atoms = "listOrNULL", relations = "listOrNULL"))


#' An S4 class to represent relations.
#'
#' A relation has slots for the output from the UMLS API, as specified by
#'
#' @slot rui
#' @slot headui
#' @slot tailui
#' @slot suppressible
#' @slot sourceui
setClass("Relation", representation(rui = "character", headui = "charOrNULL", tailui = "charOrNULL", suppressible = "logical", sourceui = "charOrNULL", 
    obsolete = "logical", sourceOriginated = "logical", rootSource = "character", relationLabel = "character", additionalRelationLabel = "charOrNULL", 
    groupId = "charOrNULL", attributeCount = "numeric"))

#' An S4 class to represent concept relations.
setClass("ConceptRelation", representation(relatedConceptURL = "character"), contains = "Relation")

#' An S4 class to represent atom relations.
setClass("AtomRelation", representation(relatedAtomURL = "character"), contains = "Relation") 
