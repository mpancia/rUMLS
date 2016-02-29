setClassUnion("charOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))

#' An S4 class to represent atoms.
#'
#' An Atom has slots for the output from the UMLS API, as specified by \href{https://documentation.uts.nlm.nih.gov/rest/atoms/#sample-output}{the NLM.}
#'
#' @slot aui The atom identifier.
#' @slot suppressible Whether or not this atom is suppressible.
#' @slot rootSource The root source vocabulary of the atom.
#' @slot termType The type of term that the atom represents.
#' @slot codeURL The API URL for the atom's code.
#' @slot conceptURL The API URL for the atom's concept.
#' @slot sourceConceptURL The API URL for the atom's source concept.
#' @slot sourceDescriptor The descriptor from the source.
#' @slot attributesURL The API URL for the atom's source attributes.
#' @slot parentsURL The API URL for the atom's parents.
#' @slot childrenURL The API URL for the atom's parents.
#' @slot relations The source-asserted relations for the atom.
#' @slot name The source-asserted atom name.
#' @slot language The language of the atom.
#' @slot definitionsURL The API URL for the atom's definition.
setClass("Atom", representation(aui = "character", suppressible = "logical", rootSource = "character", termType = "character", codeURL = "character",
    conceptURL = "character", sourceConceptURL = "charOrNULL", sourceDescriptor = "charOrNULL", attributesURL = "charOrNULL", parentsURL = "charOrNULL",
    childrenURL = "charOrNULL", relations = "charOrNULL", name = "character", language = "character", definitionsURL = "charOrNULL"))

print.Atom <- function(x, ..., n = NULL, width = NULL) {
    cat(x@name, x@aui, x@rootSource, sep = "\t")
}

#' An S4 class to represent concepts.
#'
#' A Concept has slots for the output from the UMLS API, as specified by  \href{https://documentation.uts.nlm.nih.gov/rest/concept/#sample-output}{the NLM.}
#'
#' @slot cui The concept identifier.
#' @slot suppressible Whether or not the concept is suppressible.
#' @slot dateAdded The date the concept was added.
#' @slot majorRevisionDate The date of the last major revision.
#' @slot status The status of the concept.
#' @slot atomCount The number of atoms.
#' @slot attributeCount The number of attributes.
#' @slot cvMemberCount
#' @slot atomsURL The API URL for the concept atoms.
#' @slot definitionsURL The API URL for the atom's parents.
#' @slot relationsURL The API URL for the atom's parents.
#' @slot semanticTypes The associated \href{https://www.nlm.nih.gov/research/umls/META3_current_semantic_types.html}{semantic types} of the concept.
#' @slot preferredAtom The UMLS-asserted preferred \linkS4class{Atom}.
#' @slot relationCount The number of relations.
#' @slot name The preferred name of the concept.
#' @slot atoms The atoms associated to the concept. May be a list of \linkS4class{Atom} objects or empty, depending on whether or not the atoms have been retrieved.
#' @slot relations The relations between the concept and other UMLS entities. May be a list of \linkS4class{Relation} objects or empty, depending on whether or not the relations have been retrieved.
setClass("Concept", representation(cui = "character", suppressible = "logical", dateAdded = "Date", majorRevisionDate = "Date", status = "character",
    atomCount = "numeric", attributeCount = "numeric", cvMemberCount = "numeric", atomsURL = "character", definitionsURL = "charOrNULL", relationsURL = "character",
    semanticTypes = "list", preferredAtom = "Atom", relationCount = "numeric", name = "character", atoms = "ANY", relations = "listOrNULL"))

#' An S4 class to represent NLM-asserted relations.
#'
#' A relation has slots for the output from the UMLS API, as specified by \href{https://documentation.uts.nlm.nih.gov/rest/relations/}{the NLM.}
#'
#' @slot rui The relation identifier.
#' @slot headui The identifier of the head of the relation (target).
#' @slot tailui The identifier of the tail of the relation (source).
#' @slot suppressible Whether or not this relation is suppressible.
#' @slot sourceui The source identifier.
setClass("Relation", representation(rui = "character", headui = "charOrNULL", tailui = "charOrNULL", suppressible = "logical", sourceui = "charOrNULL",
    obsolete = "logical", sourceOriginated = "logical", rootSource = "character", relationLabel = "character", additionalRelationLabel = "charOrNULL",
    groupId = "charOrNULL", attributeCount = "numeric"))

#' An S4 class to represent concept relations.
#'
#' These are a subclass of \code{Relation.}
#' @slot rui The relation identifier.
#' @slot headui The identifier of the head of the relation (target).
#' @slot tailui The identifier of the tail of the relation (source).
#' @slot suppressible Whether or not this relation is suppressible.
#' @slot sourceui The source identifier.
#' @slot relatedConceptURL The API URL of the related concept.
setClass("ConceptRelation", representation(relatedConceptURL = "character"), contains = "Relation")

#' An S4 class to represent atom relations.
#'
#' These are a subclass of \code{Relation.}
#' @slot rui The relation identifier.
#' @slot headui The identifier of the head of the relation (target).
#' @slot tailui The identifier of the tail of the relation (source).
#' @slot suppressible Whether or not this relation is suppressible.
#' @slot sourceui The source identifier.
#' @slot relatedAtomURL The API URL of the related atom.
setClass("AtomRelation", representation(relatedAtomURL = "character"), contains = "Relation")

#' An S4 class to represent definitions.
#' @slot rootSource The source of the definition.
#' @slot sourceOriginated Whether or not the definition originated from the source.
#' @slot value The definition.
setClass("Definition", representation(sourceOriginated = "logical", rootSource = "character", value = "character"))
