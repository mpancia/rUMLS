setClassUnion("charOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClass("Atom", representation(aui = "character", suppressible = "logical", rootSource = "character", termType = "character", codeURL = "character", 
    conceptURL = "character", sourceConceptURL = "charOrNULL", sourceDescriptor = "charOrNULL", attributesURL = "charOrNULL", parentsURL = "charOrNULL", 
    childrenURL = "charOrNULL", relations = "charOrNULL", name = "character", language = "character", definitionsURL = "charOrNULL"))

print.Atom <- function(x, ..., n = NULL, width = NULL) {
    cat(x@name, x@aui, x@rootSource, sep = "\t")
}

setClass("Concept", representation(cui = "character", suppressible = "logical", dateAdded = "Date", majorRevisionDate = "Date", status = "character", 
    atomCount = "numeric", attributeCount = "numeric", cvMemberCount = "numeric", atomsURL = "character", definitionsURL = "charOrNULL", relationsURL = "character", 
    semanticTypes = "list", preferredAtom = "Atom", relationCount = "numeric", name = "character", atoms = "listOrNULL", relations = "listOrNULL"))

print.Concept <- function(x, ..., n = NULL, width = NULL) {
    cat(x@name, x@cui, sep = "\t")
    cat("\n")
    cat("Semantic Types")
    cat("\n")
    cat(paste0(rep("-", 40)))
    cat("\n")
    cat(paste(x@semanticTypes, collapse = "\n"))
    cat("\n")
    cat(paste0(rep("-", 40)))
    cat("\n")
    cat("Number of Atoms:", x@atomCount)
    cat("\n")
    cat("Number of Relations:", x@relationCount)
}

setClass("Relation", representation(rui = "character", headui = "charOrNULL", tailui = "charOrNULL", suppressible = "logical", sourceui = "charOrNULL", 
    obsolete = "logical", sourceOriginated = "logical", rootSource = "character", relationLabel = "character", additionalRelationLabel = "charOrNULL", 
    groupId = "charOrNULL", attributeCount = "numeric"))

print.Relation <- function(x, ..., n = NULL, width = NULL) {
    cat("RUI:", x@rui)
    cat("\n")
    cat("Source":x@rootSource)
    cat("\n")
    cat("Relation Label:", x@relationLabel)
    cat("\n")
    cat("Additional Relation Label:", x@additionalRelationLabel)
    
    invisible(x)
}

setClass("ConceptRelation", representation(relatedConceptURL = "character"), contains = "Relation")

setClass("AtomRelation", representation(relatedAtomURL = "character"), contains = "Relation") 
