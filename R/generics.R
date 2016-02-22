#' Get unique identifier.
#'
#' @export
#' @return The unique identifier for an object, or a list of such things.
setGeneric("ui", function(x) {
    standardGeneric("ui")
})

# Vectorize UI.
setMethod("ui", signature(x = "list"), function(x) {
  lapply(x, ui)
})

#' Get descriptors.
#'
#' @export
setGeneric("descr", function(x) {
    standardGeneric("descr")
})

# Vectorize descriptors.
setMethod("descr", signature(x = "list"), function(x) {
    lapply(x, descr)
})


#' Get the synonyms for an object.
#'
#' @export
#' @return A list of synonyms.
setGeneric("synonyms", function(x, ...) {
    standardGeneric("synonyms")
})



#' Get the relations associated to an object.
#'
#' @export
#' @return A \linkS4class{Relation} object or list of such things.
setGeneric("relations", function(x) {
    standardGeneric("relations")
})

#' Get the head UI for a relation.
#'
#' @export
#' @return The UI of the target of a relationship, or a list of such things.
setGeneric("headui", function(x) {
    standardGeneric("headui")
})



#' Get the tail UI for a relation.
#'
#' @return The UI of the source of a relationship, or a list of such things.
#' @export
setGeneric("tailui", function(x) {
    standardGeneric("tailui")
})


#' @export
setGeneric("related", function(x) {
    standardGeneric("related")
})


#' Get UMLS Concept.
#'
#' This retrieves a UMLS Concept, as identified by a CUI.
#'
#' @param CUI A CUI or list of CUIs.
#'
#' @return A \code{\linkS4class{Concept}} or list of \code{\linkS4class{Concept}} objects.
#' @export
#'
setGeneric("get_concept", function(CUI, info_ret = "none") {
    standardGeneric("get_concept")
})


#' @export
#'
#' @examples
setGeneric("neighborhood", function(x) {
    standardGeneric("neighborhood")
})

#' Get the source vocabulary for an object.
#'
#' @export
#' @return The source vocabulary for an object, or a list of such things.
setGeneric("source_vocab", function(x) {
    standardGeneric("source_vocab")
})

#' Get labels for relationship.
#'
#' Retrieves the labels for a relationship \code{rel}. Vectorized over \code{rel}.
#'
#' @param rel The input relation.
#' @param type The type of relation.
#'
#' @return A label for the relation or a list of such things.
#' @export
#'
setGeneric("relation_label", function(rel, type) {
    standardGeneric("relation_label")
})

#'  Get UMLS concept definitions.
#'
#' Retrieves the definitions of a given \linkS4class{Concept}, as per \href{https://documentation.uts.nlm.nih.gov/rest/definitions/}{the NLM}. This is vectorized over the input.
#'
#' @param CUI The input concept, CUI, or list of such things.
#' @param sab List of source vocabularies, comma separated.
#' @return A \linkS4class{Definition} object or a list of \linkS4class{Definition} objects.
#' @export
#'
setGeneric("definitions", function(CUI, sabs) {
  standardGeneric("definitions")
})
