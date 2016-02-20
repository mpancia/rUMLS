#' Get unique identifier.
#'
#' @export
setGeneric("ui", function(x) {
    standardGeneric("ui")
})

#' Get descriptors.
#'
#' @export
setGeneric("descr", function(x) {
  standardGeneric("descr")
})

setMethod("descr", signature(x = "list"), function(x) {
  lapply(x, descr)
})


#' @export
#'
#' @examples
setGeneric("synonyms", function(x, ...) {
    standardGeneric("synonyms")
})




#' @export
#'
#' @examples
setGeneric("relations", function(x) {
    standardGeneric("relations")
})


#' @export
#'
#' @examples
setGeneric("headui", function(x) {
    standardGeneric("headui")
})



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("tailui", function(x) {
    standardGeneric("tailui")
})

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("related", function(x) {
    standardGeneric("related")
})


#' @export
#'
#' @examples
setGeneric("get_concept", function(x, info_ret = "none") {
    standardGeneric("get_concept")
})


#' @export
#'
#' @examples
setGeneric("neighborhood", function(x) {
    standardGeneric("neighborhood")
})

#' @export
setGeneric("source_vocab", function(x) {
    standardGeneric("source_vocab")
})

#' Get labels for relationship.
#'
#' @param rel
#' @param type
#'
#' @return
#' @export
#'
setGeneric("relation_label", function(rel, type) {
    standardGeneric("relation_label")
})
