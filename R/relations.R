#' @include generics.R
#' @include class_defs.R
NULL



setMethod("ui", signature(x = "Relation"), function(x) {
    x@rui
})

setMethod("tailui", signature(x = "Relation"), function(x) {
    x@tailui
})

setMethod("tailui", signature(x = "list"), function(x) {
    sapply(x, tailui)
})

setMethod("headui", signature(x = "Relation"), function(x) {
    x@headui
})

setMethod("headui", signature(x = "list"), function(x) {
    sapply(x, headui)
})

setMethod("source_vocab", signature(x = "Relation"), function(x) {
    x@rootSource
})

setMethod("source_vocab", signature(x = "list"), function(x) {
    sapply(x, source_vocab)
})

#' Get a relation label.
#'
#' @param rel
#' @param type
#'
#' @return
#' @export
#'
#' @examples
setMethod("relation_label", signature(rel = "Relation"), function(rel, type = "primary") {
    if (type == "primary") {
        rel@relationLabel
    } else {
        rel@additionalRelationLabel
    }
})

setMethod("relation_label", signature(rel = "list"), function(rel, type = "primary") {
    sapply(rel, relation_label, type = type)
})

#' Convert a list of relations to an edgelist.
#'
#' @param rels
#'
#' @return
#' @export
#'
#' @examples
as.edge.df <- function(rels) {
    tails <- sapply(rels, tailui)
    heads <- sapply(rels, headui)
    sources <- sapply(rels, source_vocab)
    relation_labels <- sapply(rels, relation_label)
    data.frame(tails = tails, heads = heads, sources = sources, relation_labels = relation_labels)
}

rels.as.graph <- function(rels) {
    edge_df <- as.edge.df(rels)
    graph_from_data_frame(edge_df)
} 
