#' @include generics.R
#' @include class_defs.R
NULL

# Set generic methods
setMethod("ui", signature(x = "Relation"), function(x) {
    x@rui
})

# Get the tail UI.
setMethod("tailui", signature(x = "Relation"), function(x) {
    x@tailui
})

# Vectorize tail UI.
setMethod("tailui", signature(x = "list"), function(x) {
    sapply(x, tailui)
})

# Get the head UI.
setMethod("headui", signature(x = "Relation"), function(x) {
    x@headui
})

# Vectorize head UI.
setMethod("headui", signature(x = "list"), function(x) {
    sapply(x, headui)
})

# Get source vocabulary.
setMethod("source_vocab", signature(x = "Relation"), function(x) {
    x@rootSource
})

# Vectorize source vocabulary.
setMethod("source_vocab", signature(x = "list"), function(x) {
    sapply(x, source_vocab)
})

# Get relation label.
setMethod("relation_label", signature(rel = "Relation"), function(rel, type = "primary") {
    if (type == "primary") {
        rel@relationLabel
    } else {
        rel@additionalRelationLabel
    }
})

# Vectorize relation label.
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

#' Print a relation.
#' @export
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
