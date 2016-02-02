#' @include generics.R

setMethod("tailui", signature(x = "Relation"), function(x)
{
  x@tailui
}
)

setMethod("headui", signature(x = "Relation"), function(x)
{
  x@headui
}
)

setMethod("source_vocab", signature(x = "Relation"), function(x)
{
  x@rootSource
}
)

#' Get a relation label.
#'
#' @param rel
#' @param type
#'
#' @return
#' @export
#'
#' @examples
relation_label <- function(rel, type = "primary")
{
  if(type == "primary")
  {
    rel@relationLabel
  }
  else
  {
    rel@additionalRelationLabel
  }
}

#' Convert a list of relations to an edgelist.
#'
#' @param rels
#'
#' @return
#' @export
#'
#' @examples
as.edge.df <- function(rels)
{
  tails <- sapply(rels, tailui)
  heads <- sapply(rels, headui)
  sources <- sapply(rels, source_vocab)
  relation_labels <- sapply(rels, relation_label)
  data.frame(tails = tails, heads = heads, sources = sources, relation_labels = relation_labels)
}

rels.as.graph <- function(rels)
{
  edge_df <- as.edge.df(rels)
  graph_from_data_frame(edge_df)
}
