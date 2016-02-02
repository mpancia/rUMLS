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
