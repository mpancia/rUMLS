#' Get unique identifier.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("ui", function(x)
{
  standardGeneric("ui")
}
)

#' Title
#'
#' @param x Concept.
#'
#' @return
#' @export
#'
#' @examples
setMethod("ui", signature(x = "Concept"), function(x)
{
  x@cui
}
)
setMethod("ui", signature(x = "Atom"), function(x)
{
  x@aui
}
)
setMethod("ui", signature(x = "Relation"), function(x)
{
  x@rui
}
)

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("synonyms", function(x)
{
 standardGeneric("synonyms")
}
)

setMethod("synonyms", signature(x = "Concept"), function(x)
{
  atoms <- x@atoms
  unique(sapply(atoms, function(atom) attr(atom, "name")))
}
)

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("relations", function(x)
{
  standardGeneric("relations")
}
)

setMethod("relations", signature(x = "Concept"), function(x)
{
 x@relations
}
)

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("headui", function(x)
{
  standardGeneric("headui")
})

setMethod("headui", signature(x = "Relation"), function(x)
{
  x@headui
}
)

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("tailui", function(x)
{
  standardGeneric("tailui")
}
)
setMethod("tailui", signature(x = "Relation"), function(x)
{
  x@tailui
}
)

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
setGeneric("related", function(x)
{
  standardGeneric("related")
}
)

setMethod("related", signature(x = "Concept"), function(x)
{
  relationships <- relations(x)
  otherTails <- sapply(relationships, headui)
  otherHeads <- sapply(relationships, tailui)
  otherUIs <- unique(c(otherTails, otherHeads))
  otherConcepts <- lapply(otherUIs, get_concept)
  otherConcepts
}
)
