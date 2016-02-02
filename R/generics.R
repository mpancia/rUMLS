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


#' @export
#'
#' @examples
setGeneric("synonyms", function(x)
{
 standardGeneric("synonyms")
}
)




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

#' @export
#'
#' @examples
setGeneric("headui", function(x)
{
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
setGeneric("tailui", function(x)
{
  standardGeneric("tailui")
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


#' @export
#'
#' @examples
setGeneric("get_concept", function(x)
{
 standardGeneric("get_concept")
}
)


#' @export
#'
#' @examples
setGeneric("neighborhood", function(x)
{
  standardGeneric("neighborhood")
}
)

#' @export
setGeneric("source_vocab", function(x)
{
  standardGeneric("source_vocab")
}
)
