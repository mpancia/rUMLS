setGeneric("ui", function(c)
{
  standardGeneric("ui")
}
)

setMethod("ui", signature(c = "Concept"), function(c)
{
  c@cui
}
)
setMethod("ui", signature(c = "Atom"), function(c)
{
  c@aui
}
)
setMethod("ui", signature(c = "Relation"), function(c)
{
  c@rui
}
)

setGeneric("synonyms", function(c)
{
 standardGeneric("synonyms")
}
)

setMethod("synonyms", signature(c = "Concept"), function(c)
{
  atoms <- c@atoms
  unique(sapply(atoms, function(atom) attr(atom, "name")))
}
)

setGeneric("relations", function(c)
{
  standardGeneric("relations")
}
)

setMethod("relations", signature(c = "Concept"), function(c)
{
 c@relations
}
)

setGeneric("headui", function(c)
{
  standardGeneric("headui")
})

setMethod("headui", signature(c = "Relation"), function(c)
{
  c@headui
}
)

setGeneric("tailui", function(c)
{
  standardGeneric("tailui")
}
)
setMethod("tailui", signature(c = "Relation"), function(c)
{
  c@tailui
}
)
