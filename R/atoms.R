#' @include generics.R
#' @include class_defs.R
NULL
setMethod("source_vocab", signature = (x = "Atom"), function(x) {
    x@rootSource
})

setMethod("ui", signature(x = "Atom"), function(x) {
    x@aui
}) 
