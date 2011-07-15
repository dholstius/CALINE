setGeneric("Meteorology", function(...) standardGeneric("Meteorology"))

setMethod("Meteorology", "ANY", function(...) {
	new("Meteorology", data.frame(...))
})