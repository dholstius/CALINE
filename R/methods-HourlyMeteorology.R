setGeneric("HourlyMeteorology", function(...) standardGeneric("HourlyMeteorology"))

setMethod("HourlyMeteorology", "ANY", function(...) {
	new("HourlyMeteorology", data.frame(...))
})