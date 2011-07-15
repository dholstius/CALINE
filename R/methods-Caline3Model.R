setGeneric("Caline3Model", 
	function(receptors, sources, conditions, ...) {
		standardGeneric("Caline3Model")
	}
)

setMethod("Caline3Model", 
	signature(
		receptors = "Receptors", 
		sources = "FreeFlowLinks", 
		conditions = "HourlyMeteorology"
	),
	function(receptors, sources, conditions, ...) {
		new("Caline3Model", 
			receptors = receptors, 
			sources = sources, 
			conditions = conditions, 
			...
		)
	}
)

# Native code wrapper
CALINE3.Fortran <- function(...) {
	message("Entering CALINE3.Fortran")
	message("Exiting CALINE3.Fortran")
	NULL	
}

setGeneric("predict")

setMethod("predict", signature(object="Caline3Model"), function(object, ...) {
	message("Entering predict() for Caline3Model")
	CALINE3.Fortran(receptors=object, ...)
	message("Exiting predict() for Caline3Model")
})