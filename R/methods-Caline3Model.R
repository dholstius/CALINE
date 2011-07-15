require(plume)

# Native code wrapper
.CALINE3.Fortran <- function(receptors, links, meteorology, 
		settling.velocity = 0.0,
		deposition.velocity = 0.0,
		...) {
	message("Entering CALINE3.Fortran")
	NULL	
}

.CALINE3.receptors <- function(object, ...) {
	message("Entering CALINE3.receptors")
	.CALINE3.Fortran(receptors=object, ...)
}

setGeneric("predict")
setMethod("predict", signature(object="Receptors"), .CALINE3.receptors)