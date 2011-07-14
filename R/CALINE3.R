require(plume)

# TODO: move these into 'plume'
setClass("Meteorology", contains = "data.frame")
setClass("Concentrations", contains = "matrix")

# Native code wrapper
.CALINE3.Fortran <- function(receptors, links, meteorology, ...) {
	message("Entering CALINE3.Fortran")
	conc <- Concentrations(NA)
	conc	
}