.onLoad <- function(lib=NULL, pkg=NULL) {
	if(is.null(pkg)) 
		pkg <- "CALINE"
	library.dynam("CALINE", pkg, lib)
	ver <- utils::packageDescription(pkg, field="Version")
	packageStartupMessage("This is ", pkg, " ", ver)
}
.onUnload <- function(lib) library.dynam.unload("CALINE", lib)
.onAttach <- function(lib, package) {}
