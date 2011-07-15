setGeneric("Pollutant", function(label, ...) standardGeneric("Pollutant"))

setMethod("Pollutant", 
	signature(label="character"), 
	function(label, ...) {
		new("Pollutant", label=label, ...)
	}
)

setMethod("initialize", "Pollutant",
	function(.Object, label, ...) {
		if(label == "CO") {
			molecularWeight <- 28.0
		}
		callNextMethod(.Object, ..., label=label, molecularWeight=molecularWeight)
	}
)
		