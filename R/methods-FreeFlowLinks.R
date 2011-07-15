##############################################################################
#
#  Factory methods
#
##############################################################################

setGeneric("FreeFlowLinks", function(...) standardGeneric("FreeFlowLinks"))

setMethod("FreeFlowLinks", "ANY", function(...) {
	new("FreeFlowLinks", ...)
})

##############################################################################
#
#  Coercion methods
#
##############################################################################

setAs(from="FreeFlowLinks", to="data.frame", function(from) {
	ssdf <- as(from, "SpatialSegmentsDataFrame")
	dat <- as(ssdf, "data.frame")
	names(dat)[1:4] <- c("XL1","YL1","XL2","YL2") # FIXME: hack
	dat
})
