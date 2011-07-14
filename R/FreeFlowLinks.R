setClass("FreeFlowLinks", 
	representation = representation(
		traffic.volume = "character",
		emission.factor = "character",
		classification = "character",
		width = "character",
		height = "character"), 
	contains = "SpatialSegmentsDataFrame")

setGeneric("FreeFlowLinks", function(...) standardGeneric("FreeFlowLinks"))
setMethod("FreeFlowLinks", "ANY", function(...) {
	new("FreeFlowLinks", ...)
})

as.data.frame.FreeFlowLinks <- function(from) {
	ssdf <- as(from, "SpatialSegmentsDataFrame")
	dat <- as(ssdf, "data.frame")
	names(dat)[1:4] <- c("XL1","YL1","XL2","YL2") # FIXME: hack
	dat
}
setAs(from="FreeFlowLinks", to="data.frame", as.data.frame.FreeFlowLinks)
