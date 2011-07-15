require(plume)

setClass("HourlyMeteorology", 
	contains = "Conditions"
)

setClass("FreeFlowLinks", 
	representation(
		traffic.volume = "character",
		emission.factor = "character",
		classification = "character",
		width = "character",
		height = "character"
	), 
	contains = c("Sources", "SpatialSegmentsDataFrame")
)

setClass("Caline3Model",
	representation(
		receptors = "Receptors",
		sources = "FreeFlowLinks",
		conditions = "HourlyMeteorology",
		pollutant = "character",
		surfaceRoughness = "numeric",
		averagingTime = "numeric",
		settlingVelocity = "numeric",
		depositionVelocity = "numeric"
	),
	prototype(
		pollutant = "CO",
		surfaceRoughness = 300.0,
		averagingTime = 60.0,
		settlingVelocity = 0.0,
		depositionVelocity = 0.0
	),
	contains = "DispersionModel"
)