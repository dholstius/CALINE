require(plume)

setClass("HourlyMeteorology", 
	contains = "Conditions"
)

setClass("Pollutant", 
	representation(
		label = "character",
		molecularWeight = "numeric",
		settlingVelocity = "numeric",
		depositionVelocity = "numeric"
	),
	prototype(
		settlingVelocity = 0.0,
		depositionVelocity = 0.0
	)
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
		pollutant = "Pollutant",
		surfaceRoughness = "numeric",
		averagingTime = "numeric"
	),
	prototype(
		pollutant = Pollutant("CO"),
		surfaceRoughness = 300.0,
		averagingTime = 60.0,
	),
	contains = "DispersionModel"
)