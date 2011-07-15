require(CALINE)

.mat <- matrix(c(0, -5000, 0, 5000), ncol=4)
.attr <- data.frame(TYP='AG', VPH=3000, W=30, H=0, EF=30)
row.names(.attr) <- row.names(.mat) <- "LINK A"
.geom <- SpatialSegments(.mat)
.linkdata <- SpatialLinesDataFrame(.geom, .attr)

SingleLink <- Caline3Model(
	Receptors(x=30, y=0, z=1.8),
	FreeFlowLinks(.linkdata),
	HourlyMeteorology(
		U = 1.0,
		BRG = 270.0,
		CLAS = "F",
		MIXH = 1000.0
	),
	pollutant = "CO",
	surfaceRoughness = 10.0
)