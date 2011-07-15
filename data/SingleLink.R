.mat <- matrix(c(0, -5000, 0, 5000), ncol=4)
.attr <- data.frame(TYP='AG', VPH=3000, W=30, H=0, EF=30)
row.names(.attr) <- row.names(.mat) <- "LINK A"
.geom <- SpatialSegments(.mat)
.sldf <- SpatialLinesDataFrame(.geom, .attr)

require(CALINE)
links <- FreeFlowLinks(.sldf)
receptors <- Receptors(x=30, y=0, z=1.8)
meteorology <- Meteorology(
	U = 1.0,
	BRG = 270.0,
	CLAS = 6,
	MIXH = 1000.0
)

ambientConcentration <- 3.0
averagingTime <- 60.0
surfaceRoughness <- 10.0
settlingVelocity <- 0.0
depositionVelocity <- 0.0