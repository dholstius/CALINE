message("Testing Caline3Model", appendLF=FALSE)

require(CALINE)

.mat <- matrix(c(0, -5000, 0, 5000), ncol=4)
.attr <- data.frame(TYP='AG', VPH=3000, W=30, H=0, EF=30)
row.names(.attr) <- row.names(.mat) <- "LINK A"
.geom <- SpatialSegments(.mat)
.sldf <- SpatialLinesDataFrame(.geom, .attr)

rcp <- Receptors(x=30, y=0, z=1.8)
lnk <- FreeFlowLinks(.sldf)
met <- HourlyMeteorology(U=1.0, BRG=270.0, CLAS="F", MIXH=1000.0)

mod <- Caline3Model(rcp, lnk, met)
expect_equal(mod@surfaceRoughness, 300.0)

p <- mod@pollutant
expect_equal(p@label, "CO")
expect_equal(p@settlingVelocity, 0.0)
expect_equal(p@depositionVelocity, 0.0)

message("done")


message("Testing predict()", appendLF=FALSE)

pred <- predict(mod)
summary(pred)

message("done")