message("Testing FreeFlowLinks", appendLF=FALSE)
mat <- matrix(c(0, -5000, 0, 5000), ncol=4)
attr <- data.frame(TYP='AG', VPH=3000, W=30, H=0, EF=30)
row.names(attr) <- row.names(mat) <- "LINK A"
geom <- SpatialSegments(mat)
sldf <- SpatialLinesDataFrame(geom, attr)
ssdf <- FreeFlowLinks(sldf)
dat <- as(ssdf, "data.frame")
expect_equal(dat, 
	structure(
		list(XL1 = 0, YL1 = -5000, XL2 = 0, YL2 = 5000, TYP = "AG", VPH = 3000, W = 30, H = 0, EF = 30), 
		.Names = c("XL1", "YL1", "XL2", "YL2", "TYP", "VPH", "W", "H", "EF"), 
		row.names = c(NA, -1L), 
		class = "data.frame"))
message("done")