setGeneric("Caline3Model", 
	function(receptors, sources, conditions, ...) {
		standardGeneric("Caline3Model")
	}
)

setMethod("Caline3Model", 
	signature(
		receptors = "Receptors", 
		sources = "FreeFlowLinks", 
		conditions = "HourlyMeteorology"
	),
	function(receptors, sources, conditions, ...) {
		new("Caline3Model", 
			receptors = receptors, 
			sources = sources, 
			conditions = conditions, 
			...
		)
	}
)

# Native code wrapper
CALINE3.Fortran <- function(...) {
	.call <- match.call()
	.fun <- .call[[1]]
	.args <- .call[-1]
	message("Entering: ", .fun)
	message("Args: ")
	eval(...)
	print(ls())
	quoted <- quote(.Fortran(
		"CALINE3_HOURLY_RECEPTOR_TOTALS",
		NR, XR, YR, ZR,	
		NL, XL1, YL1, XL2, YL2, WL, HL, NTYP, VPHL, EFL,
		NM, UM, BRGM, CLASM, MIXHM,
		ATIM, Z0, VS, VD,
		C = array(0.0, dim=shape),
		PACKAGE = "CALINE"
	))
	message("Exiting: ", .fun)
	print(quoted)
	quoted
	
	#pred <- retval$C
	#dim(pred) <- shape
	#pred
}

setGeneric("predict")

setMethod("predict", signature(object="Caline3Model"), function(object, ...) {

	.call <- match.call()
	.fun <- .call[[1]]
	message("Entering: ", .fun)
	
	# Convert receptors to a data.frame
	# FIXME: make sure it has the right columns
	rcp <- as(object@receptors, "data.frame")
	names(rcp)[1:3] <- c("XR", "YR", "ZR")
	
	# Convert links to a data.frame
	# FIXME: make sure it has the right columns
	lnk <- as(object@sources, "data.frame")
	if(is.character(lnk$TYP)) {
		clas.lookup <- list(AG=0, BR=1, FL=2, DP=3, 
			`At Grade`=0, `Bridge`=1, `Fill`=2, `Depressed`=3)
		lnk$NTYP <- as.integer(clas.lookup[lnk$TYP])
	} else if(is.integer(TYP)) {
		lnk$NTYP <- lnk$TYP
	} else {
		stop('lnk$TYP must be character or integer')
	}
	
	# Convert meteorology to a data.frame
	# FIXME: make sure it has the right columns
	met <- as(object@conditions, "data.frame")
	
	# TODO: tidy these up
	real4 <- function(x, with) if(missing(with)) as.single(x) else with(with, as.single(x))
	int <- function(x, with) if(missing(with)) as.integer(x) else with(with, as.integer(x))
	
	quoted <- quote(
		CALINE3.Fortran(
			NR = nrow(rcp), 
			XR = real4(XR, rcp),
			YR = real4(YR, rcp),
			ZR = real4(ZR, rcp),
			NL = nrow(lnk), 
			XL1 = real4(XL1, lnk), 
			YL1 = real4(YL1, lnk), 
			XL2 = real4(XL2, lnk), 
			YL2 = real4(YL2, lnk), 
			WL = real4(WL, lnk), 
			HL = real4(HL, lnk),
			NTYP = int(NTYP, lnk), 
			VPHL = real4(VPHL, lnk), 
			EFL = real4(EFL, lnk),
			NM = nrow(met), 
			UM = real4(UM, met), 
			BRGM = real4(BRGM, met), 
			CLASM = real4(CLASM, met), 
			MIXHM = real4(MIXHM, met),
			ATIM = real4(object@averagingTime),
			Z0 = real4(object@surfaceRoughness), 
			VS = real4(object@pollutant@settlingVelocity), 
			VD = real4(object@pollutant@depositionVelocity)
		)
	)
	message("Exiting: ", .fun)
	pred
})