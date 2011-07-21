real4 <- function(x, ...) as.single(x)
setGeneric("real4")
setMethod("real4", "data.frame", function(x, ..., .verbose=FALSE) {
	for(n in colnames(x)) {
		val <- x[,n]
		if(is.numeric(val)) {
			if(.verbose) message("real4(): Coercing column ", n)
			x[,n] <- real4(val)
		}
	}
	x
})
