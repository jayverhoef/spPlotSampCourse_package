#-------------------------------------------------------------------------------
#
#           plotPointsRGBT.SpatialPointsDataFrame
#
#-------------------------------------------------------------------------------

#' Plots Points for Two Variables by RGB Color and Transparency
#'
#' Plots Points in a SpatialPointsDataFrame for Two Variables in the data.frame by RGB Color and Transparency
#'
#' @param x a SpatialPointsDataFrame object.
#' @param colorCol the column name in x for RGB colors.  The higher the value in this column, the redder it will be.  The lower the value in this column, the bluer it will be.
#' @param tranCol the column name in x for transparency.  The higher the value in this column, the more transparent it will be.  The lower the value in this column, the less transparent it will be.
#' @param rlim: the limits of red (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param glim: the limits of green (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param blim: the limits of blue (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param tlim: the limits of transparency (between 0 and 1). Generally, the range of  limits used for the map.
#' @param hminmax user-specified limits for the values of colorCol.  Default is NULL, in which case the limits of the observed values are used.  If specified, it should be a vector of two values.  If any values of the observed data exceed the limits, an error is returned.
#' @param tminmax user-specified limits for the values of tranCol.  Default is NULL, in which case the limits of the observed values are used.  If specified, it should be a vector of two values.  If any values of the observed data exceed the limits, an error is returned.
#'
#' @return a plot, and, if an assigment is made, then the minimum and maximum values used for the colorCol and tranCol are returned as a data.frame.  This is useful when adding other plots, such as a legend, which needs data limits.
#'
#' @author Jay Ver Hoef
#' @rdname plotPointsRGBT
#' @method plotPointsRGBT SpatialPointsDataFrame
#' @S3method plotPointsRGBT SpatialPointsDataFrame

plotPointsRGBT.SpatialPointsDataFrame <- function(x, colorCol, tranCol, 
	rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1), hminmax = NULL, 
	tminmax = NULL, ...) 
{
	if(!is.numeric(rlim)) return("rlim should be numeric")
	if(length(rlim) != 2) return("rlim should have two values")
	if(min(rlim) < 0 | max(rlim) > 1) return("rlim must be bounded by 0 and 1")
	if(min(rlim) < 0 | max(rlim) > 1) return("rlim must be bounded by 0 and 1")
	if(!is.numeric(glim)) return("glim should be numeric")
	if(length(glim) != 2) return("glim should have two values")
	if(min(glim) < 0 | max(glim) > 1) return("glim must be bounded by 0 and 1")
	if(min(glim) < 0 | max(glim) > 1) return("glim must be bounded by 0 and 1")
	if(!is.numeric(blim)) return("blim should be numeric")
	if(length(blim) != 2) return("blim should have two values")
	if(min(blim) < 0 | max(blim) > 1) return("blim must be bounded by 0 and 1")
	if(min(blim) < 0 | max(blim) > 1) return("blim must be bounded by 0 and 1")
	if(!is.numeric(tlim)) return("tlim should be numeric")
	if(length(tlim) != 2) return("tlim should have two values")
	if(min(tlim) < 0 | max(tlim) > 1) return("tlim must be bounded by 0 and 1")
	if(min(tlim) < 0 | max(tlim) > 1) return("tlim must be bounded by 0 and 1")
	if(!is.null(hminmax) & !is.numeric(hminmax)) 
		return("hminmax should be numeric")
	if(!is.null(hminmax) & length(hminmax) != 2) 
		return("hminmax should have two values")
	if(!is.null(tminmax) & !is.numeric(tminmax)) 
		return("tminmax should be numeric")
	if(!is.null(tminmax) & length(tminmax) != 2) 
		return("tminmax should have two values")
	minColor <- min(x@data[,colorCol])
	maxColor <- max(x@data[,colorCol])
	minTran <- min(x@data[,tranCol])
	maxTran <- max(x@data[,tranCol])
	minmaxDF <- data.frame(min = c(minColor, minTran), max = c(maxColor, maxTran))
	rownames(minmaxDF) <- c("colorValue","transparency")
	colnames(minmaxDF) <- c("min", "max")
	if(!is.null(hminmax)) {
		if(min(hminmax) > minColor) 
			return("minimum of hminmax is greater than some colorCol values")
		if(max(hminmax) < maxColor) 
			return("maximum of hminmax is less than some colorCol values")
		minHeat <- min(hminmax)
		maxHeat <- max(hminmax)
	}
	if(!is.null(tminmax)) {
		if(min(tminmax) > minTran) 
			return("minimum of tminmax is greater than some tranCol values")
		if(max(tminmax) < maxTran) 
			return("maximum of tminmax is less than some tranCol values")
		minTran <- min(tminmax)
		maxTran <- max(tminmax)
	}
	heat01 <- (x@data[,colorCol] - minColor)/(maxColor - minColor)
	tran01 <- (x@data[,tranCol] - minTran)/(maxTran - minTran)
	rvals <- rlim[2]*heat01 + rlim[1]*(1 - heat01)
	gvals <- glim[2]*heat01 + glim[1]*(1 - heat01)
	bvals <- blim[2]*heat01 + blim[1]*(1 - heat01)
	tvals <- tlim[2]*tran01 + tlim[1]*(1 - tran01)
	plot(x,	col = rgb(rvals, gvals, bvals, alpha = 1 - tvals), ...)
	return(invisible(minmaxDF))
} 

