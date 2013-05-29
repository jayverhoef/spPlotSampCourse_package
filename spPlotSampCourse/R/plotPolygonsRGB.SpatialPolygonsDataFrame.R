#-------------------------------------------------------------------------------
#
#           plotPolygonsRGB.SpatialPolygonsDataFrame
#
#-------------------------------------------------------------------------------

#' Plots SpatialPolygonsDataFrame Polygons and Colors Them 
#'
#' Plots SpatialPolygonsDataFrame polygons and colors them according to values in a specified column of the data data.frame in a SpatialPolygonsDataFrame
#'
#' @param x a SpatialPolygonsDataFrame object.
#' @param colorCol the column name in the data.frame of x whose values will be used for coloring the points.  The higher the value in this column, the more it will contribute to the second limit of the rlim, glim, and blim (see next set of arguments).  The lower the value in this column, the more it will contribute to the first limit of the rlim, glim, and blim (see next set of arguments)
#' @param rlim the limits of contributions of the red color, which must be bounded between 0 and 1.
#' @param glim the limits of contributions of the green color, which must be bounded between 0 and 1.
#' @param blim the limits of contributions of the blue color, which must be bounded between 0 and 1.
#' @param hminmax user-specified limits for the values of colorCol.  Default is NULL, in which case the limits of the observed values are used.  If specified, it should be a vector of two values.  If any values of the observed data exceed the limits, an error is returned.
#'
#' @return a plot, and, if an assigment is made, then the minimum and maximum values used for the colorCol are returned as a data.frame.  This is useful when adding other plots, such as a legend, which needs data limits.
#'
#' @author Jay Ver Hoef
#' @rdname plotPolygonsRGB
#' @method plotPolygonsRGB SpatialPolygonsDataFrame
#' @S3method plotPolygonsRGB SpatialPolygonsDataFrame

plotPolygonsRGB.SpatialPolygonsDataFrame <- function(x, colorCol,
	rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1), 
	hminmax = NULL, ...) 
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
	if(!is.null(hminmax) & !is.numeric(hminmax)) 
		return("hminmax should be numeric")
	if(!is.null(hminmax) & length(hminmax) != 2) 
		return("hminmax should have two values")
	minColorVal <- min(x@data[,colorCol])
	maxColorVal <- max(x@data[,colorCol])
	minmaxDF <- data.frame(min = minColorVal, max = maxColorVal)
	rownames(minmaxDF) <- c("colorValue")
	colnames(minmaxDF) <- c("min", "max")
	if(!is.null(hminmax)) {
		if(min(hminmax) > minColorVal) 
			return("minimum of hminmax is greater than some colorCol values")
		if(max(hminmax) < maxColorVal) 
			return("maximum of hminmax is less than some colorCol values")
		minColorVal <- min(hminmax)
		maxColorVal <- max(hminmax)
	}
	colorVal01 <- (x@data[,colorCol] - minColorVal)/(maxColorVal - minColorVal)
	rvals <- rlim[2]*colorVal01 + rlim[1]*(1 - colorVal01)
	gvals <- glim[2]*colorVal01 + glim[1]*(1 - colorVal01)
	bvals <- blim[2]*colorVal01 + blim[1]*(1 - colorVal01)
	plot(x,	col = rgb(rvals, gvals, bvals, alpha = 1), ...)
	return(invisible(minmaxDF))
} 

