#' Creates grid of prediction points within boundary but outside of plots
#' 
#' Creates grid of prediction points within boundary but outside of plots. Also
#' computes coarse and fine node points using kmeans on subsamples of grid points. 
#'
#' @param bnd an outline of a single polygon of sp Class SpatialPolygon
#'
#' @return a prediction grid as a SpatialPoints class.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

createPredGrid <- function(bnd) {
	lower.x.lim <- bnd@bbox["x","min"]
	upper.x.lim <- bnd@bbox["x","max"]
	lower.y.lim <- bnd@bbox["y","min"]
	upper.y.lim <- bnd@bbox["y","max"]
	xrange <- upper.x.lim - lower.x.lim
	yrange <- upper.y.lim - lower.y.lim
	nc <- round(sqrt(xrange/yrange)*200)
	nr <- round(nc*yrange/xrange)
	GridPts <- pointSimSyst(nrow = nr, ncol = nc,
		lower.x.lim = lower.x.lim,
		upper.x.lim = upper.x.lim,
		lower.y.lim = lower.y.lim,
		upper.y.lim = upper.y.lim)
	GridPtsp <- SpatialPoints(GridPts)
	# create grid within boundary
	GridPtsp <- GridPtsp[!is.na(over(GridPtsp, bnd))[,1]]
	return(GridPtsp)
}

