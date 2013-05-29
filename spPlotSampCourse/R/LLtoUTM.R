#-------------------------------------------------------------------------------
#
#           LLtoUTM
#
#-------------------------------------------------------------------------------

#' Converts from Lat-Lon (decimal degrees) to the Universal Transverse Mercator Coordinates
#'
#' Converts from Lat-Lon (decimal degrees) to the Universal Transverse Mercator Coordinates
#'
#' @param lat a vector with latitude in decimal degrees
#' @param lon a vector with longitude in decimal degrees (negative values for western hemisphere)
#' @param xcol name of x-values in output, default = "x"
#' @param ycol name of y-values in output, default = "y"
#'
#' @return matrix of two columns with UTM coordinates. The coordinates are calculated from central meridian that is mean of longitude. Coordinates are returned in kilometers from the western-most longitude and the southern-most latitude observed in the data set.
#'
#' @author Jay Ver Hoef
#' @export

LLtoUTM <-
function(lat, lon, xcol = "x", ycol = "y")
# This function converts from Lat-Lon (decimal degrees) to the Universal
# Transverse Mercator Coordinates and returns the new coordinates in a
# 2-column matrix with x- and y- as columns.  In this program, the
# coordinates are calculated from central meridian that is mean of longitude.
# Coordinates are returned in kilometers from the western-most
# longitude and the southern-most latitude observed in the data set.

{
# check if any longitude values straddle the -180, +180 longitude line
# if so, convert minus longitude values to longitude values > 180
  if(any(lon > 90 & lon < 180) & any(lon > -180 & lon < -90))
    lon[lon < 0] <- 360 + lon[lon < 0]
  cm <- mean(lon)
# initialize some variables
	e2 <- 0.00676865799729
	a <- 6378206.4
	ep2 <- e2 / (1-e2)
	drc <- pi / 180
	sc <- 0.9996
	fe <- 500000
	ftm <- 0.30480371
#calculate some frequently used values
	lar <- lat * drc
	ls <- sin(lar)
	ls2 <- ls^2
	els2 <- ep2 * ls2
	lc <- cos(lar)
	lc2 <- lc^2
	lc3 <- lc^3
	lc5 <- lc^5
	elc2 <- ep2 * lc2
	lt2 <- tan(lar)^2
	lt4 <- lt2^2
# do the transformation
	v <- a/sqrt(1 - e2*ls2)
	p <- drc*(cm - lon)
	temp <- 5104.57388 - (lc2*(21.73607 - 0.11422*lc2))
	r1 <- 6367399.689*(lar - ls*lc*0.000001*temp)
	r2 <- (v*ls*lc*p^2)/2
	temp <- 5 - lt2 + 9*elc2 + (2*elc2)^2
	r3 <- (v*ls*lc3*p^4*temp)/24
	r4 <- v*lc*p
	temp <- 1 - lt2 + elc2
	r5 <- (v*lc3*p^3*temp)/6
	temp <- 61 - 58*lt2 + lt4 + 270*elc2 - 330*els2
	ra6 <- (v*ls*lc5*p^6*temp)/720
	temp <- 5 - 18*lt2 + lt4 + 14*elc2 - 58*els2
	rb5 <- (v*lc5*p^5*temp)/120
	northing <- sc*(r1 + r2 + r3 + ra6)
	easting <- -sc*(r4 + r5 + rb5)
	y <- (northing - min(northing))/1000
	x <- (easting - min(easting))/1000
	
	out <- cbind(x,y)
	colnames(out) <- c(xcol, ycol)
	out

}

