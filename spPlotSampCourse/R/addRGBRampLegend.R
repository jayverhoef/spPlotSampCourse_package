#-------------------------------------------------------------------------------
#
#           addRGBRampLegend
#
#-------------------------------------------------------------------------------

#' Adds a RGB Ramp 
#'
#' Adds a RGB ramp to the currently active plot
#'
#' @param minValue The minimum value for coloring and labeling the ramp.
#' @param maxValue The maximum value for coloring and labeling the ramp.
#' @param xleft a scalar of the left x position.
#' @param ybottom: a scalar of bottom y position.
#' @param xright a scalar of the right x position.
#' @param ytop: a scalar of top y position.
#' @param rlim: the limits of red (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param glim: the limits of green (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param blim: the limits of blue (between 0 and 1) in rgb(). Generally, the same limits used for the map.
#' @param nshades: the number of shades to use when making the ramp
#' @param printFormat: the numbers of digit before and after the decimal when printing the map. The default value 4.2 means 4 digits before the decimal, and 2 digits after the decimal.
#' @param nticks: the number of ticks between the labels for the maximum and minimim values.  The maximum and minimum values are always given, so this is the number of ticks inbetween, which will be evenly spaced along the ramp.
#'
#' @seealso \code{\link{plotPointsRGB}}, \code{\link{rect}}, 

#' @return add a color ramp legend as a rectangle to the currently active plot
#'
#' @author Jay Ver Hoef
#' @rdname addRGBRampLegend
#' @export addRGBRampLegend 

addRGBRampLegend <- function(minValue, maxValue, xleft, ybottom, xright, ytop, 
	rlim = c(0,1), glim = c(0,0), blim = c(1,0), nshades = 50, printFormat = 4.2,
	nticks = 3, ...) 
{
	for(i in 1:nshades)
		rect(xleft, ybottom + (i-1)/nshades*(ytop - ybottom), xright, 
			ybottom + i/nshades*(ytop - ybottom), col = rgb(
				rlim[2]*(i-1)/(nshades - 1) + rlim[1]*(1 - (i-1)/(nshades - 1)), 
				glim[2]*(i-1)/(nshades - 1) + glim[1]*(1 - (i-1)/(nshades - 1)), 
				blim[2]*(i-1)/(nshades - 1) + blim[1]*(1 - (i-1)/(nshades - 1))
				), border = NA)
	text(xright,ybottom, labels = sprintf(
		paste("%",as.character(printFormat),"f", sep = ""), minValue), pos = 4, ...)
	text(xright, ytop, labels = sprintf(
		paste("%",as.character(printFormat),"f", sep = ""), maxValue), pos = 4, ...)
	if(nticks > 0) {
		valueInc <- (maxValue - minValue)/(nticks + 1)
		tickInc <- (ytop - ybottom)/(nticks + 1)
		for(i in 1:nticks) {
			text(xright, ybottom + tickInc*i, labels = sprintf(
				paste("%",as.character(printFormat),"f", sep = ""), 
				minValue + valueInc*i), pos = 4, ...)
		}
	}
	return(invisible())	
} 

