#-------------------------------------------------------------------------------
#
#           addTRampLegend
#
#-------------------------------------------------------------------------------

#' Adds a Transparency Ramp 
#'
#' Adds a transparency ramp to the currently active plot
#'
#' @param minTran The minimum value for labeling the transparency ramp.
#' @param maxTran The maximum value for labeling the transparency ramp.
#' @param xleft a scalar of the left x position.
#' @param ybottom: a scalar of bottom y position.
#' @param xright a scalar of the right x position.
#' @param ytop: a scalar of top y position.
#' @param rval: the value of red (between 0 and 1) in rgb(). Generally, the midpoint of limits used for the map.
#' @param gval: the value of green (between 0 and 1) in rgb(). Generally, the midpoint of  limits used for the map.
#' @param bval: the value of blue (between 0 and 1) in rgb(). Generally, the midpoint of  limits used for the map.
#' @param tlim: the limits of transparency (between 0 and 1). Generally, the range of  limits used for the map.
#' @param nshades: the number of shades to use when making the ramp
#' @param printFormat: the numbers of digit before and after the decimal when printing the map. The default value 4.2 means 4 digits before the decimal, and 2 digits after the decimal.
#' @param nticks: the number of ticks between the labels for the maximum and minimim values.  The maximum and minimum values are always given, so this is the number of ticks inbetween, which will be evenly spaced along the ramp.
#'
#' @seealso \code{\link{plotHeatTran}}, \code{\link{rect}}, \code{\link{addHeatRampLegend}}

#' @return add a transparency ramp legend as a rectangle to the currently active plot
#'
#' @author Jay Ver Hoef
#' @rdname addTRampLegend
#' @export addTRampLegend 

addTRampLegend <- function(minTran, maxTran, xleft, ybottom, xright, ytop, 
	rval = .5, gval = 0, bval = .5, tlim = c(0, 1), nshades = 50, printFormat = 4.2,
	nticks = 3, ...) 
{
	for(i in 1:nshades)
		rect(xleft, ybottom + (i-1)/nshades*(ytop - ybottom), xright, 
			ybottom + i/nshades*(ytop - ybottom), col = rgb(rval, gval, bval, 
				tlim[1]*(i-1)/(nshades - 1) + tlim[2]*(1 - (i-1)/(nshades - 1))
				), border = NA)
	text(xright,ybottom, labels = sprintf(
		paste("%",as.character(printFormat),"f", sep = ""), minTran), pos = 4, ...)
	text(xright, ytop, labels = sprintf(
		paste("%",as.character(printFormat),"f", sep = ""), maxTran), pos = 4, ...)
	if(nticks > 0) {
		tranInc <- (maxTran - minTran)/(nticks + 1)
		tickInc <- (ytop - ybottom)/(nticks + 1)
		for(i in 1:nticks) {
			text(xright, ybottom + tickInc*i, labels = sprintf(
				paste("%",as.character(printFormat),"f", sep = ""), 
				minTran + tranInc*i), pos = 4, ...)
		}
	}
	return(invisible())	
} 


