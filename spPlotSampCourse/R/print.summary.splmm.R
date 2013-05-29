#-------------------------------------------------------------------------------
#
#                    print.summary.spCountSamp
#
#-------------------------------------------------------------------------------

#' Printing a Summary of a Spatial Linear Mixed Model Fits
#'
#' method for class summary.splmm
#'
#' @param object a summary of a fitted splmm object
#'
#' @return the function \code{print.summary.splmm} the list of summary statistics of
#' the fitted spatial linear mixed model
#'
#'@rdname print
#'@method print summary.spCountSamp
#'@S3method print summary.spCountSamp
print.summary.spCountSamp <- function(x, ...)
{	
 		cat("\nEstimates:\n")
 		cat("\nTotal:\n")
		print(as.numeric(x$Estimates$estimate))
 		cat("Standard Errors:\n")
		print(as.data.frame(x$Estimates$stdError))
		cat("\n")
 		cat("\nRange Parameters:\n")
    print(as.data.frame(x$rangeParameters))
		cat("\n")
 		cat("\nProportion Surveyed:\n")
    print(as.numeric(x$proportionSurveyed))
		cat("\n")
    invisible(x)
}


