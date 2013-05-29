#-------------------------------------------------------------------------------
#
#           summary.splmm
#
#-------------------------------------------------------------------------------

#' Summarizing Spatial Linear Mixed Model Fits
#'
#' method for class splmm
#'
#' @param object a fitted splmm object
#'
#' @return the function \code{summary.splmm} computes and returns a list of summary statistics of
#' the fitted spatial linear mixed model, using the components (list element) "call" and
#' "terms" from its argument, plus
#'    \item{\code{coefficients}}{a p x 4 matrix with columns for the estimated 
#'      coefficient, its standard error, t-statistic and corresponding (two-sided) 
#'      p-value.}
#'    \item{\code{covariance parameters}}{a data frame of the estimated covariance
#'      parameters}
#'    \item{\code{generalized R-squared}}{R-squared generalized for spatial linear mixed models.  Interpreted as the proportion of variation explained by the fixed effects in the model.}
#'
#'
#'@rdname summary
#'@method summary splmm
#'@S3method summary splmm
#' @author Jay Ver Hoef
summary.splmm <- function(object, ...)
{	
    se <- sqrt(diag(object$covBetaHat))
    est <- object$betaHat
    tval <- est/se
		rdf <- object$obs.sample.size - object$rank
    ans <- object[c("call", "terms")]
		ans$dataSampleSize <- object$data.sample.size
		ans$usedSampleSize <- object$obs.sample.size
		ans$coefficients <- cbind(est, se, tval, 2 * pt(abs(tval), 
        rdf, lower.tail = FALSE))
		colnames(ans$coefficients) <- c("Estimate", "Std. Error", "t value", 
			"Pr(>|t|)")
		ans$covparms <- data.frame(varcomp = attr(object$theta,"label"),
			type = attr(object$theta,"type"), est = object$theta)
		colnames(ans$covparms) <- c("Variance Component", "Parameter Type",
			"Estimate")
		ans$R2g <- R2g(object)
		class(ans) <- "summary.splmm"
		ans
}


