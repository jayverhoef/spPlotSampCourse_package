#-------------------------------------------------------------------------------
#
#      A generalized R-squared for a fitted spatial linear mixed model
#
#-------------------------------------------------------------------------------

#' A generalized R-squared for a fitted spatial linear mixed model
#'
#' A generalized R-squared for a fitted spatial linear mixed model
#'
#' @param x a fitted splmm object
#'
#' @return the generalized R-squared, interpreted as the proportion of the variation
#' explained by the fixed effects part of the linear model
#'
#' @author Jay Ver Hoef
#'@rdname R2g
#'@export R2g
R2g <- function(x, ...){
  UseMethod("R2g")
}


#-------------------------------------------------------------------------------
#
#      A generalized R-squared for a fitted spatial linear mixed model
#
#-------------------------------------------------------------------------------

#'@return \code{NULL}
#'
#'@rdname R2g
#'@method R2g splmm
#'@S3method R2g splmm
R2g.splmm <- function(x,...)
{
	SS.bhat <- t(x$z -x$X %*% x$betaHat) %*% x$Vi %*% (x$z -x$X %*% x$betaHat)
	mu.hat <- sum(x$Vi %*% x$z)/sum(x$Vi)
	SS.mu <- matrix(x$z - mu.hat, nrow = 1) %*% x$Vi %*% matrix(x$z - mu.hat, ncol = 1)
	as.numeric(1 - SS.bhat/SS.mu)
}

#-------------------------------------------------------------------------------
#
#      A generalized R-squared for a fitted spatial linear mixed model
#
#-------------------------------------------------------------------------------

#'@return \code{NULL}
#'
#'@rdname R2g
#'@method R2g default
#'@S3method R2g default
R2g.default <- function(z, Xdesign, bhat, Vi)
{
	SS.bhat <- t(x$z -x$X %*% x$betaHat) %*% x$Vi %*% (x$z -x$X %*% x$betaHat)
	mu.hat <- sum(x$Vi %*% x$z)/sum(x$Vi)
	SS.mu <- matrix(x$z - mu.hat, nrow = 1) %*% x$Vi %*% matrix(x$z - mu.hat, ncol = 1)
	as.numeric(1 - SS.bhat/SS.mu)
}

