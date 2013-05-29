\name{summary.splmm}
\alias{summary.splmm}
\title{Summarizing Spatial Linear Mixed Model Fits}
\usage{
  \method{summary}{splmm} (object, ...)
}
\arguments{
  \item{object}{a fitted splmm object}
}
\value{
  the function \code{summary.splmm} computes and returns a
  list of summary statistics of the fitted spatial linear
  mixed model, using the components (list element) "call"
  and "terms" from its argument, plus
  \item{\code{coefficients}}{a p x 4 matrix with columns
  for the estimated coefficient, its standard error,
  t-statistic and corresponding (two-sided) p-value.}
  \item{\code{covariance parameters}}{a data frame of the
  estimated covariance parameters} \item{\code{generalized
  R-squared}}{R-squared generalized for spatial linear
  mixed models.  Interpreted as the proportion of variation
  explained by the fixed effects in the model.}
}
\description{
  method for class splmm
}
\author{
  Jay Ver Hoef
}

