\name{summary.spCountSamp}
\alias{summary.spCountSamp}
\alias{summary.splmm}
\title{Default summary method for spCountSamp objects
Default summary method for spCountSamp objects}
\usage{
  \method{summary}{spCountSamp} (ob)

  \method{summary}{splmm} (object, ...)
}
\arguments{
  \item{ob}{spCountSamp objects}

  \item{object}{a fitted splmm object}
}
\value{
  list of summary statistics, including the estimate, a
  data.frame of various standard error estimates, a
  data.frame of proportions of the variance components, and
  a data.frame of the range parameters.

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
  Default summary method for spCountSamp objects Default
  summary method for spCountSamp objects

  method for class splmm
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}

  Jay Ver Hoef
}

