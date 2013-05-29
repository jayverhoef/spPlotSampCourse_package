\name{m2LL}
\alias{m2LL}
\title{minus 2 times loglikelihood for case 12}
\usage{
  m2LL(theta, z, X, Z, xcoords, ycoords, estMeth, tranf,
    scale, type, label)
}
\arguments{
  \item{theta}{vector of estimated covariance parameters}

  \item{z}{vector of data}

  \item{X}{design matrix for fixed effects}

  \item{Z}{list of design matrices for each random effect}

  \item{xcoords}{vector with the x-coordinates}

  \item{ycoords}{vector with the y-coordinates}

  \item{estMeth}{estimation method.  Default is "REML" for
  restricted maximum likelihood.  Other options are "ML"
  for maximum likelihood}
}
\value{
  minus 2 times the loglikelihood
}
\description{
  fits a geostatistical linear model
}
\author{
  Jay Ver Hoef
}

