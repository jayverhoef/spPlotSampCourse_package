\name{covParmIni}
\alias{covParmIni}
\title{Initializes the covariance parameter vector}
\usage{
  covParmIni(varComps, useAnisotropy, z, X, xcoords,
    ycoords)
}
\arguments{
  \item{varComps}{variance components passed from slmGeo}

  \item{useAnisotropy}{logical vector indicating use of
  anisotropy for each component in varComps}

  \item{z}{a vecotr with the observed response variable}

  \item{xcoords}{the vector of the column in the data
  data.frame with the x-coordinates, default is "x"}

  \item{ycol}{the name of the column in the data data.frame
  with the y-coordinates, default is "y"}

  \item{EstMeth}{estimation method.  Default is "REML" for
  restricted maximum likelihood.  Other options are "ML"
  for maximum likelihood}

  \item{CorModel}{spatial autocorrelation model for errors.
  Default is "Exponential".}

  \item{group}{the name of the column for a grouping
  variable; all groups are considered independent when
  fitting the model}

  \item{use.spatial}{Fit a spatial model?  Default is
  "TRUE"}

  \item{use.nugget}{include a nugget effect in parameter
  estimation?  Default is "TRUE"}

  \item{use.anisotropy}{include anistropy in parameter
  estimation?  Default is "FALSE"}

  \item{use.locrep}{include variance parameter for
  replicate measurements per location in parameter
  estimation?  Default is "FALSE"}
}
\value{
  a list with fitted model objects: ...$theta (fitted
  covariance parameters) ...$m2LL (minus 2 time the
  loglikelilhood), ...$Asycov (asymptotic covariance matrix
  of covariance parameters, from negative inverse Hessian),
  ...$V (fitted covariance matrix), ...$Vi (inverse of
  fitted covariance matrix)
}
\description{
  Initializes the covariance parameter vector
}
\author{
  Jay Ver Hoef
}

