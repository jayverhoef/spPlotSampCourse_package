\name{m2LLi}
\alias{m2LLi}
\title{minus 2 times loglikelihood for selected covariance parameter}
\usage{
  m2LLi(x, i, theta, z, X, Z, xcoords, ycoords, estMeth,
    tranf, scale, type, label)
}
\arguments{
  \item{x}{real-valued variable}

  \item{i}{ith component of vector of covariance
  parameters}

  \item{theta}{vector of estimated covariance parameters}

  \item{z}{vector of data}

  \item{X}{design matrix for fixed effects}

  \item{Z}{list of design matrices for each random effect}

  \item{xcoords}{vector with the x-coordinates}

  \item{ycoords}{vector with the y-coordinates}

  \item{estMeth}{estimation method.  Default is "REML" for
  restricted maximum likelihood.  Other options are "ML"
  for maximum likelihood}

  \item{tranf}{name of transformation function. Input theta
  is transformed by tranf(theta)*scale}

  \item{scale}{vector of scaling values. Input theta is
  transformed by tranf(theta)*scale}

  \item{type}{type of covariance parameter.  Can be one of
  "parsil", "range", "rotate", "minorp", "extrap", "vc", or
  "nugget"}

  \item{label}{covariance model associated with
  parameters.}
}
\value{
  minus 2 times the loglikelihood
}
\description{
  minus 2 times loglikelihood for selected covariance
  parameter. Used for one-dimensional optimization
}
\author{
  Jay Ver Hoef
}

