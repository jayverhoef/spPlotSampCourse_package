\name{makeCovMat}
\alias{makeCovMat}
\title{creates an appropriate spatial mixed model covariance matrix from parameter vector}
\usage{
  makeCovMat(theta, Z1, Z2, xcoords1, ycoords1, xcoords2,
    ycoords2, tranf, scale, type, label, pred = FALSE)
}
\arguments{
  \item{theta}{vector of estimated covariance parameters}

  \item{Z1}{list of design matrices for each random effect
  for first data set}

  \item{Z2}{list of design matrices for each random effect
  for second data set}

  \item{xcoords1}{vector with the x-coordinates for first
  data set}

  \item{ycoords1}{vector with the y-coordinates for first
  data set}

  \item{xcoords2}{vector with the x-coordinates for second
  data set}

  \item{ycoords2}{vector with the y-coordinates for second
  data set}

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
  a covariance matrix
}
\description{
  creates an appropriate spatial mixed model covariance
  matrix from parameter vector
}
\author{
  Jay Ver Hoef
}

