\name{geoStatSim}
\alias{geoStatSim}
\title{Simulate geostatistical data on set of given locations}
\usage{
  geoStatSim(xcoords, ycoords, parsil = 1, range = 1,
    nugget = 0, minorp = 1, rotate = 0, extrap = NULL,
    spModel = "exponential")
}
\arguments{
  \item{xcoords}{a vector with x-coordinates}

  \item{ycoords}{a vector with x-coordinates}

  \item{parsil}{partial sill of autocorrelation model,
  default = 1}

  \item{range}{range of autocorrelation model, default = 1}

  \item{nugget}{range of autocorrelation model, default =
  0}

  \item{minorp}{proportion of range in x direction to that
  of y direction for unrotated anisotropic model, default =
  1}

  \item{rotate}{rotation of anisotropic axes, default = 0}

  \item{CorModel}{autocorrelation model, default =
  "exponential".  Other possibilities are ,"expRadon2",
  "expRadon4", "gaussian", "stable", "rationalQuad",
  "cauchyGrav", "cauchyMag", "cauchy", "circular",
  "spherical", "cubic", "penta", "cardinalSine", "besselK",
  "besselJ"}
}
\value{
  data.frame of three columns, the original xcoords and
  ycoords appended with a 3rd column of simulated
  geostatistical data
}
\description{
  simulates geostatistical data on set of given locations
}
\author{
  Jay Ver Hoef
}

