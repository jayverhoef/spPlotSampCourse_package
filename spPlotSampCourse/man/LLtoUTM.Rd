\name{LLtoUTM}
\alias{LLtoUTM}
\title{Converts from Lat-Lon (decimal degrees) to the Universal Transverse Mercator Coordinates}
\usage{
  LLtoUTM(lat, lon, xcol = "x", ycol = "y")
}
\arguments{
  \item{lat}{a vector with latitude in decimal degrees}

  \item{lon}{a vector with longitude in decimal degrees
  (negative values for western hemisphere)}

  \item{xcol}{name of x-values in output, default = "x"}

  \item{ycol}{name of y-values in output, default = "y"}
}
\value{
  matrix of two columns with UTM coordinates. The
  coordinates are calculated from central meridian that is
  mean of longitude. Coordinates are returned in kilometers
  from the western-most longitude and the southern-most
  latitude observed in the data set.
}
\description{
  Converts from Lat-Lon (decimal degrees) to the Universal
  Transverse Mercator Coordinates
}
\author{
  Jay Ver Hoef
}

