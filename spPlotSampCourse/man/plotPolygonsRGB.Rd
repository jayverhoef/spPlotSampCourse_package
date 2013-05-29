\name{plotPolygonsRGB}
\alias{plotPolygonsRGB}
\alias{plotPolygonsRGB.SpatialPolygonsDataFrame}
\title{Plots SpatialPolygonsDataFrame Polygons and Colors Them}
\usage{
  plotPolygonsRGB(x, ...)

  \method{plotPolygonsRGB}{SpatialPolygonsDataFrame} (x,
    colorCol, rlim = c(0, 1), glim = c(0, 0),
    blim = c(1, 0), tlim = c(0, 1), hminmax = NULL, ...)
}
\arguments{
  \item{x}{a SpatialPolygonsDataFrame object.}

  \item{colorCol}{the column name in the data.frame of x
  whose values will be used for coloring the points.  The
  higher the value in this column, the more it will
  contribute to the second limit of the rlim, glim, and
  blim (see next set of arguments).  The lower the value in
  this column, the more it will contribute to the first
  limit of the rlim, glim, and blim (see next set of
  arguments)}

  \item{rlim}{the limits of contributions of the red color,
  which must be bounded between 0 and 1.}

  \item{glim}{the limits of contributions of the green
  color, which must be bounded between 0 and 1.}

  \item{blim}{the limits of contributions of the blue
  color, which must be bounded between 0 and 1.}

  \item{hminmax}{user-specified limits for the values of
  colorCol.  Default is NULL, in which case the limits of
  the observed values are used.  If specified, it should be
  a vector of two values.  If any values of the observed
  data exceed the limits, an error is returned.}
}
\value{
  a plot, and, if an assigment is made, then the minimum
  and maximum values used for the colorCol are returned as
  a data.frame.  This is useful when adding other plots,
  such as a legend, which needs data limits.
}
\description{
  Plots SpatialPolygonsDataFrame polygons and colors them
  according to values in a specified column of the data
  data.frame in a SpatialPolygonsDataFrame
}
\author{
  Jay Ver Hoef
}

