\name{plotPointsRGBT}
\alias{plotPointsRGBT}
\alias{plotPointsRGBT.SpatialPointsDataFrame}
\title{Plots Points for Two Variables by RGB Color and Transparency}
\usage{
  plotPointsRGBT(x, ...)

  \method{plotPointsRGBT}{SpatialPointsDataFrame} (x,
    colorCol, tranCol, rlim = c(0, 1), glim = c(0, 0),
    blim = c(1, 0), tlim = c(0, 1), hminmax = NULL,
    tminmax = NULL, ...)
}
\arguments{
  \item{x}{a SpatialPointsDataFrame object.}

  \item{colorCol}{the column name in x for RGB colors.  The
  higher the value in this column, the redder it will be.
  The lower the value in this column, the bluer it will
  be.}

  \item{tranCol}{the column name in x for transparency.
  The higher the value in this column, the more transparent
  it will be.  The lower the value in this column, the less
  transparent it will be.}

  \item{rlim:}{the limits of red (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{glim:}{the limits of green (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{blim:}{the limits of blue (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{tlim:}{the limits of transparency (between 0 and
  1). Generally, the range of limits used for the map.}

  \item{hminmax}{user-specified limits for the values of
  colorCol.  Default is NULL, in which case the limits of
  the observed values are used.  If specified, it should be
  a vector of two values.  If any values of the observed
  data exceed the limits, an error is returned.}

  \item{tminmax}{user-specified limits for the values of
  tranCol.  Default is NULL, in which case the limits of
  the observed values are used.  If specified, it should be
  a vector of two values.  If any values of the observed
  data exceed the limits, an error is returned.}
}
\value{
  a plot, and, if an assigment is made, then the minimum
  and maximum values used for the colorCol and tranCol are
  returned as a data.frame.  This is useful when adding
  other plots, such as a legend, which needs data limits.
}
\description{
  Plots Points in a SpatialPointsDataFrame for Two
  Variables in the data.frame by RGB Color and Transparency
}
\author{
  Jay Ver Hoef
}

