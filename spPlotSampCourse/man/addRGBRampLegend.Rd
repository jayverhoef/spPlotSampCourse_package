\name{addRGBRampLegend}
\alias{addRGBRampLegend}
\title{Adds a RGB Ramp}
\usage{
  addRGBRampLegend(minValue, maxValue, xleft, ybottom,
    xright, ytop, rlim = c(0, 1), glim = c(0, 0),
    blim = c(1, 0), nshades = 50, printFormat = 4.2,
    nticks = 3, ...)
}
\arguments{
  \item{minValue}{The minimum value for coloring and
  labeling the ramp.}

  \item{maxValue}{The maximum value for coloring and
  labeling the ramp.}

  \item{xleft}{a scalar of the left x position.}

  \item{ybottom:}{a scalar of bottom y position.}

  \item{xright}{a scalar of the right x position.}

  \item{ytop:}{a scalar of top y position.}

  \item{rlim:}{the limits of red (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{glim:}{the limits of green (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{blim:}{the limits of blue (between 0 and 1) in
  rgb(). Generally, the same limits used for the map.}

  \item{nshades:}{the number of shades to use when making
  the ramp}

  \item{printFormat:}{the numbers of digit before and after
  the decimal when printing the map. The default value 4.2
  means 4 digits before the decimal, and 2 digits after the
  decimal.}

  \item{nticks:}{the number of ticks between the labels for
  the maximum and minimim values.  The maximum and minimum
  values are always given, so this is the number of ticks
  inbetween, which will be evenly spaced along the ramp.}
}
\value{
  add a color ramp legend as a rectangle to the currently
  active plot
}
\description{
  Adds a RGB ramp to the currently active plot
}
\author{
  Jay Ver Hoef
}
\seealso{
  \code{\link{plotPointsRGB}}, \code{\link{rect}},
}

