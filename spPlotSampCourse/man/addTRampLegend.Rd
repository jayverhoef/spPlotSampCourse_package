\name{addTRampLegend}
\alias{addTRampLegend}
\title{Adds a Transparency Ramp}
\usage{
  addTRampLegend(minTran, maxTran, xleft, ybottom, xright,
    ytop, rval = 0.5, gval = 0, bval = 0.5, tlim = c(0, 1),
    nshades = 50, printFormat = 4.2, nticks = 3, ...)
}
\arguments{
  \item{minTran}{The minimum value for labeling the
  transparency ramp.}

  \item{maxTran}{The maximum value for labeling the
  transparency ramp.}

  \item{xleft}{a scalar of the left x position.}

  \item{ybottom:}{a scalar of bottom y position.}

  \item{xright}{a scalar of the right x position.}

  \item{ytop:}{a scalar of top y position.}

  \item{rval:}{the value of red (between 0 and 1) in rgb().
  Generally, the midpoint of limits used for the map.}

  \item{gval:}{the value of green (between 0 and 1) in
  rgb(). Generally, the midpoint of limits used for the
  map.}

  \item{bval:}{the value of blue (between 0 and 1) in
  rgb(). Generally, the midpoint of limits used for the
  map.}

  \item{tlim:}{the limits of transparency (between 0 and
  1). Generally, the range of limits used for the map.}

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
  add a transparency ramp legend as a rectangle to the
  currently active plot
}
\description{
  Adds a transparency ramp to the currently active plot
}
\author{
  Jay Ver Hoef
}
\seealso{
  \code{\link{plotHeatTran}}, \code{\link{rect}},
  \code{\link{addHeatRampLegend}}
}

