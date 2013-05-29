\name{empSemivariogram}
\alias{empSemivariogram}
\alias{empSemivariogram.SpatialPointsDataFrame}
\title{Empirical Semivariogram or Covariance Based on Euclidean Distance}
\usage{
  empSemivariogram(x, ...)

  \method{empSemivariogram}{SpatialPointsDataFrame}
    (object, varName, nlag = 20,
    directions = c(0, 45, 90, 135), tolerance = 22.5,
    inc = 0, maxlag = 1e+32, nlagcutoff = 1,
    EmpVarMeth = "MethMoment")
}
\arguments{
  \item{object}{an object of class
  \link{SpatialPointsDataFrame-class}}

  \item{varName}{a response variable name in the data.frame
  of observed data in the SpatialPointsDataFrame.}

  \item{nlag}{the number of lag bins to create, by
  direction if directions are specified. The distance
  between endpoints that define a bin will have equal
  lengths for all bins.  The bin sizes are then determined
  from the minimum lag in the data, and the specification
  of maxlag.}

  \item{directions}{directions in degrees clockwise from
  north that allow lag binning to be directional.  Default
  is c(0, 45, 90, 135).  Values should be between 0 and
  180, as there is radial symmetry in orientation between
  two points.}

  \item{tolerance}{the angle on either side of the
  directions to determine if a pair of points falls in that
  direction class.  Note, a pair of points may be in more
  than one lag bin if tolerances for different directions
  overlap.}

  \item{inc}{the distance increment for each bin class.
  Default is 0, in which case maxlag and nclasses determine
  the distance increments.}

  \item{maxlag}{the maximum lag distance to consider when
  binning pairs of locations by the hydrologic distance
  that separates them.  If the specified maxlag is larger
  than the maximum distance among pairs of points, then
  maxlag is set to the maximum distance among pairs.  If
  inc is greater than 0, then maxlag is disregarded.}

  \item{nlagcutoff}{the minimum number of pairs needed to
  estimate the semivariance for a bin. If the sample size
  is less than this value, the semivariance for the bin is
  not calculated.}

  \item{EmpVarMeth}{method for computing semivariances.
  The default is "MethMoment", the classical method of
  moments, which is just the average difference-squared
  within bin classes. "Covariance" computes covariance,
  assuming a zero mean, rather than semivariance, and
  "CovMean" uses the data mean when computing
  autocovariance, which may be more biased than
  semivariograms because it subtracts off the simple mean
  of the response variable. "RobustMedian" and "RobustMean"
  are robust estimators proposed by Cressie and Hawkins
  (1980). If v is a vector of all pairwise square-roots of
  absolute differences within a bin class, then
  RobustMedian computes median(v)^4/.457. "RobustMean"
  computes mean(v)^4/(.457 + .494/length(v)).}
}
\value{
  A list of six vectors.  The lengths of all vectors are
  equal, which is equal to nlag*(number of directions) -
  (any missing lags due to nlagcutoff).  \item{distance}{
  the mean Euclidean distance separating pairs of sites
  used to calculate the semivariance for each bin }
  \item{gamma}{ the estimated semivariance for each bin,
  based on EmpVarMeth } \item{np}{ the number of pairs of
  sites used to calculate the semivariance for each bin }
  \item{azimuth}{ the azimuth, equivalent to the direction,
  used for the bin class } \item{hx}{ the x-coordinate of
  the center of the bin lag. } \item{hy}{ the y-coordinate
  of the center of the bin lag. }
}
\description{
  Computes the empirical semivariogram or covariance from
  the data based on Euclidean distance.
}
\author{
  Jay Ver Hoef
}

