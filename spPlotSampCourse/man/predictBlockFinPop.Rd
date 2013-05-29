\name{predictBlockFinPop}
\alias{predictBlockFinPop}
\alias{predictBlockFinPop.splmm}
\title{Block Prediction for a Finite Population}
\usage{
  predictBlockFinPop(x, ...)

  \method{predictBlockFinPop}{splmm} (x, wtsCol = NULL)
}
\arguments{
  \item{x}{A fitted splmm object}

  \item{wtsCol}{Name, or list of names using c(...), of the
  column(s) containing the weights. Default is NULL, in
  which case the weights will all be one, hence predicting
  the total.}
}
\value{
  a data.frame with 4 columns: the name of the response
  variable, the name of the column in the data set
  containing the weights, the block prediction value, and
  the block prediction standard error, with as many rows as
  there are columns listed by the \code{wtsCol} argument.
}
\description{
  Block prediction for a finite population, including the
  prediction of totals and means for all samples or small
  areas (subsets).
}
\details{
  Spatial block prediction for a finite population of
  samples was described by Ver Hoef (2002, 2008).  The
  fitted splmm object should be from a
  SpatialPointsDataFrame that includes the whole finite
  population.  Sampling units that were not observed are
  given NA values.  The desired weights for the whole
  population are contained in the columns of the
  SpatialPointsDataFrame, and then a spatial linear mixed
  model using \code{splmm} is fit to
  SpatialPointsDataFrame.  Then \code{predictBlockFinPop}
  can be used on the \code{splmm} object. Note, the
  function \code{predictBlockFinPop} does not report
  repeated measurements at locations, nor dropping of
  records due to missing covariate data. Take care to
  ensure that the weights are meaningful in these
  situations (e.g., by setting a weight of 1 for only one
  observation when a location is repeatedly measured, and
  setting all other weights to 0 for that location, when a
  total is desired).
}
\author{
  Jay Ver Hoef
}
\references{
  \cite{Ver Hoef, J.M. (2002) "Sampling and geostatistics
  for spatial data," Ecoscience, 9, 152 - 161.}

  \cite{Ver Hoef, J.M. (2008) "Spatial methods for
  plot-based sampling of wildlife populations,"
  Environmental and Ecological Statistics, 15, 3 - 13.}
}

