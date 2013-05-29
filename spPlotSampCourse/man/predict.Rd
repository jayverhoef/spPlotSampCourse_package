\name{predict.splmm}
\alias{predict.splmm}
\title{Predict Method for Spatial Linear Mixed Model Fits}
\usage{
  \method{predict}{splmm} (x, predData = NULL)
}
\arguments{
  \item{x}{A fitted splmm object}

  \item{predData}{An optional SpatialPointsDataFrame object
  to be used for prediction.}
}
\value{
  \code{predict.splmm} produces a SpatialPointsDataFrame
  with 2 additional columns for prediction, and prediction
  standard error for missing response variable. If
  \code{predData} is omitted, then the returned
  SpatialPointsDataFrame will be the subset of rows having
  missing response values in the data set used to fit the
  splmm object. If \code{predData} is used, then the
  returned SpatialPointsDataFrame will be the
  \code{predData} object, but the data frame contained at
  \code{@data} will have two extra columns, one with the
  response variable name appended by \code{.pred} for
  predictions and by \code{.predSE} for the prediction
  standard errors.
}
\description{
  Makes predictions for missing data or using external
  prediction data set.
}
\details{
  These are the universal kriging equations found in
  Cressie (1993, pg. 154-155).
}
\author{
  Jay Ver Hoef
}
\references{
  \cite{Cressie, N.A.C. (1993) Statistics for Spatial Data.
  Wiley.}
}

