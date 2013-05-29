\name{predictBlock}
\alias{predictBlock}
\alias{predictBlock.splmm}
\title{Block Prediction}
\usage{
  predictBlock(x, ...)

  \method{predictBlock}{splmm} (obj, spPredPts)
}
\arguments{
  \item{obj}{the fitted model of class splmm}

  \item{spPredPts}{a grid of points of class SpatialPoints
  or SpatialPointsDataFrame that will be used to
  approximate the integral for block prediction.}
}
\value{
  a data.frame with the block prediction and its estimated
  standard error.
}
\description{
  Block prediction of the average value for a grid of
  points of class SpatialPoints or SpatialPointsDataFrame
  using a fitted model of class splmm. The grid of points
  should be dense within some polygon for the block
  prediction.  If block prediction uses covariates, then
  the covariate values should be in the data.frame of the
  SpatialPointsDataFrame, and all covariates used to fit
  the splmm model must be present without any missing
  values.
}
\author{
  Jay Ver Hoef
}

