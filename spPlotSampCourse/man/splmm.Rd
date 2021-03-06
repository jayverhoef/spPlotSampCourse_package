\name{splmm}
\alias{splmm}
\title{Fits geostatistical linear model}
\usage{
  splmm(formula, spdata, estMeth = "REML",
    varComps = "exponential", useAnisotropy = FALSE)
}
\arguments{
  \item{formula}{an R linear model formula}

  \item{spdata}{an sp SpatialPointsDataFrame}

  \item{estMeth}{estimation method.  Default is "REML" for
  restricted maximum likelihood.  Other options are "ML"
  for maximum likelihood}

  \item{varComps}{a list of variance components, including
  spatial autocorrelation models for errors and traditional
  random effects.  The list of spatial autocorrelation
  models is
  "exponential","expRadon2","expRadon4","gaussian","stable",
  "rationalQuad","cauchyGrav","cauchyMag","cauchy","circular","spherical",
  "cubic","penta","cardinalSine","besselK","besselJ"
  Default is "exponential". Any names in the list not given
  above will be searched among the columns in the data set
  and used as a factor variable for levels of a traditional
  random effect.}

  \item{useAnisotropy}{include anistropy in parameter
  estimation?  Default is "FALSE"}
}
\value{
  a list of class "splmm".  The functions "summary" and
  "print" are used to obtain and print a summary. "anova"
  returns just the analysis of variance table...
}
\description{
  fits a geostatistical linear model
}
\author{
  Jay Ver Hoef
}

