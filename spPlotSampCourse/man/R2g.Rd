\name{R2g}
\alias{R2g}
\alias{R2g.default}
\alias{R2g.splmm}
\title{A generalized R-squared for a fitted spatial linear mixed model}
\usage{
  R2g(x, ...)

  \method{R2g}{splmm} (x, ...)

  \method{R2g}{default} (z, Xdesign, bhat, Vi)
}
\arguments{
  \item{x}{a fitted splmm object}
}
\value{
  the generalized R-squared, interpreted as the proportion
  of the variation explained by the fixed effects part of
  the linear model

  \code{NULL}

  \code{NULL}
}
\description{
  A generalized R-squared for a fitted spatial linear mixed
  model
}
\author{
  Jay Ver Hoef
}

