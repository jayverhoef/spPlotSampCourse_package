\name{createPredGrid}
\alias{createPredGrid}
\title{Creates grid of prediction points within boundary but outside of plots}
\usage{
  createPredGrid(bnd)
}
\arguments{
  \item{bnd}{an outline of a single polygon of sp Class
  SpatialPolygon}
}
\value{
  a prediction grid as a SpatialPoints class.
}
\description{
  Creates grid of prediction points within boundary but
  outside of plots. Also computes coarse and fine node
  points using kmeans on subsamples of grid points.
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

