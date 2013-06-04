\name{startPDF}
\alias{startPDF}
\title{Starts the PDF Viewer from Within the Package}
\usage{
  startPDF(file)
}
\arguments{
  \item{file}{path to pdf within the package. Use form like
  system.file("doc/directory/file.pdf", package =
  "spPlotSampCourse").  See the examples on how to start
  all PDFs for this course.}
}
\value{
  only creates side effect of opening PDF viewer on system
}
\description{
  Starts the PDF Viewer from Within the Package
}
\examples{
library(spPlotSampCourse)
startPDF(system.file("doc/01Introduction/Introduction.pdf", package = "spPlotSampCourse"))
startPDF(system.file("doc/02IntroToSpatialStat/IntroToSpatialStat.pdf", package = "spPlotSampCourse"))
startPDF(system.file("doc/03BlockPred/BlockPred.pdf", package = "spPlotSampCourse"))
startPDF(system.file("doc/04BlockPredFinGrid/BlockPredFinGrid.pdf", package = "spPlotSampCourse"))
startPDF(system.file("doc/05CountSamp/CountSamp.pdf", package = "spPlotSampCourse"))
}
\author{
  adapted from biobase package openPDF
}

