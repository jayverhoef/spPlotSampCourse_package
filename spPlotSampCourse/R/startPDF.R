#-------------------------------------------------------------------------------
#
#      Starts the PDF Viewer from Within the Package
#
#-------------------------------------------------------------------------------

#' Starts the PDF Viewer from Within the Package
#'
#' Starts the PDF Viewer from Within the Package
#'
#' @param file path to pdf within the package. Use form like system.file("doc/directory/file.pdf", package = "spPlotSampCourse").  See the examples on how to start all PDFs for this course. 
#'
#' @return only creates side effect of opening PDF viewer on system
#'
#' @examples
#' library(spPlotSampCourse)
#' startPDF(system.file("doc/01Introduction/Introduction.pdf", package = "spPlotSampCourse"))
#' startPDF(system.file("doc/02IntroToSpatialStat/IntroToSpatialStat.pdf", package = "spPlotSampCourse"))
#' startPDF(system.file("doc/03BlockPred/BlockPred.pdf", package = "spPlotSampCourse"))
#' startPDF(system.file("doc/04BlockPredFinGrid/BlockPredFinGrid.pdf", package = "spPlotSampCourse"))
#' startPDF(system.file("doc/05CountSamp/CountSamp.pdf", package = "spPlotSampCourse"))
#' @author adapted from biobase package openPDF
#' @rdname startPDF
#' @export startPDF

startPDF <- function(file) 
{
    OST <- .Platform$OS.type
    if (OST == "windows") 
        shell.exec(file)
    else if (OST == "unix") {
        pdf <- getOption("pdfviewer")
        msg <- NULL
        if (is.null(pdf)) 
            msg <- "getOption('pdfviewer') is NULL"
        else if (length(pdf) == 1 && nchar(pdf[[1]]) == 0) 
            msg <- "getOption('pdfviewer') is ''"
        if (!is.null(msg)) 
            stop(msg, "; please use 'options(pdfviewer=...)'")
        cmd <- paste(pdf, file)
        system(cmd)
    }
    return(TRUE)
}

