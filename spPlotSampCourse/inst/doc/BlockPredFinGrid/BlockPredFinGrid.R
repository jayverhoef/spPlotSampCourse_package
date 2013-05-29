
## @knitr setup, include=FALSE, cache=FALSE
# set global chunk options
		opts_chunk$set(fig.align='center', size = 'tiny')
		purl("BlockPredFinGrid.Rnw") ## Dump all R code to a file
		library(spPlotSampCourse)
		library(sp)
		library(maptools)
		library(xtable)


## @knitr include = FALSE, echo = FALSE
library(spPlotSampCourse)
library(maptools)
path <- system.file("rawdata/moose", package = "spPlotSampCourse")
samplesFile <- paste(path,"/","Samples", sep = "")
samples <- readShapePoly(samplesFile)
samples@data[,"x"] <- LLtoUTM(samples@data[,"CENTRLAT"],samples@data[,"CENTRLON"])[,"x"]
samples@data[,"y"] <- LLtoUTM(samples@data[,"CENTRLAT"],samples@data[,"CENTRLON"])[,"y"]
samples@data[,"TOTAL"] <- as.numeric(as.character(samples@data[,"TOTAL"]))
samples@data[,"b"] <- rep(1, times = length(samples@data[,1]))


## @knitr mooseStrat-plot, fig.width=7, fig.height=4, echo=FALSE, include = FALSE
par(mar = c(0,0,5,0))
plot(samples)
plot(samples[samples@data[,"STRAT"] == "H",], col = rgb(.6, .2, .6), add = TRUE)
plot(samples[samples@data[,"STRAT"] == "L",], col = rgb(.6, .8, .6), add = TRUE)
addRGBClassLegend(-511500, 1701915, -500000, 1725308, 
	rgblist = list(c(.6, .2, .6),c(.6, .8, .6)),
	labels = c("High","Low"), cex = 2)
title("Stratification", cex.main = 2)


## @knitr mooseCounts-plot, fig.width=7, fig.height=4, echo=FALSE, include = FALSE
samples.c <- samples[samples@data[,"COUNTED"] == "Y",]
par(mar = c(0,0,5,0))
minmax <- plotPolygonsRGB(samples.c, "TOTAL")
addRGBRampLegend(minmax[1,1], minmax[1,2], -500000, 1691915, -488500, 1715308, 
	printFormat = "2.0", nshades = 20, cex = 1)
title("Counts", cex.main = 2)


## @knitr mooseElev-plot, fig.width=7, fig.height=4, echo=FALSE, include = FALSE
par(mar = c(0,0,5,0))
minmax <- plotPolygonsRGB(samples, "ELEVMEAN")
addRGBRampLegend(minmax[1,1], minmax[1,2], -511500, 1701915, -500000, 1725308, 
	printFormat = "2.0", nshades = 20, cex = 1.2)
title("Elevation", cex.main = 2)


## @knitr 
library(spPlotSampCourse)
library(maptools)
path <- system.file("rawdata/moose", package = "spPlotSampCourse")
samplesFile <- paste(path,"/","Samples", sep = "")
samples <- readShapePoly(samplesFile)
samples@data[,"x"] <- LLtoUTM(samples@data[,"CENTRLAT"],
	samples@data[,"CENTRLON"])[,"x"]
samples@data[,"y"] <- LLtoUTM(samples@data[,"CENTRLAT"],
	samples@data[,"CENTRLON"])[,"y"]
samples@data[,"TOTAL"] <- as.numeric(as.character(samples@data[,"TOTAL"]))
samples@data[,"b"] <- rep(1, times = length(samples@data[,1]))
sdata <- samples@data
coordinates(sdata) <- c("x","y")


## @knitr cache = TRUE
moFit <- splmm(TOTAL ~ ELEVMEAN + STRAT, spdata = sdata, 
	varComps = "exponential")
#summary(moFit)
summary(moFit)$coefficients
summary(moFit)$covparms


## @knitr cache = TRUE
moFit <- splmm(TOTAL ~ STRAT - 1, spdata = sdata, varComps = "exponential")
summary(moFit)$coefficients
summary(moFit)$covparms
FPBK <- predictBlockFinPop(moFit, "b")
FPBK


## @knitr cache = TRUE
samples.H <- samples[samples@data[,"STRAT"] == "H",]
samples.L <- samples[samples@data[,"STRAT"] == "L",]
sdataH <- samples.H@data
coordinates(sdataH) <- c("x","y")
sdataL <- samples.L@data
coordinates(sdataL) <- c("x","y")
moFitH <- splmm(TOTAL ~ 1, spdata = sdataH, varComps = "exponential")
summary(moFitH)$coefficients
moFitL <- splmm(TOTAL ~ 1, spdata = sdataL, varComps = "exponential")
summary(moFitL)$coefficients


## @knitr cache = TRUE
FPBK.H <- predictBlockFinPop(moFitH, "b")
FPBK.L <-predictBlockFinPop(moFitL, "b")
rbind(FPBK.H, FPBK.L)
FPBK.strat <- data.frame(Pred = FPBK.H[,"Pred"] + FPBK.L[,"Pred"],
PredSE = sqrt(FPBK.H[,"Pred SE"]^2 + FPBK.L[,"Pred SE"]^2))
FPBK.strat
FPBK


