
## @knitr setup, include=FALSE, cache=FALSE
# set global chunk options
opts_chunk$set(fig.align='center', size = 'tiny')
purl("IntroToSpatialStat.Rnw") ## Dump all R code to a file


## @knitr echo = FALSE, include = FALSE
library(spPlotSampCourse)
library(sp)
library(maptools)
library(xtable)


## @knitr echo = FALSE, cache = TRUE
path <- system.file("rawdata/airPolluteCA", package = "spPlotSampCourse")
pointsFile <- paste(path,"/","ca_ozone_pts", sep = "")
pts <- readShapePoints(pointsFile)
outlineFile <- paste(path,"/","ca_outline", sep = "")
otl <- readShapePoly(outlineFile)

ozFit1 <- splmm(OZONE ~ 1, spdata = pts, estMeth = "REML",
	varComps = "circular", useAnisotropy = TRUE)
pGrid <- createPredGrid(otl)
pGrid <- SpatialPointsDataFrame(pGrid, 
	as.data.frame(matrix(NA, nrow = length(pGrid), ncol = 1)))
ozPred1 <- predict(ozFit1, pGrid)


## @knitr CA-predMap, fig.width=4, fig.height=6, echo = FALSE, fig.keep = 'last'
par(mar = c(0,0,0,0))
layout(matrix(1:2, nrow = 1), width = c(2,1))
plot(otl)
minmaxDF <- plotPointsRGBT(ozPred1, colorCol = "OZONE.pred", 
	tranCol = "OZONE.predSE", pch = 15, cex = .4, add = TRUE,
	rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1))
plot(pts, add = TRUE, pch = 19, col = "yellow", cex = .6)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(-2310720,-327847.8),
	scale = 100000, fill = c("transparent","black"), plot.grid = FALSE)
text(-2309651,-306484.6,"0", cex = .7)
text(-2212377, -305416.5,"100 km", cex = .7)
SpatialPolygonsRescale(layout.north.arrow(), offset = c(-1820073,743516.4),
	scale = 80000, col = "green", plot.grid = FALSE)
plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
	xaxt = "n", yaxt = "n")
addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
	.2, .2, .4, .4, printFormat = "0.4", cex = .8)
text(.45,.46, labels = "Predicted Ozone \n Concentration", cex = .9)
addTRampLegend(minmaxDF["transparency","min"], 
	minmaxDF["transparency","max"], .2, .55, .4, .75, 
	rval = .5, gval = 0, bval = .5, tlim = c(0, 1), printFormat = "0.4", cex = .8)
text(.45,.81, labels = "Standard Error \n of Predictions", cex = .9)


## @knitr autCovGraph, fig.width=6, fig.height = 4, echo = FALSE
	par(mar = c(4,5,1,1))
	plot(c(0,85),c(0,5), type = "n", xlab = "Distance", ylab = "Covariance",
		cex.lab = 2, cex.axis = 1.5)
	#nugget effect lines
	lines(c(0,5),c(4.04,4.04), lwd = 1)
	lines(c(5,5),c(4.04,5), lwd = 1)
	lines(c(0,5),c(5,5), lwd = 1)
	lines(c(5,7),c(4.5,4.5), lwd = 1)
	text(6,4.5,"Nugget", cex = 2, pos = 4)
	#partial sill effect lines
	lines(c(0,5),c(3.96,3.96), lwd = 1)
	lines(c(5,5),c(0,3.96), lwd = 1)
	lines(c(.5,5),c(0,0), lwd = 1)
	lines(c(5,20),c(2,2), lwd = 1)
	text(20,2,"Partial Sill", cex = 2, pos = 4)
	#range lines
	lines(c(0,0),c(0.04,0.2), lwd = 1)
	lines(c(0,66),c(0.2,0.2), lwd = 1)
	lines(c(66,66),c(0.2,0.1), lwd = 1)
	lines(c(33,40),c(0.2,1), lwd = 1)
	text(40,1,"Range", cex = 2, pos = 4)
	#sill lines
	lines(c(67,70),c(0,0), lwd = 1)
	lines(c(70,70),c(0,5), lwd = 1)
	lines(c(67,70),c(5,5), lwd = 1)
	lines(c(70,75),c(2.5,2.5), lwd = 1)
	text(75,2.5,"Sill", cex = 2, pos = 4)
	#plot the curve and point
	lines(0:85, 4*exp(-(0:85)/15), lwd = 5, col = "blue")
	points(0,5, pch = 19, cex = 1, col = "blue")


## @knitr AutoCor-DataFig, fig.width=11, fig.height=6, out.width='.7\\linewidth', fig.align='center', size = 'tiny'
	x <- 1:100
	z <- 6 - ((x - 50)/20)^2 + rnorm(100)
	plot(x, z, pch = 19, cex = 2, cex.lab = 2, cex.axis = 1.5)


## @knitr AutoCor-ModelFig, fig.width=11, fig.height=6, out.width='.7\\linewidth', fig.align='center', size = 'tiny'
	x <- 1:100
	gamma <- exp(-x/50)
	plot(x, gamma, type = "l", lwd = 2, cex.lab = 2, cex.axis = 1.5, col = "blue")


## @knitr AutoCor-ProcessFig, fig.width=11, fig.height=6, out.width='.7\\linewidth', fig.align='center', size = 'tiny'
	set.seed(4)
	x <- 1:100
	y <- rep(1, times = 100)
	xyz <- geoStatSim(x,y, range = 100, nugget = .01, parsil = 6)
	plot(xyz$x, xyz$z, pch = 19, cex = 2, cex.lab = 2, cex.axis = 1.5)


## @knitr AutoCor-StatisticFig, fig.width=11, fig.height=6, out.width='.7\\linewidth', fig.align='center', size = 'tiny'
	spDF <- SpatialPointsDataFrame(cbind(xyz$x,xyz$y),data.frame(z = xyz$z))
	esv <- empSemivariogram(spDF,"z",EmpVarMeth = "CovMean")
	plot(esv$hx, esv$gamma, pch = 19, cex = esv$np/100, cex.lab = 2, cex.axis = 1.5)


## @knitr size = 'tiny'
	# Independence
	SigmaInd <- diag(6)
	# variance of mean estimator for first 5
	sum(SigmaInd[1:5,1:5])/5^2
	# variance of first 5 to predict the 6th
	sum(SigmaInd[1:5,1:5])/5^2 - 2*sum(SigmaInd[6,1:5])/5 + SigmaInd[6,6]


## @knitr size = 'tiny'
	#lots of autocorrelation
	SigmaAC <- matrix(.9999, nrow = 6, ncol = 6); diag(SigmaAC) <- 1
	# variance of mean estimator for first 5
	sum(SigmaAC[1:5,1:5])/5^2
	# variance of first 5 to predict the 6th
	sum(SigmaAC[1:5,1:5])/5^2 - 2*sum(SigmaAC[6,1:5])/5 + SigmaAC[6,6]


## @knitr size = 'tiny', fig.keep = "none"
set.seed(18)
z.AC <- t(chol(SigmaAC)	) %*% rnorm(6)
z.ind <- t(chol(SigmaInd)	) %*% rnorm(6)
plot(1:5, z.AC[1:5], ylim = c(-3,3), xlim = c(1,6), pch = 19, cex = 2, col = "green")
points(6, z.AC[6], pch = 19, cex = 2, col = "red")
lines(c(0,6),c(0,0), lwd = 3)
plot(1:5, z.ind[1:5], ylim = c(-3,3), xlim = c(1,6), pch = 19, cex = 2, col = "green")
points(6, z.ind[6], pch = 19, cex = 2, col = "red")
lines(c(0,6),c(0,0), lwd = 3, pch = 19, cex = 2)


## @knitr AutoCor-EstPredFig, include = FALSE, fig.width=5, fig.height=4, out.width='.35\\linewidth', fig.show='hold'
set.seed(18)
z.AC <- t(chol(SigmaAC)	) %*% rnorm(6)
z.ind <- t(chol(SigmaInd)	) %*% rnorm(6)
plot(1:5, z.AC[1:5], ylim = c(-3,3), xlim = c(1,6), pch = 19, cex = 2, col = "green")
points(6, z.AC[6], pch = 19, cex = 2, col = "red")
lines(c(0,6),c(0,0), lwd = 3)
plot(1:5, z.ind[1:5], ylim = c(-3,3), xlim = c(1,6), pch = 19, cex = 2, col = "green")
points(6, z.ind[6], pch = 19, cex = 2, col = "red")
lines(c(0,6),c(0,0), lwd = 3, pch = 19, cex = 2)


## @knitr Notation-plot, fig.width=5, fig.height=5, echo=FALSE, dev = "tikz", include = FALSE
par(mar = c(0,0,0,0))
plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n", bty = "n",
	lwd = 3)
rect(0,0,1,1,lwd = 4)
text(.06,.93,"$D$", cex = 3)
set.seed(12)
points(runif(20), runif(20), pch = 19, cex = 2, col = "red")
points(runif(1), runif(1), pch = 1, cex = 2)
text(.57,.685,"$Z(\\bs_i)$", cex = 2)
text(.70,.525,"$Z(\\bs_0)$", cex = 2)


## @knitr colorPointsCAOzone-plot, fig.width=4, fig.height=6, echo=FALSE, fig.keep = 'last', fig.align='center'
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	plot(otl)
	minmaxDF <- plotPointsRGB(pts, "OZONE", add = TRUE, pch = 19)
	SpatialPolygonsRescale(layout.scale.bar(), offset = c(-2310720,-327847.8),
		scale = 120000, fill = c("transparent","black"), plot.grid = FALSE)
	text(-2309651,-306484.6,"0", cex = .7)
	text(-2212377, -305416.5,"100 km", cex = .7)
	SpatialPolygonsRescale(layout.north.arrow(), offset = c(-1820073,743516.4),
		scale = 130000, col = "green", plot.grid = FALSE)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		0, .3, .2, .7, printFormat = "0.4", cex = .8)
	text(.45,.75,"Ozone (ppm)")


## @knitr echo = FALSE, cache = TRUE
	data(meuse)
	coordinates(meuse) <- ~x+y
	proj4string(meuse) <- CRS("+init=epsg:28992")
	meuse@data[,"logZn"] <- log(meuse@data[,"zinc"])
	path <- system.file("rawdata/meuse", package = "spPlotSampCourse")
	outlineFile <- paste(path,"/meuse_outline", sep = "")
	spsDF <- readShapePoly(outlineFile)
	data(meuse.riv)
	meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
	meuse.sr <- SpatialPolygons(meuse.lst)


## @knitr colorPointsMeuseLogZN-plot, fig.width=4, fig.height=6, echo=FALSE, fig.keep = 'last', fig.align='center'
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	plot(meuse.sr, col = "lightblue")
	plot(spsDF, add = TRUE)
	minmaxDF <- plotPointsRGB(meuse, "logZn", add = TRUE, pch = 19, cex = .8)
	SpatialPolygonsRescale(layout.scale.bar(), offset = c(178632.7,333516.3),
		scale = 1000, fill = c("transparent","black"), plot.grid = FALSE)
	text(178632.7, 333746.7, "0")
	text(179632.2, 333746.7, "1 km")
	SpatialPolygonsRescale(layout.north.arrow(), offset = c(179209.4, 336243.6),
		scale = 1000, col = "green", plot.grid = FALSE)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		0, .3, .2, .7, printFormat = "1.2", cex = .8)
	text(.45,.75,"log(Zn)")


## @knitr echo=FALSE, cache = TRUE
	path <- system.file("rawdata/NC_SIDS", package = "spPlotSampCourse")
	sidsFile <- paste(path,"/sids", sep = "")
	sids  <- readShapePoly(sidsFile)
	sids@data[,"sidsRate74"] <- sids@data[,"SID74"]/sids@data[,"BIR74"]


## @knitr echo=FALSE, cache = TRUE
	path <- system.file("rawdata/fireDiversity", package = "spPlotSampCourse")
	fireFile <- paste(path,"/fireDiv", sep = "")
	fire  <- readShapePoly(fireFile)


## @knitr rawSIDS-plot, fig.width=7, fig.height=1.5, echo=FALSE, fig.keep = "last", include = FALSE
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	minmaxDF <- plotPolygonsRGB(sids,"sidsRate74")
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		0, .1, .2, .7, printFormat = "0.4", cex = .7)
	text(.15,.85,"SIDS")


## @knitr fireDiv-plot, fig.width=6, fig.height=3, echo=FALSE, fig.keep = "last", include = FALSE
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	minmaxDF <- plotPolygonsRGB(fire,"z")
	points(fire@data[,"xCentroid"],fire@data[,"yCentroid"] , 
		pch = as.character(fire@data$trt), cex = 3, col = "white")
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	#undebug(addRGBRampLegend)
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		0, .1, .2, .7, printFormat = "2.0", cex = .9)
	text(.15,.85,"Species\nCounts", cex = 1.2)


## @knitr echo=FALSE, cache = TRUE, include = FALSE
	library(spatstat)
	data(anemones)
	spAnemones <- SpatialPoints(cbind(anemones$x,anemones$y))


## @knitr echo=FALSE, cache = TRUE, include = FALSE
	data(ponderosa)
	spPonderosa <- SpatialPoints(cbind(ponderosa$x,ponderosa$y))


## @knitr anemones-plot, fig.width=5, fig.height=5, echo=FALSE, fig.keep = "last", include = FALSE
	par(mar = c(0,0,5,0))
	plot(spAnemones, pch = 19)
	title("Anemones", cex.main = 2)


## @knitr ponderosa-plot, fig.width=5, fig.height=5, echo=FALSE, fig.keep = "last", include = FALSE
	par(mar = c(0,0,5,0))
	plot(spPonderosa, pch = 19)
	title("Ponderosa", cex.main = 2)


## @knitr CA-predSEMap, fig.width=4, fig.height=6, fig.keep = 'last', include=FALSE
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	plot(otl)
	minmaxDF <- plotPointsRGB(ozPred1, colorCol = "OZONE.predSE", 
		pch = 15, cex = .4, add = TRUE,
		rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1))
	plot(pts, add = TRUE, pch = 19, col = "yellow", cex = .6)
	SpatialPolygonsRescale(layout.scale.bar(), offset = c(-2310720,-327847.8),
		scale = 100000, fill = c("transparent","black"), plot.grid = FALSE)
	text(-2309651,-306484.6,"0", cex = .7)
	text(-2212377, -305416.5,"100 km", cex = .7)
	SpatialPolygonsRescale(layout.north.arrow(), offset = c(-1820073,743516.4),
		scale = 80000, col = "green", plot.grid = FALSE)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		.2, .4, .4, .6, printFormat = "0.4", cex = .8)
	text(.45,.66, labels = "Standard Error \n of Predictions", cex = .9)


## @knitr include = FALSE, cache = TRUE
	path <- system.file("rawdata/lizards", package = "spPlotSampCourse")
	CAfile <- paste(path,"/CA_outline", sep = "")
	CAoutline  <- readShapePoly(CAfile)
	safile <- paste(path,"/StudyArea", sep = "")
	sa  <- readShapePoly(safile)
	lizFile <- paste(path,"/whiptail_data", sep = "")
	lizards  <- readShapePoints(lizFile)
	lizards <- lizards[lizards@data$HYPER__COL > 0,]
	lizards@data[,"logAb"] <- log(lizards@data$HYPER__COL)


## @knitr lizardCA-plot, fig.width=5, fig.height=5, fig.keep = "last", include = FALSE
	par(mar = c(0,0,0,0))
	plot(CAoutline, xlim = c(-2159549, -1514591), ylim = c(-713903.6,-259775.1))
	plot(sa, add = TRUE)
	plot(lizards, add = TRUE, pch = 19, cex = .7)
	text(-1901455, -325734.4, "California", cex = 3)


## @knitr lizardPoints-plot, fig.width=5, fig.height=5, fig.keep = "last", include = FALSE
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(2,1))
	plot(sa)
	minmaxDF <- plotPointsRGB(lizards, colorCol = "logAb", 
		pch = 19, cex = .8, add = TRUE)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addRGBRampLegend(minmaxDF["colorValue","min"], minmaxDF["colorValue","max"],
		.2, .3, .5, .7, printFormat = "1.2", cex = .8)
	text(.35,.80,"log \n Abundance", cex = 1.5)


## @knitr lizardHist-plot, fig.width=5, fig.height=5, fig.keep = "last", include = FALSE
	par(mar = c(6,6,0,0))
	hist(lizards@data[,"logAb"], col = "blue", main = "", xlab = "log(Abundance)", cex.axis = 1.5, cex.lab = 2)


## @knitr lizardBox-plot, fig.width=5, fig.height=5, fig.keep = "last", include = FALSE
	par(mar = c(1,3,3,1))
	boxplot(lizards@data[,"logAb"], col = "blue", main = "boxplot of log(Abundance)", cex.axis = 1.5, cex.lab = 2, cex.main = 2)


## @knitr include = FALSE, cache = TRUE
	lizFit <- splmm(logAb ~ CREMATOGAS + LOG_SS, spdata = lizards, estMeth = "REML",
		varComps = "besselK", useAnisotropy = TRUE)


## @knitr results = "asis", echo = FALSE, include = FALSE
xtable(summary(lizFit)$coefficients, digits = c(1,3,3,2,4))


## @knitr results = "asis", echo = FALSE, include = FALSE
xtable(summary(lizFit)$covparms, digits = c(1,1,1,5))


## @knitr results = "asis", echo = FALSE, include = FALSE, cache = TRUE
    fireSDF <- SpatialPointsDataFrame(SpatialPoints(fire@data[,
		c("xCentroid","yCentroid")]),fire@data)
	fireSDF@data[,"trt"] <- as.factor(fireSDF@data[,"trt"])
	fireFitInd <- splmm(z ~ trt - 1, spdata = fireSDF, estMeth = "REML",
		varComps = NULL)
	fireFit <- splmm(z ~ trt - 1, spdata = fireSDF, estMeth = "REML",
		varComps = "exponential")
	Cmat <- matrix(c(-1, .5, .5,  0,  0,
									  -1,  0, 0, .5, .5,
										0, -.5, -.5, .5, .5,
										0, 1, -1, 0, 0, 
										0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
	estInd <- Cmat %*% fireFitInd$betaHat
	seInd <- sqrt(diag(Cmat %*% fireFitInd$covBetaHat %*% t(Cmat)))
	estExp <- Cmat %*% fireFit$betaHat
	seExp <- sqrt(diag(Cmat %*% fireFit$covBetaHat %*% t(Cmat)))
	fireTable <- cbind(c(-4, 6, 10, 2, 0), estInd, seInd, estExp, seExp)
	colnames(fireTable) <- c("True Value", "Ind Est", "Ind SE", "Sp Est", "Sp SE")
	xtable(fireTable, digits = c(0, 0, 1, 2, 2, 2))


## @knitr fireDivOrig-plot, fig.width=6, fig.height=3, echo=FALSE, fig.keep = "last", include = FALSE
	par(mar = c(0,0,0,0))
	plotPolygonsRGB(fire,"zOrig")
	for(i in 1:25) text(fire@data[i,"xCentroid"], fire@data[i,"yCentroid"], 
		as.character(fire@data$zOrig[i]), cex = 2, col = "white")


