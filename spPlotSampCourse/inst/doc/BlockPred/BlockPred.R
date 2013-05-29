
## @knitr setup, include=FALSE, cache=FALSE
# set global chunk options
		opts_chunk$set(fig.align='center', size = 'tiny')
		purl("BlockPred.Rnw") ## Dump all R code to a file
		library(spPlotSampCourse)
		library(sp)
		library(maptools)
		library(xtable)


## @knitr BlockPred-plot, fig.width=5, fig.height=5, echo=FALSE, include = FALSE, dev = "tikz"
	par(mar = c(0,0,0,0))
	plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n",
	 bty = "n")
	rect(0,0,1,1,lwd = 4)
	mtext("1) Block Kriging", 3, cex = 1.5, line = 1, adj = -.1)
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7, 
		col = "lightgreen")
	text(.06,.93,"$R$", cex = 3)
	text(.23,.76,"$A$", cex = 3)
	set.seed(12)
	points(runif(20), runif(20), pch = 19, cex = 2, col = "blue")
	points(runif(1), runif(1), pch = 1, cex = 2)
	text(.58,.68,"$Z(\\bs_i)$", cex = 2)
	text(.73,.52,"$Z(\\bs_0)$", cex = 2)


## @knitr CApolys-plot, fig.width=7, fig.height=8, echo=FALSE, include = FALSE, dev = "tikz"
	path <- system.file("rawdata/airPolluteCA", package = "spPlotSampCourse")
	outlineFile <- paste(path,"/","ca_outline", sep = "")
	otl <- readShapePoly(outlineFile)
	pointsFile <- paste(path,"/","ca_ozone_pts", sep = "")
	pts <- readShapePoints(pointsFile)
	polyLAFile <- paste(path,"/","polyLA", sep = "")
	polyLA <- readShapePoly(polyLAFile)
	polySFFile <- paste(path,"/","polySF", sep = "")
	polySF <- readShapePoly(polySFFile)

	par(mar = c(0,0,0,0))
	plot(otl)
	plot(polyLA, add = TRUE, col = "green", lwd = 5)
	plot(polySF, add = TRUE, col = "green", lwd = 5)
	plot(pts, add = TRUE, pch = 19, cex = .6)


## @knitr cache = TRUE
	library(spPlotSampCourse)
	library(maptools)
	path <- system.file("rawdata/airPolluteCA", package = "spPlotSampCourse")
	outlineFile <- paste(path,"/","ca_outline", sep = "")
	otl <- readShapePoly(outlineFile)
	pointsFile <- paste(path,"/","ca_ozone_pts", sep = "")
	pts <- readShapePoints(pointsFile)
	polyLAFile <- paste(path,"/","polyLA", sep = "")
	polyLA <- readShapePoly(polyLAFile)
	polySFFile <- paste(path,"/","polySF", sep = "")
	polySF <- readShapePoly(polySFFile)
	ozFit1 <- splmm(OZONE ~ 1, spdata = pts, estMeth = "REML",
		varComps = "circular", useAnisotropy = TRUE)
	blockPredGridLA <- createBlockPredGrid(polyLA)
	predictBlock(ozFit1, blockPredGridLA)
	blockPredGridSF <- createBlockPredGrid(polySF)
	predictBlock(ozFit1, blockPredGridSF)


## @knitr meuseDist-plot, fig.width=8, fig.height=8, echo=FALSE, include = FALSE
	data(meuse.grid)
	coordinates(meuse.grid) <- ~x+y
	proj4string(meuse.grid) <- CRS("+init=epsg:28992")

	layout(matrix(c(1,2), nrow = 1), width = c(2,1))
	par(mar = c(0,0,0,0))
	plotPointsRGB(meuse.grid,"dist", pch = 15)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
				xaxt = "n", yaxt = "n")
	text(.35,.83,"Distance\nto River", cex = 2)
	addRGBRampLegend(min(meuse.grid@data$dist), max(meuse.grid@data$dist), 
		.1, .3, .4, .7, 
		rlim = c(0,1), glim = c(0,0), blim = c(1,0), printFormat = "0.3",
		cex = 2, nticks = 3)


## @knitr meuseFFreq-plot, fig.width=8, fig.height=8, echo=FALSE, include = FALSE
	layout(matrix(c(1,2), nrow = 1), width = c(2,1))
	par(mar = c(0,0,0,0))
	plot(meuse.grid[meuse.grid@data[,"ffreq"] == 1,], pch = 15, 
		col = rgb(1,0,0))
	plot(meuse.grid[meuse.grid@data[,"ffreq"] == 2,], pch = 15, 
		col = rgb(.5,0,.5), add = TRUE)
	plot(meuse.grid[meuse.grid@data[,"ffreq"] == 3,], pch = 15, 
		col = rgb(0,0,1), add = TRUE)
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
				xaxt = "n", yaxt = "n")
	text(.45,.85,"Flood\nFrequency\nClass", cex = 2)
	addRGBClassLegend(.2, .3, .5, .7, 
		rgblist = list(c(1,0,0), c(.5, 0, .5), c(0, 0, 1)), 
		labels = c("1", "2", "3"), cex = 3) 


## @knitr cache = TRUE
	data(meuse)
	coordinates(meuse) <- ~x+y
	proj4string(meuse) <- CRS("+init=epsg:28992")
	data(meuse.grid)
	coordinates(meuse.grid) <- ~x+y
	proj4string(meuse.grid) <- CRS("+init=epsg:28992")
	znFit1 <- splmm(zinc ~ dist + ffreq, spdata = meuse, varComps = "besselK")
	predictBlock(znFit1, meuse.grid)


## @knitr 
	summary(znFit1)$coefficients
	summary(znFit1)$covparms
	summary(znFit1)$R2g


