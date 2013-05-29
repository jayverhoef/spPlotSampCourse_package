library(splmm)
library(splmmCourse)
library(sp)
library(maptools)

path <- system.file("data/airPolluteCA", package = "splmmCourse")
pointsFile <- paste(path,"/","ca_ozone_pts", sep = "")
pts <- readShapePoints(pointsFile)
outlineFile <- paste(path,"/","ca_outline", sep = "")
otl <- readShapePoly(outlineFile)
plot(otl)
plot(pts, add = TRUE, pch = 19)

ozFit1 <- splmm(OZONE ~ 1, spdata = pts, estMeth = "REML",
	varComps = "circular", useAnisotropy = TRUE)
summary(ozFit1)
#ozFitElev <- splmm(OZONE ~ ELEVATION, spdata = pts, estMeth = "ML")
#summary(ozFitElev)
#undebug(createPredGrid)
pGrid <- createPredGrid(otl)
pGrid <- SpatialPointsDataFrame(pGrid, 
	as.data.frame(matrix(NA, nrow = length(pGrid), ncol = 1)))
ozPred1 <- predict(ozFit1, pGrid)

# INTERACTIVE PLOT
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(3,1))
#1
	plot(otl)
	#undebug(plotHeatTran)
	minmaxDF <- plotHeatTran(ozPred1, heatCol = "OZONE.pred", 
		tranCol = "OZONE.predSE", pch = 15, cex = .6, add = TRUE,
		rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1))
	plot(pts, add = TRUE, pch = 19, col = "yellow")
	SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1),
		scale = 100000, fill = c("transparent","black"), plot.grid = FALSE)
	text(locator(1),"0")
	text(locator(1),"100 km")
	SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1),
		scale = 80000, col = "green", plot.grid = FALSE)
#2
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	# undebug(addHeatRampLegend)
	addHeatRampLegend(minmaxDF["heat","min"], minmaxDF["heat","max"],
		.2, .2, .4, .4, printFormat = 0.4)
	text(.45,.44, labels = "Predicted Ozone \n Concentration", cex = 1.1)
	addTransparencyRampLegend(minmaxDF["transparency","min"], 
		minmaxDF["transparency","max"], .2, .5, .4, .7, 
		rval = .5, gval = 0, bval = .5, printFormat = 0.4)
	text(.45,.74, labels = "Standard Error \n of Predictions", cex = 1.1)
dev.off()

# SAVE TO PDF
setwd(path)
pdf("testHeatTran.pdf", height = 14.0, width = 9)
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), width = c(3,1))
#1
	plot(otl)
	#undebug(plotHeatTran)
	minmaxDF <- plotHeatTran(ozPred1, heatCol = "OZONE.pred", 
		tranCol = "OZONE.predSE", pch = 15, cex = .6, add = TRUE,
		rlim = c(0,1), glim = c(0,0), blim = c(1,0), tlim = c(0,1))
	plot(pts, add = TRUE, pch = 19, col = "yellow")
#	locator()
	SpatialPolygonsRescale(layout.scale.bar(), offset = c(-2310720,-327847.8),
		scale = 100000, fill = c("transparent","black"), plot.grid = FALSE)
	text(-2309651,-306484.6,"0")
	text(-2212377, -305416.5,"100 km")
	SpatialPolygonsRescale(layout.north.arrow(), offset = c(-1820073,743516.4),
		scale = 80000, col = "green", plot.grid = FALSE)
#2
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	# undebug(addHeatRampLegend)
	addHeatRampLegend(minmaxDF["heat","min"], minmaxDF["heat","max"],
		.2, .2, .4, .4, printFormat = 0.4)
	text(.45,.44, labels = "Predicted Ozone \n Concentration", cex = 1.1)
	addTransparencyRampLegend(minmaxDF["transparency","min"], 
		minmaxDF["transparency","max"], .2, .5, .4, .7, 
		rval = .5, gval = 0, bval = .5, printFormat = 0.4)
	text(.45,.74, labels = "Standard Error \n of Predictions", cex = 1.1)
dev.off()


