	library(spPlotSampCourse)
	path <- system.file("rawdata/seals", package = "spPlotSampCourse")
	outlineFile <- paste(path,"/","outline", sep = "")
	outline <- readShapePoly(outlineFile)
	plotsFile <- paste(path,"/","plots", sep = "")
	plots <- readShapePoly(plotsFile)

# number of photos
length(plots@data[,1])
# percentage of zeros
sum(plots@data[,"counts"] == 0)/length(plots@data[,1])
sum(plots@data[,"counts"] > 0)
sum(plots@data[,"counts"])
max(plots@data[,"counts"])
# area surveyed in km^2
islandArea <- 0
for(j in 2:11)
	islandArea <- islandArea + outline@polygons[[1]]@Polygons[[j]]@area
totalArea <- outline@polygons[[1]]@Polygons[[1]]@area - islandArea

#-------------------------------------------------------------------------------
#  Run the function
#-------------------------------------------------------------------------------

#undebug(spCountSamp)
sCSout <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 6, nNodesRequestF = 25, 
		percentZero = 75, nodeSetSeed = 101)
summary(sCSout)

qtiles <- quantile(sCSout$Predictions$Predictions, p = (1:7)/8)
breaks <- c(min(sCSout$Predictions$Predictions)-1e-32, 
	qtiles, max(sCSout$Predictions$Predictions))
cramp <- rainbow(length(breaks) - 1, start = .66, end = .99)

image.spCountSamp(sCSout, breaks = breaks, col = cramp)
title("Fitted Prediction Surface", cex.main = 2)
plot(plots, add = TRUE)
plot(outline, add = TRUE)
addBreakColorLegend(682464, 1181494, 684511, 1189428, 
	breaks = breaks, colors = cramp, printFormat = "2.4", cex = 1.1)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(688240, 1181567),
	scale = 5000, fill = c("transparent","black"), plot.grid = FALSE)
text(688203,1182408,"0", cex = 1.5)
text(693175,1182408,"5 km", cex = 1.5)
SpatialPolygonsRescale(layout.north.arrow(), offset = c(697562,1193085),
	scale = 2000, col = "green", plot.grid = FALSE)

# a simple estimate
sealDensity <- sum(plots@data[,"counts"])/
	(summary(sCSout)$proportionSurveyed*totalArea)
estSimple <- sealDensity*totalArea
estSimple

#-------------------------------------------------------------------------------
#  Publication graphics
#-------------------------------------------------------------------------------

hist(pltsData[pltsData[,"counts"] > 0, "counts"])
countPlus <- pltsData[pltsData[,"counts"] > 0, "counts"]
sum(countPlus == 1)/length(countPlus)
sum(countPlus > 1 & countPlus < 6)/length(countPlus)
sum(countPlus >= 6)/length(countPlus)

pdf("/media/Hitachi2GB/00NMML/GlacierPhotoSampling/spCountSamp/spCountSamp/inst/doc/TEX/studyArea.pdf",
	height = 16, width = 16)
plot(plots)
plot(outline, add = TRUE)
for(i in 1:length(plots@polygons)) {
	if(pltsData[i,"counts"] > 0) {
		if(pltsData[i,"counts"] < 3)
			plot(SpatialPolygons(list(plots@polygons[[i]])), 
				col = "blue", add = TRUE)
		if(pltsData[i,"counts"] >= 3 & pltsData[i,"counts"] < 11)
			plot(SpatialPolygons(list(plots@polygons[[i]])), 
				col = "green", add = TRUE)
		if(pltsData[i,"counts"] >= 11)
			plot(SpatialPolygons(list(plots@polygons[[i]])), 
				col = "red", add = TRUE)
	}
}
dev.off()

fits <- as.vector(sCSout$fits$fitsFixed)
obs <- as.vector(pltsData[,"counts"])

plot(fits, obs, pch = 19, cex = .5)
plot(fits, (obs - fits)^2/fits, pch = 19, cex = .5)
pltsData[(obs - fits)^2/fits > 100000,]
plotsOutlier <- plots[(obs - fits)^2/fits > 100000, ]
plot(plotsOutlier, col = "yellow", add = TRUE, lwd = 5)




# Study Area

shape.path.filename <- paste(rootPath, "/", "icy_20080811_surveyextent_nad83", sep = "")
outline <- readShapePoly(shape.path.filename)

