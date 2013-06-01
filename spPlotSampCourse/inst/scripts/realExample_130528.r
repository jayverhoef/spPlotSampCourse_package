library(spPlotSampCourse)
path <- system.file("rawdata/seals", package = "spPlotSampCourse")
outlineFile <- paste(path,"/","outline", sep = "")
outline <- readShapePoly(outlineFile)
plotsFile <- paste(path,"/","plots", sep = "")
plots <- readShapePoly(plotsFile)
plot(outline)
minmax <- plotPolygonsRGB(plots, colorCol = "counts", add = TRUE)
addRGBRampLegend(minmax[,1], minmax[,2], 683353, 1182043, 684490, 1188985, 
	printFormat = "2.0", cex = 2)
text(686700, 1182800, "Icy Bay, Alaska, 2008", cex = 3, pos = 4)

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

sCSout <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 6, nNodesRequestF = 25, 
		percentZero = 75, nodeSetSeed = 101)
summary(sCSout)

plot(sCSout, cex.leglab = 1.5, cex.main = 2, main = "Study Area")

# a simple estimate
sealDensity <- sum(pltsData[,"counts"])/
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

