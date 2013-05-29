library(splmm)
library(splmmCourse)
library(sp)
library(maptools)


data(meuse)
summary(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
meuse@data[,"logZn"] <- log(meuse@data[,"zinc"])

data(meuse.grid)
summary(meuse.grid)
coordinates(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")

path <- system.file("data/meuse", package = "splmmCourse")
outlineFile <- paste(path,"/meuse_outline", sep = "")
spsDF <- readShapePoly(outlineFile)
plot(spsDF)
plot(meuse, add = TRUE, pch = 19)

data(meuse.riv)
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.sr <- SpatialPolygons(meuse.lst)

meFitZn <- splmm(logZn ~ dist + ffreq + soil, spdata = meuse, estMeth = "REML",
	varComps = "besselK", useAnisotropy = TRUE)
summary(meFitZn)

spsPredZn <- predict(meFitZn, meuse.grid)

par(mar = c(0,0,0,0))
layout(matrix(1:2, nrow = 1), width = c(2,1))
#1
plot(spsDF)
minmaxDF <- plotHeatTran(spsPredZn, heatCol = "logZn.pred", 
		tranCol = "logZn.predSE", pch = 15, cex = 1.177, add = TRUE,
		rlim = c(0,1), glim = c(1,0), blim = c(0,0), tlim = c(0,1))
plot(meuse.sr, col = "blue", add = TRUE)
plot(meuse, add = TRUE, pch = 19, col = "yellow")
SpatialPolygonsRescale(layout.scale.bar(), offset = locator(1),
	scale = 1000, fill = c("transparent","black"), plot.grid = FALSE)
text(locator(1),"0")
text(locator(1),"1 km")
SpatialPolygonsRescale(layout.north.arrow(), offset = locator(1),
	scale = 400, col = "green", plot.grid = FALSE)
#2
plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
addHeatRampLegend(minmaxDF["heat","min"], minmaxDF["heat","max"],
		.2, .2, .4, .4, 
	rlim = c(0,1), glim = c(1,0), blim = c(0,0),
	printFormat = 0.4)
text(.45,.43, labels = "Predicted log(Zinc) Concentration", cex = 1.1)
addTransparencyRampLegend(minmaxDF["transparency","min"], 
	minmaxDF["transparency","max"], .2, .5, .4, .7, 
	rval = .5, gval = .5, bval = 0, printFormat = 0.4)
text(.45,.73, labels = "Standard Error of Predictions", cex = 1.1)


# right click on mouse to terminate collection of points
#region <- locator(type = "o")
#n <- length(region$x)
#p <- Polygon(cbind(region$x, region$y)[c(1:n,1),], hole = FALSE)
#ps <- Polygons(list(p), ID = "region")
#sps <- SpatialPolygons(list(ps))
#DF <- data.frame(name = as.character("outline"))
#row.names(DF) <-  as.character("region")
#spsDF <- SpatialPolygonsDataFrame(sps,DF)
#path <- "/media/Hitachi2GB/00NMML/RPackages/splmmCourse_package/splmmCourse/inst/data/meuse"
#writeOGR(spsDF, path, layer = "meuse_outline", driver = "ESRI Shapefile")
