
## @knitr setup, include=FALSE
# smaller font size for chunks
opts_chunk$set(size = 'tiny')
purl("CountSamp.Rnw") ## Dump all R code to a file


## @knitr icyBayIntro-plot, fig.width=8, fig.height=8, echo=FALSE, include = FALSE, cache = TRUE
library(spPlotSampCourse)
path <- system.file("rawdata/seals", package = "spPlotSampCourse")
outlineFile <- paste(path,"/","outline", sep = "")
outline <- readShapePoly(outlineFile)
plotsFile <- paste(path,"/","plots", sep = "")
plots <- readShapePoly(plotsFile)
par(mar = c(0,0,0,0))
plotsub <- plots[plots@data[,"counts"] > 0,]
plot(outline)
qtiles <- quantile(plotsub@data$counts, p = (1:3)/4)
breaks <- c(min(plotsub@data$counts)-1e-10, 
	qtiles, max(plotsub@data$counts))
cramp <- c(rgb(0,0,1), rgb(.33,.6,.67), rgb(.67,.6,.33), rgb(1,0,0))
ob <- plotsub
colorCol <- "counts"
plot(plotsub[plotsub@data$counts > breaks[1] & plotsub@data$counts <= breaks[2], ],
	col = cramp[1], add = TRUE, border = par("bg"))
plot(plotsub[plotsub@data$counts > breaks[2] & plotsub@data$counts <= breaks[3], ],
	col = cramp[2], add = TRUE, border = par("bg"))
plot(plotsub[plotsub@data$counts > breaks[3] & plotsub@data$counts <= breaks[4], ],
	col = cramp[3], add = TRUE, border = par("bg"))
plot(plotsub[plotsub@data$counts > breaks[4] & plotsub@data$counts <= breaks[5], ],
	col = cramp[4], add = TRUE, border = par("bg"))
plot(plots, add = TRUE)
addBreakColorLegend(682464, 1181494, 684511, 1189428, 
	breaks = breaks, colors = cramp, printFormat = "2.0", cex = 1.1)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(688240, 1181567),
	scale = 5000, fill = c("transparent","black"), plot.grid = FALSE)
text(688203,1182408,"0", cex = 1.5)
text(693175,1182408,"5 km", cex = 1.5)
SpatialPolygonsRescale(layout.north.arrow(), offset = c(697562,1193085),
	scale = 2000, col = "green", plot.grid = FALSE)
text(685332, 1196567, "Icy Bay, Alaska, 2008", pos = 4, cex = 2.5)


## @knitr irregSamples-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, dev = "tikz"
	par(mar = c(0,0,0,0))
	plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n", bty = "n")
	rect(0,0,1,1,lwd = 2)
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7, col = "lightgreen")
	rect(1.5*(1/6),3*(1/6),2.5*(1/6),4*(1/6), lwd = 5, col = "blue")
	text(2*(1/6),3.5*(1/6),"$B_i$", cex = 2.5, col = "white")
	rect(1.9*(1/6),1.2*(1/6),2.9*(1/6),2.2*(1/6), lwd = 5, col = "blue")
	rect(3*(1/6),3.5*(1/6),4*(1/6),4.5*(1/6), lwd = 5, col = "blue")
	rect(.8*(1/6),4.6*(1/6),1.8*(1/6),5.6*(1/6), lwd = 5, col = "blue")
	rect(4.5*(1/6),.5*(1/6),5.5*(1/6),1.5*(1/6), lwd = 5, col = "blue")
	rect(4.7*(1/6),3.8*(1/6),5.7*(1/6),4.8*(1/6), lwd = 5, col = "blue")
	rect(0.2*(1/6),0.2*(1/6),1.2*(1/6),1.2*(1/6), lwd = 5, col = "blue")
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7)
	text(.06,.93,"$R$", cex = 3)
	text(.23,.76,"$A$", cex = 3)


## @knitr spatialBasisExplanation-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, dev = "tikz", cache = TRUE
	circle <- function(x,y,r,...){
		m = 1000;
		A = seq(0, 2*pi, 2*pi/m);
		X = x + r * cos(A);
		Y = y + r * sin(A);
		lines(X,Y,...);
	}
	pointSimInhibit <- function(npair = 100, irange=0.05,
		lower.x.lim = -1, upper.x.lim = 1,
		lower.y.lim = -1, upper.y.lim = 1) 
	{ 
		spatpts <- data.frame(matrix(0,npair,2))
		colnames(spatpts) <- c("x","y")
		inpts <- 1 
		x.range <- upper.x.lim - lower.x.lim
		y.range <- upper.y.lim - lower.y.lim
		while(inpts <= npair){ok <- 1 
			xpt <- lower.x.lim + x.range*runif(1) 
			ypt <- lower.y.lim + y.range*runif(1) 
			chk <- inpts-1 
			ichk <- 1 
			while(ichk <= chk){ 
				distpt <- sqrt((xpt-spatpts[ichk,1])^2+(ypt-spatpts[ichk,2])^2) 
				if(distpt < irange){ 
					ok <- 0 
				ichk <- chk} 
			ichk <- ichk+1 } 
			if(ok == 1){spatpts[inpts,1:2] <- cbind(xpt,ypt) 
			inpts <- inpts+1 } 
		} 
		spatpts 
	} 
	par(mar = c(5,5,1,1))
	plot(c(0,10),c(0,10), type = "n", xlab = "x", ylab = "y", 
		cex.lab = 2, cex.axis = 2)
	points(pointSimInhibit(200, .4, 0, 10, 0, 10), pch = 19, cex = 1.5, col = "grey")
	for(i in 1:5) {
		for (j in 1:5) {
			inc <- 2
			x <- (10 + inc)/6 - inc/2 + (i-1)*inc
			y <- (10 + inc)/6 - inc/2 + (j-1)*inc
			points(x,y, pch = 19, cex = 2.5, col = "red")
			circle(x, y, .9*inc/2, lwd = 5, col = "red")
		}
	}
	for(i in 1:2) {
		for (j in 1:2) {
			inc <- 5
			x <- (10 + inc)/3 - inc/2 + (i-1)*inc
			y <- (10 + inc)/3 - inc/2 + (j-1)*inc
			points(x,y, pch = 19, cex = 2.5, col = "blue")
			circle(x, y, .9*inc/2, lwd = 5, col = "blue")
		}
	}
set.seed(101)
text(.2,9.9,"$\\bs_i$", pos = 4, cex = 3)
text(1,9,"$\\bkappa_{F,j}$", pos = 4, cex = 3)
text(2.5,7.5,"$\\bkappa_{C,j}$", pos = 4, cex = 3)


## @knitr sim2-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE
	library(spPlotSampCourse)
	#  SAMPLE PLOTS
		ni <- 16
		nj <- 16
		PlotSize <- 0.3
		offst <- (10-.3*16)/(2*17)
		ep <- .01
		samples <- NULL
		ID <- 1
		for(i in 1:ni) {
			for (j in 1:nj) {
				if (i != 3 & j != 2 & j != 5)
				{
					  xL <- (i-1)*10/ni + offst
					  xU <- (i-1)*10/ni + offst + PlotSize
					  yL <- (j-1)*10/nj + offst
					  yU <- (j-1)*10/nj + offst + PlotSize
					  samples <- c(samples, Polygons(list(Polygon(cbind(c(xL, xU, xU, xL, xL),
					                                    c(yL, yL, yU, yU, yL)))), ID = ID))
						ID = ID + 1
				}
			}
		}
		plots <- SpatialPolygons(samples)
		df <- data.frame(pltarea = rep(NA, times = length(plots@polygons)))
		for(i in 1:length(plots@polygons)) 
			df[i,"pltarea"] <- plots@polygons[[i]]@Polygons[[1]]@area
		row.names(df) <- as.character(1:length(plots@polygons))
		plots <- SpatialPolygonsDataFrame(plots, df)
		par(mar = c(0,0,0,0))
		plot(plots)
		loXlim <- 0
		upXlim <- 10
		loYlim <- 0
		upYlim <- 10
		outline <- SpatialPolygons(list(Polygons(list(Polygon(
			cbind(c(loXlim, upXlim, upXlim, loXlim, loXlim),
			c(loYlim, loYlim, upYlim, upYlim, loYlim)))), ID = "bnd")))
		plot(outline, add = TRUE)
	#  SIMULATION
		set.seed(32)
		lower.x.bbox <- runif(1, 3.5, 4.5)
		upper.x.bbox <- runif(1, 7.5, 8.5)
		lower.y.bbox <- runif(1, 3.5, 4.5)
		upper.y.bbox <- runif(1, 7.5, 8.5)
		nseed.big <- 100
		nseed.sma <- 25
		Poi.mean.big <- 15
		Poi.mean.sma <- 9
		big.range <- 1
		sma.range <- 0.02
		trend <- TRUE

		PlotSize <- .5
		pcover <- .5
		SampBal <- TRUE

		Sim <- pointSimClus(nseed.big = nseed.big,
			nseed.sma = nseed.sma,
			Poi.mean.big = Poi.mean.big,
			Poi.mean.sma = Poi.mean.sma,
			big.range = big.range,
			sma.range = sma.range,
			lower.x.lim = 0, upper.x.lim = 10,
			lower.y.lim = 0, upper.y.lim = 10,
			lower.x.bbox = lower.x.bbox, upper.x.bbox = upper.x.bbox,
			lower.y.bbox = lower.y.bbox, upper.y.bbox = upper.y.bbox,
			trend = trend)
		simPts <- Sim$SimPts
		coordinates(simPts) <- c("x","y")
		plot(simPts, add = TRUE, pch = 19, cex = .5)

		TrueAbundance <- length(coordinates(simPts)[,1])

		counts <- rep(NA, times  = length(plots@polygons))
		for(i in 1:length(plots@polygons)) {
			counts[i] <- sum(!is.na(over(simPts, 
				SpatialPolygons(list(plots@polygons[[i]])))))
		}
		# add count data to Photo Plot Outlines
		pltsData <- plots@data
		pltsData[,"counts"] <- counts
		plots@data <- pltsData
		#undebug(spCountSamp)
		EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
			nNodesRequestC = 4, 
			nNodesRequestF = 14, percentZero = 75)

		points(EstOut2$nodeLocationsF, pch = 19, col = "red", cex = 2)
		points(EstOut2$nodeLocationsC, pch = 19, col = "blue", cex = 2)
		plot(EstOut2$convexPolyKnotsFine, add = TRUE, lwd = 3, border = rgb(.2,.8,.2))


## @knitr exFittedSurface-plot, fig.width=8, fig.height=6, echo=FALSE, include = FALSE, fig.keep = 'last', cache = TRUE
	qtiles <- quantile(EstOut2$Predictions$Predictions, p = (1:7)/8)
	breaks <- c(min(EstOut2$Predictions$Predictions)-1e-32, 
		qtiles, max(EstOut2$Predictions$Predictions))
	cramp <- rainbow(length(breaks) - 1, start = .66, end = .99)
	par(mar = c(0,0,0,0))
	layout(matrix(1:2, nrow = 1), widths = c(3,1))
	image.spCountSamp(EstOut2, breaks = breaks, col = cramp)
	plot(plots, add = TRUE)
	plot(outline, add = TRUE)
	par(mar = c(0,0,0,0))
	plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
		xaxt = "n", yaxt = "n")
	addBreakColorLegend(.1, .1, .5, .9, 
		breaks = breaks, colors = cramp, printFormat = "2.4", cex = 1.5)


## @knitr resids4overdisp-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE
	fits <-	EstOut2$fits$fitsFixed
	par(mar = c(5,5,1,1))
	plot(fits, (counts - fits)^2/fits, pch = 19,
		xlab = "Fitted Value", ylab = "Pearson Residuals",
		cex.lab = 1.8, cex.axis = 1.5)
	ab <- coef(lm(y ~ x, data = data.frame(x = fits, 
		y = (counts - fits)^2/fits)))
	abline(a = ab[1], b = ab[2], lwd = 3, col = "blue")
	ODtrad <- sum((counts - fits)^2/fits)/(length(fits) - 19)
	lines(c(min(fits),max(fits)), c(ODtrad,ODtrad), lwd = 3, col = "black")
	lines(c(min(fits),max(fits)), c(1,1), lwd = 3, lty = 2)
	x.75 <- quantile(fits, .75)
	ODtrim <- EstOut2$ODtrim
	lines(c(x.75,max(fits)), c(ODtrim,ODtrim), lwd = 3, col = rgb(.2,.9,.2))


## @knitr rawresids4overdisp-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE
	par(mar = c(5,5,1,1))
	plot(fits, (counts - fits)^2, pch = 19,
		xlab = "Fitted Value", ylab = "Squared Raw Residuals",
		cex.lab = 1.8, cex.axis = 1.5)
	b <- coef(lm(y ~ x - 1, data = data.frame(x = fits, 
		y = (counts - fits)^2, weights = sqrt(fits))))
	abline(a = 0, b = b, lwd = 5, col = "blue")
	abline(a = 0, b = 1, lwd = 5, lty = 2)


## @knitr echo=FALSE, include = FALSE, cache = TRUE
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
	#  Run the function
	#undebug(spCountSamp)
	sCSout <- spCountSamp(counts ~ 1, outline, plots, 
			nNodesRequestC = 6, nNodesRequestF = 24, 
			percentZero = 50, nodeSetSeed = 101)
	summary(sCSout)


## @knitr icyBayKnots-plot, fig.width=8, fig.height=6, echo=FALSE, include = FALSE
par(mar = c(0,0,0,0))
plot(outline)
plot(plots, add = TRUE)
points(sCSout$nodeLocationsF, pch = 19, col = "red", cex = 2)
points(sCSout$nodeLocationsC, pch = 19, col = "blue", cex = 2)
plot(sCSout$convexPolyKnotsFine, add = TRUE, lwd = 3, border = rgb(.2,.8,.2))


## @knitr icyBayFit-plot, fig.width=8, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE
	qtiles <- quantile(sCSout$Predictions$Predictions, p = (1:7)/8)
	breaks <- c(min(sCSout$Predictions$Predictions)-1e-32, 
		qtiles, max(sCSout$Predictions$Predictions))
	cramp <- rainbow(length(breaks) - 1, start = .66, end = .99)

	image(sCSout, breaks = breaks, col = cramp)
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


## @knitr size = 'tiny'
sealDensity <- sum(plots@data[,"counts"])/
	(sCSout$propSurveyed*totalArea)
sealDensity*totalArea
summary(sCSout)


