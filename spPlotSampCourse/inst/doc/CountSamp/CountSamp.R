
## @knitr setup, include=FALSE
# smaller font size for chunks
opts_chunk$set(size = 'footnotesize')
purl("CountSamp.Rnw") ## Dump all R code to a file


## @knitr icyBayIntro-plot, fig.width=6, fig.height=4, echo=FALSE, include = FALSE, cache = TRUE
library(spPlotSampCourse)
path <- system.file("rawdata/seals", package = "spPlotSampCourse")
outlineFile <- paste(path,"/","outline", sep = "")
outline <- readShapePoly(outlineFile)
plotsFile <- paste(path,"/","plots", sep = "")
plots <- readShapePoly(plotsFile)
par(mar = c(0.1,0.1,0.1,0.1))
plot(outline)
minmax <- plotPolygonsRGB(plots, colorCol = "counts", add = TRUE, border = par("bg"))
addRGBRampLegend(minmax[,1], minmax[,2], 683353, 1182043, 684490, 1188985, 
	printFormat = "2.0")
text(686700, 1182800, "Icy Bay, Alaska, 2008", pos = 4)


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


## @knitr sim2-plot, fig.width=6, fig.height=6, echo=FALSE, include = FALSE
library(spPlotSampCourse)
#-------------------------------------------------------------------------------
#  SAMPLE PLOTS
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
#  SIMULATION
#-------------------------------------------------------------------------------
set.seed(2)
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


