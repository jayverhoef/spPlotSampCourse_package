#-------------------------------------------------------------------------------
#
#           empiricalSemivariogram.SpatialPointsDataFrame
#
#-------------------------------------------------------------------------------

#' Empirical Semivariogram or Covariance Based on Euclidean Distance
#'
#' Computes the empirical semivariogram or covariance from the data based on Euclidean distance.
#'
#' @param object an object of class \link{SpatialPointsDataFrame-class}
#' @param varName a response variable name in the data.frame of observed data in the SpatialPointsDataFrame.
#' @param nlag the number of lag bins to create, by direction if directions are specified. The distance between endpoints that define a bin will have equal lengths for all bins.  The bin sizes are then determined from the minimum lag in the data, and the specification of maxlag.
#' @param directions directions in degrees clockwise from north that allow lag binning to be directional.  Default is c(0, 45, 90, 135).  Values should be between 0	and 180, as there is radial symmetry in orientation between two points. 
#' @param tolerance the angle on either side of the directions to determine if a pair of points	falls in that direction class.  Note, a pair of points may be in more than one lag bin if tolerances for different directions overlap.
#' @param inc	the distance increment for each bin class.  Default is 0, in which case	maxlag and nclasses determine the distance increments. 
#' @param maxlag the maximum lag distance to consider when binning pairs of locations by the hydrologic distance that separates them.  If the specified maxlag is larger than the maximum distance among pairs of points, then maxlag is set to the maximum distance among pairs.  If inc is greater than 0, then maxlag is disregarded.
#' @param nlagcutoff the minimum number of pairs needed to estimate the semivariance for a bin. If the sample size is less than this value, the semivariance for the bin is not calculated.
#' @param EmpVarMeth method for computing semivariances.  The default is "MethMoment", the classical method of moments, which is just the average difference-squared within bin classes. "Covariance" computes covariance, assuming a zero mean, rather than semivariance, and "CovMean" uses the data mean when computing autocovariance, which may	be more biased than semivariograms because it subtracts off the simple mean of the response variable.	"RobustMedian" and "RobustMean" are robust estimators proposed by Cressie and Hawkins (1980). If v is a vector of all pairwise square-roots of absolute differences within a bin class, then RobustMedian computes median(v)^4/.457. "RobustMean" computes mean(v)^4/(.457 + .494/length(v)).
#'
#' @return A list of six vectors.  The lengths of all vectors are equal, which is equal to nlag*(number of directions) - (any missing lags due to nlagcutoff). 
#'  \item{distance}{
#'   the mean Euclidean distance separating pairs of sites used 
#'   to calculate the semivariance for each bin
#'}
#'  \item{gamma}{
#'   the estimated semivariance for each bin, based on EmpVarMeth
#'}
#'  \item{np}{
#'    the number of pairs of sites used to calculate the semivariance for each bin 
#'}
#'  \item{azimuth}{
#'	the azimuth, equivalent to the direction, used for the bin class
#'}
#'  \item{hx}{
#'	the x-coordinate of the center of the bin lag.
#'}
#'  \item{hy}{
#'	the y-coordinate of the center of the bin lag.
#'}
#'
#' @author Jay Ver Hoef
#' @rdname empSemivariogram
#' @method empSemivariogram SpatialPointsDataFrame
#' @S3method empSemivariogram SpatialPointsDataFrame

empSemivariogram.SpatialPointsDataFrame <- function(object, varName,
	nlag = 20, directions = c(0,45,90,135),
	tolerance = 22.5, inc = 0, maxlag = 1e32, nlagcutoff = 1,
	EmpVarMeth = "MethMoment")
{
	data <- object@data
	var <- varName
	x <- object@coords[,1]
	y <- object@coords[,2]

	n1 <- length(data[,1])
   # distance matrix among locations
	distance <- sqrt( ( matrix(x,nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(x,nrow=1,ncol=n1) )^2 +
		( matrix(y,nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(y,nrow=1,ncol=n1) )^2 )
	difx <- -(matrix(y,nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(y,nrow=1,ncol=n1))
	signind <- -(matrix(x,nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(x,nrow=1,ncol=n1)) < 0
	distance <- distance*1.0000000001
	theta.deg <- acos(difx/distance)*180/pi
   # matrix of degrees clockwise from north between locations
	theta.deg[signind] <- 360-theta.deg[signind]
	diff2 <- ( matrix(data[,var],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,var],nrow=1,ncol=n1) )^2
	sqrtdiff <- sqrt(abs( matrix(data[,var],nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1) -
		matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(data[,var],nrow=1,ncol=n1) ) )
	if(EmpVarMeth == "CovMean") temp4cov <- data[,var] - mean(data[,var])
	else temp4cov <- data[,var]
	covprod <- (matrix(temp4cov,nrow=n1,ncol=1) %*%
		matrix(rep(1,times=n1),nrow=1,ncol=n1)) *
		(matrix(rep(1,times=n1),nrow=n1,ncol=1) %*%
		matrix(temp4cov,ncol=n1,nrow=1))
# convert to vectors
	distance <- matrix(distance, ncol = 1)
	theta.deg <- matrix(theta.deg, ncol = 1)
	diff2 <- matrix(diff2, ncol = 1)
	sqrtdiff <- matrix(sqrtdiff, ncol = 1)
	covprod <- matrix(covprod, ncol = 1)
# trim off values greater than maxlag
	indmax <- distance <= maxlag
	distance <- distance[indmax,]
	theta.deg <- theta.deg[indmax,]
	diff2 <- diff2[indmax,]
	sqrtdiff <- sqrtdiff[indmax,]
	covprod <- covprod[indmax,]

	maxd<-max(distance)
	if( inc <= 0) inc <- maxd/nlag
	ind <- distance==0
	ndir <- length(directions)
	store.results <- matrix(data = NA, ncol = 6,
		dimnames = list(NULL, c("distance", "gamma", "np", "azimuth", "hx", "hy")))
	for (j in 1:ndir) {
		for ( i in 1:nlag){
			if( (directions[j]-tolerance)<0 && (directions[j]+tolerance)>0 )
				ind1 <- theta.deg >= 360+directions[j]-tolerance |
					theta.deg < directions[j]+tolerance
			else if( (directions[j]+tolerance)>360 && (directions[j]-tolerance)<360 )
				ind1 <- theta.deg < directions[j]+tolerance-360 |
					theta.deg >= directions[j]-tolerance
			else
				ind1 <- theta.deg >= directions[j]-tolerance &
					theta.deg < directions[j]+tolerance
			ind<-distance>(i-1)*inc & distance<=i*inc &
				!is.na(theta.deg) & ind1
			nclass <- sum(ind)
			if(EmpVarMeth == "MethMoment") cv <- mean(diff2[ind])
			if(EmpVarMeth == "RobustMean") cv <- ((mean(sqrtdiff[ind]))^4)/(.457 + .494/sum(ind))
			if(EmpVarMeth == "RobustMedian") cv <- (median(sqrtdiff[ind]))^4/.457
			if(EmpVarMeth == "Covariance" | EmpVarMeth == "CovMean") cv <- mean(covprod[ind])
			mean.dis <- mean(distance[ind])
			if(nclass > 0) store.results<-rbind(store.results,
				c(mean.dis,cv,nclass,directions[j],0,0))
		}
	}
	store.results[,"hx"]<-store.results[,"distance"]*sin(store.results[,"azimuth"]*pi/180)
	store.results[,"hy"]<-store.results[,"distance"]*cos(store.results[,"azimuth"]*pi/180)
	store.results[,"gamma"]<-store.results[,"gamma"]/2
	ind <- store.results[,"np"] >= nlagcutoff
	store.results <- store.results[ind,]
	ind <- !is.na(store.results[,"hx"])
	store.results <- store.results[ind,]
	store.results <- as.data.frame(store.results)
	class(store.results) <- "empSemivariogram"
	store.results
}

