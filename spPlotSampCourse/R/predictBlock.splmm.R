#-------------------------------------------------------------------------------
#
#          Block Prediction
#
#-------------------------------------------------------------------------------

#' Block Prediction
#'
#' Block prediction of the average value for a grid of points of class SpatialPoints or SpatialPointsDataFrame using a fitted model of class splmm. The grid of points should be dense within some polygon for the block prediction.  If block prediction uses covariates, then the covariate values should be in the data.frame of the SpatialPointsDataFrame, and all covariates used to fit the splmm model must be present without any missing values. 
#'
#' @param obj the fitted model of class splmm
#' @param spPredPts a grid of points of class SpatialPoints or SpatialPointsDataFrame that will be used to approximate the integral for block prediction.
#'
#' @return a data.frame with the block prediction and its estimated standard error.
#'
#' @author Jay Ver Hoef
#' @rdname predictBlock
#' @method predictBlock splmm
#' @S3method predictBlock splmm

predictBlock.splmm <- function(obj, spPredPts) {
	if(class(spPredPts) == "SpatialPoints") {
		Xp <- matrix(rep(1, times = length(obj$xcoords)),ncol = 1)
	} else if(class(spPredPts) == "SpatialPointsDataFrame"){
		respCol <- as.character(as.list(attr(obj$terms,"variables")[-1]))[1]
		spPredPts@data[1:length(spPredPts@data[,1]),respCol] <- 1
		Xp <- model.matrix(obj$formula, spPredPts@data)
	} else {
		error("spPredPts must be class SpatialPoints or SpatialPointsDataFrame")
	}

	covMat <- matrix(0, nrow = length(obj$xcoords), ncol = length(spPredPts@coords[,1]))
	type <- attr(obj$theta,"type")
	label <- attr(obj$theta,"label")
	theta <- obj$theta
	if(sum(type == "range") > 0) {
		spModels <- label[type == "range"]
		for(i in 1:length(spModels)) {
			rotatei <- 0
			minorpi <- 1
			rangei <- theta[type == "range" & 
				label == spModels[i]]
			parsili <- theta[type == "parsil" & 
				label == spModels[i]]
			if(any(type == "rotate" & 
				label == spModels[i])) {
					rotatei <- theta[type == "rotate" & 
						label == spModels[i]]
					minorpi <- theta[type == "minorp" & 
						label == spModels[i]]
			}
			if(any(type == "extrap" & 
				label == spModels[i])) {
					extrapi <- theta[type == "extrap" & 
						label == spModels[i]]
			}
			dismat <- spPlotSampCourse:::distGeoAni(obj$xcoords, obj$ycoords, 
				spPredPts@coords[,1], spPredPts@coords[,2], rotate = rotatei, 
				range = rangei, minorp = minorpi) 
			# compute correlation matrix for scaled distance matrix
			if(spModels[i] == "exponential") CorMat <- corModelExponential(dismat)
			if(spModels[i] == "expRadon2") CorMat <- corModelExpRadon2(dismat)
			if(spModels[i] == "expRadon4") CorMat <- corModelExpRadon4(dismat)
			if(spModels[i] == "gaussian") CorMat <- corModelGaussian(dismat)
			if(spModels[i] == "stable") CorMat <- corModelStable(dismat, extrapi)
			if(spModels[i] == "rationalQuad") CorMat <- corModelRationalQuad(dismat)
			if(spModels[i] == "cauchyGrav") CorMat <- corModelCauchyGrav(dismat)
			if(spModels[i] == "cauchyMag") CorMat <- corModelCauchyMag(dismat)
			if(spModels[i] == "cauchy") CorMat <- corModelCauchy(dismat, extrapi)
			if(spModels[i] == "circular") CorMat <- spPlotSampCourse:::corModelCircular(dismat)
			if(spModels[i] == "spherical") CorMat <- corModelSpherical(dismat)
			if(spModels[i] == "cubic") CorMat <- corModelCubic(dismat)
			if(spModels[i] == "penta") CorMat <- corModelPenta(dismat)
			if(spModels[i] == "cardinalSine") CorMat <- corModelCardinalSine(dismat)
			if(spModels[i] == "besselK") CorMat <- spPlotSampCourse:::corModelBesselK(dismat, extrapi)
			if(spModels[i] == "besselJ") CorMat <- corModelBesselJ(dismat, extrapi)
			# create the full covariance matrix with random effects for location and nugget
			covMat <- covMat + parsili*CorMat 
		}
	}
	cVec <- apply(covMat,1,mean)
	xA <- apply(Xp,2,mean)


	covMat <- matrix(0, nrow = length(spPredPts@coords[,1]), 
		ncol = length(spPredPts@coords[,1]))
	if(sum(type == "range") > 0) {
		spModels <- label[type == "range"]
		for(i in 1:length(spModels)) {
			rotatei <- 0
			minorpi <- 1
			rangei <- theta[type == "range" & 
				label == spModels[i]]
			parsili <- theta[type == "parsil" & 
				label == spModels[i]]
			if(any(type == "rotate" & 
				label == spModels[i])) {
					rotatei <- theta[type == "rotate" & 
						label == spModels[i]]
					minorpi <- theta[type == "minorp" & 
						label == spModels[i]]
			}
			if(any(type == "extrap" & 
				label == spModels[i])) {
					extrapi <- theta[type == "extrap" & 
						label == spModels[i]]
			}
			dismat <- spPlotSampCourse:::distGeoAni(spPredPts@coords[,1], 
				spPredPts@coords[,2], spPredPts@coords[,1], spPredPts@coords[,2], 
				rotate = rotatei, range = rangei, minorp = minorpi) 
			# compute correlation matrix for scaled distance matrix
			if(spModels[i] == "exponential") CorMat <- corModelExponential(dismat)
			if(spModels[i] == "expRadon2") CorMat <- corModelExpRadon2(dismat)
			if(spModels[i] == "expRadon4") CorMat <- corModelExpRadon4(dismat)
			if(spModels[i] == "gaussian") CorMat <- corModelGaussian(dismat)
			if(spModels[i] == "stable") CorMat <- corModelStable(dismat, extrapi)
			if(spModels[i] == "rationalQuad") CorMat <- corModelRationalQuad(dismat)
			if(spModels[i] == "cauchyGrav") CorMat <- corModelCauchyGrav(dismat)
			if(spModels[i] == "cauchyMag") CorMat <- corModelCauchyMag(dismat)
			if(spModels[i] == "cauchy") CorMat <- corModelCauchy(dismat, extrapi)
			if(spModels[i] == "circular") CorMat <- spPlotSampCourse:::corModelCircular(dismat)
			if(spModels[i] == "spherical") CorMat <- corModelSpherical(dismat)
			if(spModels[i] == "cubic") CorMat <- corModelCubic(dismat)
			if(spModels[i] == "penta") CorMat <- corModelPenta(dismat)
			if(spModels[i] == "cardinalSine") CorMat <- corModelCardinalSine(dismat)
			if(spModels[i] == "besselK") CorMat <- spPlotSampCourse:::corModelBesselK(dismat, extrapi)
			if(spModels[i] == "besselJ") CorMat <- corModelBesselJ(dismat, extrapi)
			# create the full covariance matrix with random effects for location and nugget
			covMat <- covMat + parsili*CorMat 
		}
	}
	sigma2A <- mean(covMat)

	Vi <- obj$Vi
	covBetaHat <- obj$covBetaHat
	X <- obj$X
	z <- obj$z
	n <- obj$obs.sample.size
	p <- obj$rank

	XXSiXi <- X %*% covBetaHat
	XSi <- t(X) %*% Vi
	r1 <- xA - XSi %*% cVec
	m <- covBetaHat %*% r1
	tlam <- t(cVec + XXSiXi %*% r1) %*% Vi
	outpt <- data.frame(
		BlockPredEst = tlam %*% z,
		BlockPredSE = sqrt(sigma2A - tlam %*% cVec + t(m) %*% xA)
	)

	return(outpt)

}






