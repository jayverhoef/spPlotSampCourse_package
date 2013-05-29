#-------------------------------------------------------------------------------
#
#               predict.splmm
#
#-------------------------------------------------------------------------------

#' Predict Method for Spatial Linear Mixed Model Fits
#'
#' Makes predictions for missing data or using external prediction data set. 
#'
#' @param x A fitted splmm object
#' @param predData An optional SpatialPointsDataFrame object to be used for prediction.
#'
#' @details These are the universal kriging equations found in Cressie (1993, pg. 154-155).
#'
#' @return \code{predict.splmm} produces a SpatialPointsDataFrame with 2 additional columns for prediction, and prediction standard error for missing response variable. If \code{predData} is omitted, then the returned SpatialPointsDataFrame will be the subset of rows having missing response values in the data set used to fit the splmm object. If \code{predData} is used, then the returned SpatialPointsDataFrame will be the \code{predData} object, but the data frame contained at \code{@@data} will have two extra columns, one with the response variable name appended by \code{.pred} for predictions and by \code{.predSE} for the prediction standard errors.
#'
#' @references \cite{Cressie, N.A.C. (1993) Statistics for Spatial Data. Wiley.}
#'
#' @rdname predict
#' @method predict splmm
#' @S3method predict splmm
#' @author Jay Ver Hoef
predict.splmm <- function(x, predData = NULL)
{
	if(class(x) != "splmm") return("x is not a splmm object")
	varComps <- unlist(as.list(as.list(x$call)[-1]$varComps)[-1])
	modelList <- c("exponential","expRadon2","expRadon4",
		"gaussian","stable","rationalQuad","cauchyGrav","cauchyMag",
		"cauchy","circular","spherical","cubic","penta","cardinalSine",
		"besselK","besselJ")
	spModels <- NULL
	spModels <- varComps[varComps %in% modelList]
	reModels <- NULL
	Z <- NULL
	reModels <- varComps[varComps %in% names(x$data)]
	respCol <- as.character(as.list(attr(x$terms,"variables")[-1]))[1]
	if(is.null(predData)) {
		if(sum(attr(x$data,"NAs")) == 0L) return("nothing to predict")
		pData <- x$data[attr(x$data,"NAs"),,drop = FALSE]
		row.names(pData) <- paste("_p_",row.names(pData),sep = "")
		xcoordp <- x$xcoords[attr(x$data,"NAs")]
		ycoordp <- x$ycoords[attr(x$data,"NAs")]
	} else {
		if( summary(predData)[attr(summary(predData),"names") == "Class"] !=
			"SpatialPointsDataFrame")
			return("predData must be a SpatialPointsDataFrame object")
		pData <- predData@data
		row.names(pData) <- paste("_p_",row.names(pData),sep = "")
		xcoordp <- predData@coords[,1]
		ycoordp <- predData@coords[,2]
	}
	pData[,respCol] <- -999
	Xp <- model.matrix(x$formula, pData)
	pData[,respCol] <- NA
	if(length(reModels) > 0L) {
		Z <- rep(list(),length(reModels))
			for(i in 1L:length(reModels)) {
				Zdat <- as.factor(as.character(c(x$data[rownames(x$z),reModels[i]], 
					pData[,reModels[i]])))
				Zdat <- as.data.frame(Zdat)
				rownames(Zdat) <- c(rownames(Zdat)[1:x$obs.sample.size], 
					row.names(pData))
				names(Zdat) <- reModels[i]
				Z[[i]] <- model.matrix(as.formula(paste("~ -1 +",reModels[i])),Zdat)
			}
	}
	if(length(reModels) > 0) names(Z) <- reModels
	#subset to all records without missing covariates or random effects
	ind <- rep(TRUE, times = nrow(pData))
	ind <- rownames(pData) %in% rownames(Xp) & ind
	if(length(reModels) > 0) {
		for(i in 1L:length(reModels))
			ind <- rownames(pData) %in% rownames(Z[[i]])[(x$obs.sample.size + 1):
				nrow(Zdat)] & ind
	}
	Xp <- Xp[rownames(pData)[ind],,drop = F]
	if(length(reModels) > 0) {
		Z1 <- rep(list(),length(reModels))
		Z2 <- rep(list(),length(reModels))
		for(i in 1L:length(reModels)) {
			Z1[[i]] <- Z[[i]][1:x$obs.sample.size,]
			Z2[[i]] <- Z[[i]][(x$obs.sample.size + 1):
				nrow(Z[[i]]),][rownames(pData)[ind],]
		}
	}
	if(length(reModels) > 0) {
		names(Z1) <- reModels
		names(Z2) <- reModels
	}
	xcoordp <- xcoordp[ind]
	ycoordp <- ycoordp[ind]
	xcoordo <- x$xcoords[!attr(x$data,"NAs")]
	ycoordo <- x$ycoords[!attr(x$data,"NAs")]

	theta <- x$theta/attr(x$theta,"scale")
	theta[attr(theta,"tranf") == "exp"] <- log(theta[attr(theta,"tranf") == 
		"exp"])
	theta[attr(theta,"tranf") == "exp/(1+exp)"] <- log(theta[attr(theta,"tranf") == 
		"exp/(1+exp)"]/(1 - theta[attr(theta,"tranf") == "exp/(1+exp)"]))
	Vpred <- makeCovMat(theta = theta, Z1 = Z1, Z2 = Z2, xcoords1 = xcoordo, 
		ycoords1 = ycoordo, xcoords2 = xcoordp, ycoords2 = ycoordp,
		tranf = attr(x$theta,"tranf"), scale = attr(x$theta,"scale"), 
		type = attr(x$theta,"type"), label = attr(x$theta,"label"),
		pred = TRUE)

	Vi <- x$Vi
	X <- x$X
	covb <- x$covBetaHat
	sill <- sum(x$theta[attr(theta,"type") == "nugget" | 
		attr(theta,"type") == "parsil" | attr(theta,"type") == "vc"])
	z <- x$z
	pred.out <- matrix(NA, nrow = length(xcoordp), ncol = 2)
	pred.out[,1] <- apply(as.vector((Vi %*% z)) * Vpred, 2, sum) +
		Xp %*% x$betaHat - t(Vpred) %*% Vi %*% X %*% x$betaHat	
	pred.out[,2] <- sqrt(rep(sill, times = nrow(Xp)) - 
		apply((Vi %*% Vpred) * Vpred, 2, sum) +
		apply((covb %*% t(Xp)) * t(Xp), 2, sum) -
		2*apply((covb %*% t(Xp)) * (t(X) %*% Vi %*% Vpred), 2, sum) +
		apply((covb %*% t(X) %*% Vi %*% Vpred) * (t(X) %*% Vi %*% Vpred), 2, sum))
	colnames(pred.out) <- c(paste(respCol, ".pred", sep = ""), 
		paste(respCol, ".predSE", sep = ""))
	row.names(pred.out) <- row.names(Xp)
	pData <- cbind(pData[row.names(Xp),],pred.out)
	return(SpatialPointsDataFrame( 
		SpatialPoints(cbind(xcoordp,ycoordp)), pData, match.ID = TRUE))
}

