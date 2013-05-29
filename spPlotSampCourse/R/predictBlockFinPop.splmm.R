#-------------------------------------------------------------------------------
#
#           predictBlockFinPop
#
#-------------------------------------------------------------------------------

#' Block Prediction for a Finite Population 
#'
#' Block prediction for a finite population, including the prediction of totals and means for all samples or small areas (subsets).
#'
#' @param x A fitted splmm object
#' @param wtsCol Name, or list of names using c(...), of the column(s) containing the weights. Default is NULL, in which case the weights will all be one, hence predicting the total.
#'
#' @details Spatial block prediction for a finite population of samples was described by Ver Hoef (2002, 2008).  The fitted splmm object should be from a SpatialPointsDataFrame that includes the whole finite population.  Sampling units that were not observed are given NA values.  The desired weights for the whole population are contained in the columns of the SpatialPointsDataFrame, and then a spatial linear mixed model using \code{splmm} is fit to SpatialPointsDataFrame.  Then \code{predictBlockFinPop} can be used on the \code{splmm} object. Note, the function \code{predictBlockFinPop} does not report repeated measurements at locations, nor dropping of records due to missing covariate data. Take care to ensure that the weights are meaningful in these situations (e.g., by setting a weight of 1 for only one observation when a location is repeatedly measured, and setting all other weights to 0 for that location, when a total is desired).
#'
#' @return a data.frame with 4 columns: the name of the response variable, the name of the column in the data set containing the weights, the block prediction value, and the block prediction standard error, with as many rows as there are columns listed by the \code{wtsCol} argument. 
#'
#' @references \cite{Ver Hoef, J.M. (2002) "Sampling and geostatistics for spatial data," Ecoscience, 9, 152 - 161.} 
#' @references \cite{Ver Hoef, J.M. (2008) "Spatial methods for plot-based sampling of wildlife populations," Environmental and Ecological Statistics, 15, 3 - 13.}

#' @author Jay Ver Hoef
#' @rdname predictBlockFinPop
#' @method predictBlockFinPop splmm
#' @S3method predictBlockFinPop splmm
predictBlockFinPop.splmm <- function(x, wtsCol = NULL)
{
	if(class(x) != "splmm") return("x is not a splmm object")
	varComps <- unlist(as.list(x$call)[-1]$varComps)
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
	if(sum(attr(x$data,"NAs")) == 0L) return("nothing to predict")
	pData <- x$data[attr(x$data,"NAs"),,drop = FALSE]
	row.names(pData) <- paste("_p_",row.names(pData),sep = "")
	xcoordp <- x$xcoords[attr(x$data,"NAs")]
	ycoordp <- x$ycoords[attr(x$data,"NAs")]
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

	outpt <- NULL
	for(i in 1L:max(length(wtsCol),1)) {
		if(is.null(wtsCol)) {
			Bs <- matrix(rep(1, times = length(x$z)), ncol = 1)
			Bu <- matrix(rep(1, times = nrow(Xp)), ncol = 1)
			wtsCol <- "Total(Default)"
		} else {
			if(!any(wtsCol[i] %in% names(x$data))) 
				return("specified wtsCol name is not in the data set")
			Bs <- x$data[rownames(x$z), wtsCol[i]]
			Bu <- x$data[substr(rownames(Xp),4L,10000L), wtsCol[i]]
		}

		theta <- x$theta/attr(x$theta,"scale")
		theta[attr(theta,"tranf") == "exp"] <- log(theta[attr(theta,"tranf") == 
			"exp"])
		theta[attr(theta,"tranf") == "exp/(1+exp)"] <- log(theta[attr(theta,"tranf") == 
			"exp/(1+exp)"]/(1 - theta[attr(theta,"tranf") == "exp/(1+exp)"]))
		SU <- makeCovMat(theta = theta, Z1 = Z1, Z2 = Z2, xcoords1 = xcoordo, 
			ycoords1 = ycoordo, xcoords2 = xcoordp, ycoords2 = ycoordp,
			tranf = attr(x$theta,"tranf"), scale = attr(x$theta,"scale"), 
			type = attr(x$theta,"type"), label = attr(x$theta,"label"),
			pred = TRUE)
		UU <- makeCovMat(theta = theta, Z1 = Z2, Z2 = Z2, xcoords1 = xcoordp, 
			ycoords1 = ycoordp, xcoords2 = xcoordp, ycoords2 = ycoordp,
			tranf = attr(x$theta,"tranf"), scale = attr(x$theta,"scale"), 
			type = attr(x$theta,"type"), label = attr(x$theta,"label"))

		Xs <- x$X
		Xu <- Xp

		# predictions

		part1 <- Xs %*% solve(t(Xs) %*% x$Vi %*% Xs)
		part2 <- t(Xu) - t(Xs) %*% x$Vi %*% SU
		D <- SU + part1 %*% part2
		FF <- x$Vi %*% D
		Ao <- Bs + FF %*% Bu
		yEst <- t(Ao) %*% x$z
	
		# variance

		part1 <- t(FF) %*% x$V %*% FF
		part2 <- t(FF) %*% SU 
		yVar <- t(Bu) %*% ( part1 - part2 - t(part2) + UU ) %*% Bu
		ySE <- sqrt(yVar)
		outpt <-  rbind(outpt, data.frame(respCol = respCol, wtsCol = wtsCol[i], 
			Prediction = yEst, StandError = ySE))
	}
	names(outpt) <- c("Resp", "Wts", "Pred", "Pred SE")
	return(outpt)
}

