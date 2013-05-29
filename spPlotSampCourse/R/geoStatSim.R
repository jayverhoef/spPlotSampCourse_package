#-------------------------------------------------------------------------------
#
#           geostatSim
#
#-------------------------------------------------------------------------------

#' Simulate geostatistical data on set of given locations
#'
#' simulates geostatistical data on set of given locations
#'
#' @param xcoords a vector with x-coordinates
#' @param ycoords a vector with x-coordinates
#' @param parsil partial sill of autocorrelation model, default = 1
#' @param range range of autocorrelation model, default = 1
#' @param nugget range of autocorrelation model, default = 0
#' @param minorp proportion of range in x direction to that of y direction for unrotated anisotropic model, default = 1
#' @param rotate rotation of anisotropic axes, default = 0
#' @param CorModel autocorrelation model, default = "exponential".  Other possibilities are ,"expRadon2", "expRadon4", "gaussian", "stable", "rationalQuad", "cauchyGrav", "cauchyMag", "cauchy", "circular", "spherical", "cubic", "penta", "cardinalSine", "besselK", "besselJ"
#'
#' @return data.frame of three columns, the original xcoords and ycoords appended with a 3rd column of simulated geostatistical data
#'
#' @author Jay Ver Hoef
#' @export
geoStatSim <- function(xcoords, ycoords, parsil = 1, range = 1, nugget = 0,
	minorp = 1, rotate = 0, extrap = NULL, spModel = "exponential")
{
	n <- length(xcoords)
	dismat <- distGeoAni(xcoords, ycoords, xcoords, ycoords, rotate = rotate, 
		range = range, minorp = minorp) 
	# compute correlation matrix for scaled distance matrix
	if(spModel == "exponential") CorMat <- corModelExponential(dismat)
	if(spModel == "expRadon2") CorMat <- corModelExpRadon2(dismat)
	if(spModel == "expRadon4") CorMat <- corModelExpRadon4(dismat)
	if(spModel == "gaussian") CorMat <- corModelGaussian(dismat)
	if(spModel == "stable") CorMat <- corModelStable(dismat, extrap)
	if(spModel == "rationalQuad") CorMat <- corModelRationalQuad(dismat)
	if(spModel == "cauchyGrav") CorMat <- corModelCauchyGrav(dismat)
	if(spModel == "cauchyMag") CorMat <- corModelCauchyMag(dismat)
	if(spModel == "cauchy") CorMat <- corModelCauchy(dismat, extrap)
	if(spModel == "circular") CorMat <- corModelCircular(dismat)
	if(spModel == "spherical") CorMat <- corModelSpherical(dismat)
	if(spModel == "cubic") CorMat <- corModelCubic(dismat)
	if(spModel == "penta") CorMat <- corModelPenta(dismat)
	if(spModel == "cardinalSine") CorMat <- corModelCardinalSine(dismat)
	if(spModel == "besselK") CorMat <- corModelBesselK(dismat, extrap)
	if(spModel == "besselJ") CorMat <- corModelBesselJ(dismat, extrap)
	CovMat <- parsil*CorMat + diag(nugget, nrow = n, ncol = n)
	data.frame(x = xcoords, y = ycoords, z = t(chol(CovMat))%*%rnorm(n))
}

