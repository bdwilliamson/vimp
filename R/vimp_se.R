#' Estimate standard errors
#'
#' Compute standard error estimates for estimates of variable importance.
#'
#' @param est the estimate of variable importance.
#' @param update the influence curve-based update.
#' @param denom a list of point estimate and influence curve for the denominator (if any) to make the measure of predictiveness interpretable.
#' @param n the sample size.
#' @param scale the scale to compute SEs on (either "log", for log-scale, or "identity", for same scale as point estimate).
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE}). 
#'
#' @return The standard error for the estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
vimp_se <- function(est, update, denom = NULL, n = length(update), scale = "log", na.rm = FALSE) {

    ## get scale
    scales <- c("log", "identity")
    full_scale <- scales[pmatch(scale, scales)]
    ## if denom is supplied, make a matrix with both ICs; otherwise, matrix with only regular IC
    if (!is.null(denom)) {
        ic <- cbind(update, denom$ic)
        if (full_scale == "log") {
            grad <- matrix(c(1/est, (-1)/denom$point_est), nrow = 1)    
        } else {
            grad <- matrix(c(1/denom$point_est, -est/(denom$point_est^2)), nrow = 1)
        }
    } else {
        ic <- matrix(update, ncol = 1)
        if (full_scale == "log") {
            grad <- matrix(1/est, nrow = 1)
        } else {
            grad <- matrix(1, nrow = 1)
        }
    }
    ## compute the variance
    var <- grad %*% t(ic) %*% ic %*% t(grad)
    
    ## calculate se
    if (na.rm) {
  	 n <- length(update[!is.na(update)])
    }
    se <- sqrt(var/n)
    return(se)
}
