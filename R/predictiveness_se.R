#' Estimate standard errors for measures of predictiveness
#'
#' Compute standard error estimates for estimates of measures of predictiveness.
#'
#' @param est the estimate of variable importance.
#' @param update the influence curve-based update.
#' @param denom a list of point estimate and influence curve for the denominator (if any) to make the measure of predictiveness interpretable.
#' @param n the sample size.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE}).
#'
#' @return The standard error for the estimated measure of predictiveness for the given group of covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
predictiveness_se <- function(est, update, denom = NULL, n = length(update), na.rm = FALSE) {
    se <- vimp_se(est = est, update = update, denom = denom, n = n, scale = "identity", na.rm = na.rm)
    return(se)
}
