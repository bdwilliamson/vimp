#' Estimate the classification accuracy
#'
#' Compute nonparametric estimate of classification accuracy.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights (IPW, etc.).
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated classification accuracy of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_accuracy <- function(fitted_values, y, weights = rep(1, length(y)), na.rm = FALSE) {
    est <- 1 - mean(weights*((fitted_values > 1/2) != y), na.rm = na.rm)
    grad <- weights*((-1)*(((fitted_values > 1/2) != y) - mean((fitted_values > 1/2) != y, na.rm = na.rm)))
    return(list(point_est = est, ic = grad))
}
