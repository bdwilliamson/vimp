#' Estimate mean squared error
#'
#' Compute nonparametric estimate of mean squared error.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights (IPW, etc.).
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated mean squared error of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_mse <- function(fitted_values, y, weights = rep(1, length(y)), na.rm = FALSE) {
    ## point estimates of all components
    mse <- mean(weights*(y - fitted_values)^2, na.rm = na.rm)
    ## influence curves
    ic_mse <- weights*(y - fitted_values)^2 - mse
    return(list(point_est = mse, ic = ic_mse))
}
