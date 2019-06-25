#' Estimate mean squared error
#'
#' Compute nonparametric estimate of mean squared error.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated mean squared error of the fitted regression function.
#' @export
measure_mse <- function(fitted_values, y, na.rm = FALSE) {
    return(mean((y - fitted_values)^2, na.rm = na.rm))
}