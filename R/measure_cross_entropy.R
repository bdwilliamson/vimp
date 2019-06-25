#' Estimate the cross-entropy
#'
#' Compute nonparametric estimate of cross-entropy.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated cross-entropy of the fitted regression function.
#' @export
measure_cross_entropy <- function(fitted_values, y, na.rm = FALSE) {
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    if (is.null(dim(fitted_values))) { # assume predicting y = 1
      fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else if(dim(fitted_values)[2] < 2) {
        fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else {
        fitted_mat <- fitted_values
    }
    return(2*sum(diag(t(y_mult)%*%log(fitted_mat)), na.rm = na.rm)/dim(y_mult)[1])
}