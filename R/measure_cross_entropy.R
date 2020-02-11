#' Estimate the cross-entropy
#'
#' Compute nonparametric estimate of cross-entropy.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights (IPW, etc.).
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated cross-entropy of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_cross_entropy <- function(fitted_values, y, weights = rep(1, length(y)), na.rm = FALSE) {
    ## point estimates of all components
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else if (dim(y)[2] < 2) {
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
    cross_entropy <- 2*sum(diag(t(weights*y_mult)%*%log(fitted_mat)), na.rm = na.rm)/dim(y_mult)[1]
    ## influence curve
    ic_cross_entropy <- weights*(2*rowSums(y_mult*log(fitted_mat), na.rm = na.rm) - cross_entropy)
    return(list(point_est = cross_entropy, ic = ic_cross_entropy))
}
