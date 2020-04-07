#' Estimate R-squared
#' Compute nonparametric estimate of R-squared.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights (IPW, etc.).
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated R-squared of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_r_squared <- function(fitted_values, y, weights = rep(1, length(y)), na.rm = FALSE) {
    ## point estimates of all components
    mse <- mean(weights*(y - fitted_values)^2, na.rm = na.rm)
    var <- mean(weights*(y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    est <- 1 - mse/var
    ## influence curves
    ic_mse <- (y - fitted_values)^2 - mse
    ic_var <- (y - mean(y, na.rm = na.rm))^2 - var
    grad <- matrix(c(1/var, -mse/var^2), nrow = 1)
    return(list(point_est = est, ic = weights*as.vector(grad%*%t(cbind(ic_mse, ic_var)))))
}
