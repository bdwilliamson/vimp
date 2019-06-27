#' Estimate the deviance
#'
#' Compute nonparametric estimate of deviance.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated deviance of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_deviance <- function(fitted_values, y, na.rm = FALSE) {
    ## point estimates of all components
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
    p <- apply(y_mult, 2, mean, na.rm = na.rm)
    denom_point_est <- (-1)*sum(log(p))
    cross_entropy <- 2*sum(diag(t(y_mult)%*%log(fitted_mat)), na.rm = na.rm)/dim(y_mult)[1]
    est <- num/denom_point_est
    ## influence curve
    ic_denom <- rowSums(-1/p*((y_mult == 1) - p))
    ic_cross_entropy <- 2*rowSums(y_mult*log(fitted_mat), na.rm = na.rm) - est
    grad <- matrix(c(1/denom_point_est, -num/denom_point_est^2), nrow = 1)
    return(list(point_est = est, ic = grad%*%t(cbind(ic_num, ic_denom))))
}