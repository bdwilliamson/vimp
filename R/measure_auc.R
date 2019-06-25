#' Estimate area under the receiver operating characteristic curve (AUC)
#'
#' Compute nonparametric estimate of AUC.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return A named list of: (1) the estimated AUC of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_auc <- function(fitted_values, y, na.rm = FALSE) {
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)
    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)

    ## marginal probabilities
    p_0 <- mean(y == 0)
    p_1 <- mean(y == 1)

    ## sensitivity and specificity
    sens <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 0] < x, na.rm = na.rm)))
    spec <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 1] > x, na.rm = na.rm)))

    ## contributions from cases and controls
    contrib_1 <- (y == 1)/p_1*sens
    contrib_0 <- (y == 0)/p_0*spec

    ## gradient
    grad <- contrib_1 + contrib_0 - ((y == 0)/p_0 + (y == 1)/p_1)*est
    return(list(point_est = est, ic = grad))
}