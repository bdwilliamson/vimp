#' Estimate area under the receiver operating characteristic curve (AUC)
#'
#' Compute nonparametric estimate of AUC.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated AUC of the fitted regression function.
#' @export
measure_auc <- function(fitted_values, y, na.rm = FALSE) {
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)
    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)
    return(est)
}