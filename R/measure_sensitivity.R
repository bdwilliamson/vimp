#' Estimate the sensitivity
#'
#' Compute nonparametric estimate of sensitivity.
#'
#' @inheritParams measure_accuracy
#' @param cutoff The risk score cutoff at which the specificity is evaluated.
#' Fitted values above \code{cutoff} are interpreted as positive tests.
#'
#' @return A named list of: (1) the estimated sensitivity of the fitted regression
#' function using specified \code{cutoff}; (2) the estimated influence function; and
#' (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @importFrom data.table data.table `:=`
#' @export
measure_sensitivity <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "logit",
                        na.rm = FALSE, nuisance_estimators = NULL,
                        a = NULL, cutoff = 0.5, ...) {

    p_1 <- mean(y == 1)

    est <- sum(y == 1 & fitted_values > cutoff) / sum(y == 1)
    grad <- (y == 1 & fitted_values > cutoff)/p_1 - (y == 1)*est/(p_1)

    ipc_eif_preds <- NULL

    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
