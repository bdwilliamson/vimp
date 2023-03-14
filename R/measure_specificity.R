#' Estimate the specificity
#'
#' Compute nonparametric estimate of specificity.
#'
#' @inheritParams measure_accuracy
#' @param cutoff The risk score cutoff at which the specificity is evaluated.
#' Fitted values above \code{cutoff} are interpreted as positive tests.
#'
#' @return A named list of: (1) the estimated specificity of the fitted regression
#' function using specified \code{cutoff}; (2) the estimated influence function; and
#' (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @importFrom data.table data.table `:=`
#' @export
measure_specificity <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "logit",
                        na.rm = FALSE, nuisance_estimators = NULL,
                        a = NULL, cutoff = 0.5, ...) {

    p_0 <- mean(y == 0)

    est <- sum(y == 0 & fitted_values <= cutoff) / sum(y == 0)
    grad <- (y == 0 & fitted_values <= cutoff)/p_0 - (y == 0)*est/(p_0)

    ipc_eif_preds <- NULL

    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
