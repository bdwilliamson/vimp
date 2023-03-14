#' Estimate the positive predictive value (NPV)
#'
#' Compute nonparametric estimate of NPV.
#'
#' @inheritParams measure_accuracy
#' @param cutoff The risk score cutoff at which the NPV is evaluated.
#' Fitted values above \code{cutoff} are interpreted as positive tests.
#'
#' @return A named list of: (1) the estimated NPV of the fitted regression
#' function using specified \code{cutoff}; (2) the estimated influence function; and
#' (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @importFrom data.table data.table `:=`
#' @export
measure_npv <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "logit",
                        na.rm = FALSE, nuisance_estimators = NULL,
                        a = NULL, cutoff = 0.5, ...) {

  theta_0 <- mean(fitted_values <= cutoff)

  est <- sum(y == 0 & fitted_values <= cutoff) / sum(fitted_values <= cutoff)
  grad <- (y == 0 & fitted_values <= cutoff)/theta_0 - (fitted_values <= cutoff)*est/(theta_0)

  ipc_eif_preds <- NULL

  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
