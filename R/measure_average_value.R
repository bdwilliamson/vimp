#' Estimate the average value under the optimal treatment rule
#'
#' Compute nonparametric estimate of the average value under the optimal
#' treatment rule.
#' @inheritParams measure_accuracy
#' @param nuisance_estimators a list of nuisance function estimators on the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#'   Specifically: an estimator of the optimal treatment rule; an estimator of the
#'   propensity score under the estimated optimal treatment rule; and an estimator
#'   of the outcome regression when treatment is assigned according to the estimated optimal rule.
#' @param a the observed treatment assignment (may be within a specified fold,
#'   for cross-fitted estimates).
#'
#' @return A named list of: (1) the estimated classification accuracy of the
#'   fitted regression function; (2) the estimated influence function; and
#'   (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_average_value <- function(nuisance_estimators, y, a, full_y = NULL,
                             C = rep(1, length(y)), Z = NULL,
                             ipc_weights = rep(1, length(y)),
                             ipc_fit_type = "external",
                             ipc_eif_preds = rep(1, length(y)),
                             ipc_est_type = "aipw", scale = "identity",
                             na.rm = FALSE, ...) {
  # compute the EIF: if there is coarsening, do a correction
  if (!all(ipc_weights == 1)) {
    obs_grad <- ((a == nuisance_estimators$f_n) / nuisance_estimators$g_n) *
      (y - nuisance_estimators$q_n) + nuisance_estimators$q_n - mean(nuisance_estimators$q_n)
    obs_est <- mean((1 * ipc_weights[C == 1]) * (obs_grad + mean(nuisance_estimators$q_n)))
    # if IPC EIF preds aren't entered, estimate the regression
    ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                             Z = Z, ipc_fit_type = ipc_fit_type,
                                             ipc_eif_preds = ipc_eif_preds, ...)
    weighted_obs_grad <- rep(0, length(C))
    weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
    grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
    if (ipc_est_type == "ipw") {
      est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
    } else {
      est <- scale_est(obs_est, grad, scale = scale)
    }
  } else {
    grad <- ((a == nuisance_estimators$f_n) / nuisance_estimators$g_n) *
      (y - nuisance_estimators$q_n) + nuisance_estimators$q_n - mean(nuisance_estimators$q_n)
    est <- mean(grad + mean(nuisance_estimators$q_n))
  }
  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
