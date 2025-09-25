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
#' @export
measure_sensitivity <- function(fitted_values, y, full_y = NULL,
                                C = rep(1, length(y)), Z = NULL,
                                ipc_weights = rep(1, length(y)),
                                ipc_fit_type = "external",
                                ipc_eif_preds = rep(1, length(y)),
                                ipc_est_type = "aipw", scale = "logit",
                                na.rm = FALSE, nuisance_estimators = NULL,
                                a = NULL, cutoff = 0.5, ...) {

  if (!all(ipc_weights == 1)) {
    p_1 <- mean(y == 1, na.rm = na.rm)
    temp_est <- sum(y == 1 & fitted_values > cutoff, na.rm = na.rm) / sum(y == 1, na.rm = na.rm)
    obs_grad <- (y == 1 & fitted_values > cutoff)/p_1 - (y == 1)*temp_est/(p_1)
    obs_est <-  sum((1 * ipc_weights[C == 1]) * (y == 1 & fitted_values > cutoff), na.rm = na.rm) /
      sum((1 * ipc_weights[C == 1]) * (y == 1), na.rm = na.rm)

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
  } else{
    p_1 <- mean(y == 1, na.rm = na.rm)
    est <- sum(y == 1 & fitted_values > cutoff, na.rm = na.rm) / sum(y == 1, na.rm = na.rm)
    grad <- (y == 1 & fitted_values > cutoff)/p_1 - (y == 1)*est/(p_1)
  }

  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
