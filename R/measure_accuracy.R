#' Estimate the classification accuracy
#'
#' Compute nonparametric estimate of classification accuracy.
#'
#' @param fitted_values fitted values from a regression function using the observed data.
#' @param y the observed outcome.
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated classification accuracy of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @importFrom data.table data.table as.data.table
#' @export
measure_accuracy <- function(fitted_values, y, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
  # compute the EIF: if there is coarsening, do a correction
  if (!all(ipc_weights == 1)) {
    obs_grad <- ((-1)*(((fitted_values > 1/2) != y) - mean((fitted_values > 1/2) != y, na.rm = na.rm)))
    # if IPC EIF preds aren't entered, estimate the regression
    if (ipc_fit_type != "external") {
      ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, subset(Z, C == 1, drop = FALSE), ...)
      ipc_eif_preds <- predict(ipc_eif_mod, newdata = Z)$pred
    }
    weighted_obs_grad <- rep(0, length(C))
    weighted_obs_grad[C == 1] <- obs_grad / ipc_weights[C == 1]
    grad <- weighted_obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
    est <- (1 - mean((1 / ipc_weights[C == 1]) * ((fitted_values > 1/2) != y), na.rm = na.rm)) + mean(grad)
  } else {
    est <- 1 - mean(((fitted_values > 1/2) != y), na.rm = na.rm)
    grad <- ((-1)*(((fitted_values > 1/2) != y) - mean((fitted_values > 1/2) != y, na.rm = na.rm)))
  }
  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}