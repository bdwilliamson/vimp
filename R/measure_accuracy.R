#' Estimate the classification accuracy
#'
#' Compute nonparametric estimate of classification accuracy.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated classification accuracy of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_accuracy <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), 
                             ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), 
                             na.rm = FALSE, ...) {
  # compute the EIF: if there is coarsening, do a correction
  if ((length(unique(ipc_weights)) > 1)) {
    obs_grad <- ((-1)*(((fitted_values[C == 1] > 1/2) != y[C == 1]) - mean((fitted_values[C == 1] > 1/2) != y[C == 1], na.rm = na.rm)))
    # if IPC EIF preds aren't entered, estimate the regression
    if (ipc_fit_type != "external") {
      ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = x[C == 1, , drop = FALSE], ...)
      ipc_eif_preds <- predict(ipc_eif_mod)$pred
    } 
    grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
    est <- (1 - mean((C / ipc_weights) * ((fitted_values > 1/2) != y), na.rm = na.rm)) + mean(grad)
  } else {
    est <- 1 - mean(((fitted_values > 1/2) != y), na.rm = na.rm)
    grad <- ((-1)*(((fitted_values > 1/2) != y) - mean((fitted_values > 1/2) != y, na.rm = na.rm)))
  }
  return(list(point_est = est, ic = grad))
}
