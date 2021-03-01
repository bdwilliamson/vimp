#' Estimate the classification accuracy
#'
#' Compute nonparametric estimate of classification accuracy.
#'
#' @param fitted_values fitted values from a regression function using the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#' @param y the observed outcome (may be within a specified fold, for 
#'   cross-fitted estimates).
#' @param full_y the observed outcome (not used, defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes 
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object 
#'   containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (IPC) 
#'   (e.g., inverse weights from a two-phase sample) weighted estimation. 
#'   Assumed to be already inverted. 
#'   (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", 
#'   fit a SuperLearner to determine the IPC correction to the efficient 
#'   influence function.
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values 
#'   from a regression of the full-data EIF on the fully observed 
#'   covariates/outcome; otherwise, not used.
#' @param ipc_est_type IPC correction, either \code{"ipw"} (for classical 
#'   inverse probability weighting) or \code{"aipw"} (for augmented inverse
#'   probability weighting; the default).
#' @param scale if doing an IPC correction, then the scale that the correction 
#'   should be computed on (e.g., "identity"; or "logit" to logit-transform, 
#'   apply the correction, and back-transform).
#' @param na.rm logical; should \code{NA}s be removed in computation? 
#'   (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated classification accuracy of the 
#'   fitted regression function; (2) the estimated influence function; and 
#'   (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_accuracy <- function(fitted_values, y, full_y = NULL, 
                             C = rep(1, length(y)), Z = NULL, 
                             ipc_weights = rep(1, length(y)), 
                             ipc_fit_type = "external", 
                             ipc_eif_preds = rep(1, length(y)), 
                             ipc_est_type = "aipw", scale = "identity", 
                             na.rm = FALSE, ...) {
  # compute the EIF: if there is coarsening, do a correction
  if (!all(ipc_weights == 1)) {
    obs_grad <- ((fitted_values > 1/2) == y) - 
      mean((fitted_values > 1/2) == y, na.rm = na.rm)
    obs_est <- mean((1 * ipc_weights[C == 1]) * ((fitted_values > 1/2) == y), 
                    na.rm = na.rm)
    # if IPC EIF preds aren't entered, estimate the regression
    if (ipc_fit_type != "external") {
      ipc_eif_mod <- SuperLearner::SuperLearner(
        Y = obs_grad, X = subset(Z, C == 1, drop = FALSE), 
        method = "method.CC_LS", ...
      )
      ipc_eif_preds <- SuperLearner::predict.SuperLearner(
        ipc_eif_mod, newdata = Z, onlySL = TRUE
      )$pred
    }
    weighted_obs_grad <- rep(0, length(C))
    weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
    grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
    if (ipc_est_type == "ipw") {
      est <- scale_est(obs_est, rep(1, length(grad)), scale = scale)
    } else {
      est <- scale_est(obs_est, grad, scale = scale)  
    }
  } else {
    est <- mean(((fitted_values > 1/2) == y), na.rm = na.rm)
    grad <- ((fitted_values > 1/2) == y) - mean((fitted_values > 1/2) == y, 
                                                na.rm = na.rm)
  }
  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
