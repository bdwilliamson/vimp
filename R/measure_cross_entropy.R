#' Estimate the cross-entropy
#'
#' Compute nonparametric estimate of cross-entropy.
#'
#' @param fitted_values fitted values from a regression function using the
#'   observed data.
#' @param y the observed outcome.
#' @param full_y the observed outcome (not used, defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object
#'   containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g.,
#'   inverse weights from a two-phase sample) weighted estimation.
#'   Assumed to be already inverted
#'   (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL",
#'   fit a SuperLearner to determine the correction to the efficient
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
#' @return A named list of: (1) the estimated cross-entropy of the fitted
#'   regression function; (2) the estimated influence function; and
#'   (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_cross_entropy <- function(fitted_values, y, full_y = NULL, 
                                  C = rep(1, length(y)), Z = NULL, 
                                  ipc_weights = rep(1, length(y)),
                                  ipc_fit_type = "external",
                                  ipc_eif_preds = rep(1, length(y)),
                                  ipc_est_type = "aipw", scale = "identity",
                                  na.rm = FALSE, ...) {
    # point estimates of all components
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else if (dim(y)[2] < 2) {
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    if (is.null(dim(fitted_values))) { # assume predicting y = 1
      fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else if(dim(fitted_values)[2] < 2) {
        fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else {
        fitted_mat <- fitted_values
    }
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        obs_ce <- sum(diag(t(y_mult) %*% log(fitted_mat)),
                      na.rm = na.rm) / sum(C == 1)
        obs_grad <- rowSums(y_mult * log(fitted_mat), na.rm = na.rm) - obs_ce
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
        obs_est <- sum(diag(t(1 * ipc_weights[C == 1] * y_mult) %*%
                              log(fitted_mat)), na.rm = na.rm) / sum(C == 1)
        if (ipc_est_type == "ipw") {
          est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
          est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        cross_entropy <- sum(diag(t(y_mult)%*%log(fitted_mat)),
                             na.rm = na.rm)/dim(y_mult)[1]
        # influence curve
        grad <- rowSums(y_mult*log(fitted_mat), na.rm = na.rm) - cross_entropy
    }
    return(list(point_est = cross_entropy, eif = grad,
                ipc_eif_preds = ipc_eif_preds))
}
