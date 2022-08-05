#' Estimate the cross-entropy
#'
#' Compute nonparametric estimate of cross-entropy.
#'
#' @inheritParams measure_accuracy
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
                                  na.rm = FALSE, nuisance_estimators = NULL,
                                  a = NULL, ...) {
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
        ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                                 Z = Z, ipc_fit_type = ipc_fit_type,
                                                 ipc_eif_preds = ipc_eif_preds, ...)
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
