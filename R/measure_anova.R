#' Estimate ANOVA decomposition-based variable importance.
#'
#' @inheritParams measure_accuracy
#' @param full fitted values from a regression function of the observed outcome
#'   on the full set of covariates.
#' @param reduced fitted values from a regression on the reduced set of observed
#'   covariates.
#'
#' @return A named list of: (1) the estimated ANOVA (based on a one-step
#'   correction) of the fitted regression functions; (2) the estimated
#'   influence function; (3) the naive ANOVA estimate; and (4) the IPC EIF
#'   predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_anova <- function(full, reduced, y, full_y = NULL,
                          C = rep(1, length(y)), Z = NULL,
                          ipc_weights = rep(1, length(y)),
                          ipc_fit_type = "external",
                          ipc_eif_preds = rep(1, length(y)),
                          ipc_est_type = "aipw", scale = "logit",
                          na.rm = FALSE, nuisance_estimators = NULL,
                          a = NULL, ...) {
    if (is.null(full_y)) {
        obs_mn_y <- mean(y, na.rm = na.rm)
    } else {
        obs_mn_y <- mean(full_y, na.rm = na.rm)
    }
    # add on if they aren't equal length
    if (length(full) < length(reduced)) {
        full <- c(full, rep(NA, length(reduced) - length(full)))
    }
    if (length(reduced) < length(full)) {
        reduced <- c(reduced, rep(NA, length(reduced) - length(full)))
    }
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed full-data EIF
        obs_num <- mean(((full - reduced) ^ 2), na.rm = na.rm)
        obs_var <- measure_mse(
            fitted_values = rep(obs_mn_y, length(y)), y, na.rm = na.rm
        )
        obs_eif_num <- (2 * (y - full) * (full - reduced) +
                            (full - reduced) ^ 2 - obs_num)[C == 1]
        obs_grad <- obs_eif_num / obs_var$point_est -
            obs_num / (obs_var$point_est ^ 2) * obs_var$eif
        # if IPC EIF preds aren't entered, estimate the regression
        ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                                 Z = Z, ipc_fit_type = ipc_fit_type,
                                                 ipc_eif_preds = ipc_eif_preds, ...)
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        num <- mean((1 * ipc_weights[C == 1]) * ((full - reduced) ^ 2),
                    na.rm = na.rm)
        denom <- mean((1 * ipc_weights[C == 1]) *
                          (y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        obs_est <- num / denom
        if (ipc_est_type == "ipw") {
            est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
            est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        num <- mean((full - reduced) ^ 2, na.rm = na.rm)
        var <- measure_mse(fitted_values = rep(obs_mn_y, length(y)), y,
                           na.rm = na.rm)
        num_eif <- 2 * (y - full) * (full - reduced) +
            (full - reduced) ^ 2 - num
        grad <- num_eif / var$point_est - num / (var$point_est ^ 2) * var$eif
        est <- num / var$point_est + mean(grad)
    }
    return(list(point_est = est, eif = grad, naive = num / var$point_est,
                ipc_eif_preds = ipc_eif_preds))
}
