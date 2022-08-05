#' Estimate R-squared
#'
#' @inheritParams measure_accuracy
#'
#' @return A named list of: (1) the estimated R-squared of the fitted regression
#'    function; (2) the estimated influence function; and
#'    (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_r_squared <- function(fitted_values, y, full_y = NULL,
                              C = rep(1, length(y)), Z = NULL,
                              ipc_weights = rep(1, length(y)),
                              ipc_fit_type = "external",
                              ipc_eif_preds = rep(1, length(y)),
                              ipc_est_type = "aipw", scale = "identity",
                              na.rm = FALSE, nuisance_estimators = NULL,
                              a = NULL, ...) {
    if (is.null(full_y)) {
        obs_mn_y <- mean(y, na.rm = na.rm)
    } else {
        obs_mn_y <- mean(full_y, na.rm = na.rm)
    }
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- measure_mse(fitted_values, y, na.rm = na.rm)
        obs_var <- measure_mse(
            fitted_values = rep(obs_mn_y, length(y)), y, na.rm = na.rm
        )
        obs_grad <- as.vector(
            matrix(c(1 / obs_var$point_est,
                     -obs_mse$point_est / (obs_var$point_est ^ 2)),
                   nrow = 1) %*% t(cbind(obs_mse$eif, obs_var$eif))
        )
        # if IPC EIF preds aren't entered, estimate the regression
        ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                                 Z = Z, ipc_fit_type = ipc_fit_type,
                                                 ipc_eif_preds = ipc_eif_preds, ...)
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        mse <- mean((1 * ipc_weights[C == 1]) * (y - fitted_values) ^ 2,
                    na.rm = na.rm)
        var <- mean((1 * ipc_weights[C == 1]) * (y - mean(y, na.rm = na.rm)) ^ 2,
                    na.rm = na.rm)
        obs_est <- (1 - mse / var)
        if (ipc_est_type == "ipw") {
            est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
            est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        # point estimates of all components
        mse <- measure_mse(fitted_values, y, na.rm = na.rm)
        var <- measure_mse(fitted_values = rep(obs_mn_y, length(y)), y,
                           na.rm = na.rm)
        est <- 1 - mse$point_est / var$point_est
        # influence curve
        grad <- (-1) * as.vector(
            matrix(c(1/var$point_est,
                     -mse$point_est/(var$point_est^2)),
                   nrow = 1) %*% t(cbind(mse$eif, var$eif))
        )
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
