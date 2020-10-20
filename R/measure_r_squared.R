#' Estimate R-squared
#'
#' @param fitted_values fitted values from a regression function using the observed data.
#' @param y the observed outcome.
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation. Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param scale if doing an IPC correction, then the scale that the correction should be computed on (e.g., "identity"; or "logit" to logit-transform, apply the correction, and back-transform)
#' @param na.rm logical; should \code{NA}s be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated R-squared of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_r_squared <- function(fitted_values, y, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), scale = "identity", na.rm = FALSE, ...) {
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- measure_mse(fitted_values, y, na.rm = na.rm)
        obs_mn_y <- mean(y, na.rm = na.rm)
        obs_var <- measure_mse(fitted_values = rep(obs_mn_y, length(y)), y, na.rm = na.rm)
        obs_grad <- as.vector(matrix(c(1 / obs_var$point_est, -obs_mse$point_est / (obs_var$point_est ^ 2)), nrow = 1) %*% t(cbind(obs_mse$eif, obs_var$eif)))
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = subset(Z, C == 1, drop = FALSE), method = "method.CC_LS", ...)
            ipc_eif_preds <- SuperLearner::predict.SuperLearner(ipc_eif_mod, newdata = Z, onlySL = TRUE)$pred
        }
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        mse <- mean((1 * ipc_weights[C == 1]) * (y - fitted_values) ^ 2, na.rm = na.rm)
        var <- mean((1 * ipc_weights[C == 1]) * (y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        obs_est <- (1 - mse / var)
        est <- scale_est(obs_est, grad, scale = scale)
    } else {
        # point estimates of all components
        mse <- measure_mse(fitted_values, y, na.rm = na.rm)
        mn_y <- mean(y, na.rm = na.rm)
        var <- measure_mse(fitted_values = rep(mn_y, length(y)), y, na.rm = na.rm)
        est <- 1 - mse$point_est / var$point_est
        # influence curve
        grad <- (-1) * as.vector(matrix(c(1/var$point_est, -mse$point_est/(var$point_est^2)), nrow = 1) %*% t(cbind(mse$eif, var$eif)))
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
