#' Estimate R-squared
#' Compute nonparametric estimate of R-squared.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either (i) NULL (the default, in which case the argument \code{C} above must be all ones), or (ii) a character list specifying the variable(s) among Y and X that are thought to play a role in the coarsening mechanism.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated R-squared of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_r_squared <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    z_names <- names(Z)
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- measure_mse(fitted_values[C == 1], y[C == 1], na.rm = na.rm)
        obs_mn_y <- mean(y[C == 1], na.rm = na.rm)
        obs_var <- measure_mse(fitted_values = rep(obs_mn_y, sum(C == 1)), y[C == 1], na.rm = na.rm)
        obs_grad <- as.vector(matrix(c(1 / obs_var$point_est, -obs_mse$point_est / (obs_var$point_est ^ 2)), nrow = 1) %*% t(cbind(obs_mse$eif, obs_var$eif)))
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            df <- data.frame(y = y, x)[, z_names]
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, df, ...)
            ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- rep(0, length(y))
        grad[C == 1] <- obs_grad / ipc_weights[C == 1]
        grad[C == 0] <- (-1) * ipc_eif_preds
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        mse <- mean((C / ipc_weights) * (y - fitted_values) ^ 2, na.rm = na.rm)
        var <- mean((C / ipc_weights) * (y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        est <- (1 - mse / var) + mean(grad)
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
