#' Estimate R-squared
#' Compute nonparametric estimate of R-squared.
#'
#' @param fitted_values fitted values from a regression function.
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated R-squared of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_r_squared <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", na.rm = FALSE, ...) {
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- measure_mse(fitted_values[C == 1], y[C == 1], na.rm = na.rm)
        obs_var <- mean(((y - mean(y, na.rm = na.rm)) ^ 2)[C == 1], na.rm = na.rm)
        obs_var_eif <- ((y - mean(y, na.rm = na.rm)) ^ 2)[C == 1] - obs_var
        obs_grad <- as.vector(matrix(c(1 / var, -obs_mse$point_est / (obs_var ^ 2)), nrow = 1) %*% t(cbind(obs_mse$ic, obs_var_eif)))
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = x[C == 1, , drop = FALSE], ...)
            ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        mse <- mean((C / ipc_weights) * (y - fitted_values) ^ 2, na.rm = na.rm)
        var <- mean((C / ipc_weights) * (y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        est <- (1 - mse / var) + mean(grad)
    } else {
        # point estimates of all components
        mse <- measure_mse(fitted_values, y, na.rm = na.rm)
        var <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        est <- 1 - mse$point_est/var
        # influence curves
        ic_var <- (y - mean(y, na.rm = na.rm))^2 - var
        grad <- as.vector(matrix(c(1/var, -mse$point_est/(var^2)), nrow = 1) %*% t(cbind(mse$ic, ic_var)))
    }
    return(list(point_est = est, ic = grad, ipc_eif_preds = ipc_eif_preds))
}
