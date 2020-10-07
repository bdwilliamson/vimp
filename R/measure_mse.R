#' Estimate mean squared error
#'
#' Compute nonparametric estimate of mean squared error.
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
#' @return A named list of: (1) the estimated mean squared error of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_mse <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- mean(((y - fitted_values) ^ 2)[C == 1], na.rm = na.rm)
        obs_grad <- ((y - fitted_values) ^ 2)[C == 1] - obs_mse
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            df <- data.frame(y = y[C == 1], x[C == 1, , drop = FALSE])
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, df, ...)
            ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        est <- mean((C / ipc_weights) * (y - fitted_values) ^ 2, na.rm = na.rm) + mean(grad)
    } else {
        mse <- mean((y - fitted_values)^2, na.rm = na.rm)
        # influence curves
        grad <- (y - fitted_values)^2 - mse
    }
    return(list(point_est = mse, ic = grad, ipc_eif_preds = ipc_eif_preds))
}
