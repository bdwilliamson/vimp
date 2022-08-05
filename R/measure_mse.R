#' Estimate mean squared error
#'
#' Compute nonparametric estimate of mean squared error.
#'
#' @inheritParams measure_accuracy
#'
#' @return A named list of: (1) the estimated mean squared error of the fitted
#'   regression function; (2) the estimated influence function; and
#'   (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_mse <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "identity",
                        na.rm = FALSE, nuisance_estimators = NULL,
                        a = NULL, ...) {
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # observed mse
        obs_mse <- mean(((y - fitted_values) ^ 2), na.rm = na.rm)
        obs_grad <- ((y - fitted_values) ^ 2) - obs_mse
        # if IPC EIF preds aren't entered, estimate the regression
        ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                                 Z = Z, ipc_fit_type = ipc_fit_type,
                                                 ipc_eif_preds = ipc_eif_preds, ...)
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        obs_est <- mean((1 * ipc_weights[C == 1]) * (y - fitted_values) ^ 2,
                        na.rm = na.rm)
        if (ipc_est_type == "ipw") {
            est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
            est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        est <- mean((y - fitted_values)^2, na.rm = na.rm)
        # influence curves
        grad <- (y - fitted_values)^2 - est
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
