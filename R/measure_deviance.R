#' Estimate the deviance
#'
#' Compute nonparametric estimate of deviance.
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
#' @return A named list of: (1) the estimated deviance of the fitted regression function, and (2) the estimated influence function.
#' @export
measure_deviance <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    # point estimates of all components
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else if (dim(y)[2] == 1) {
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # get full-data gradient on fully-observed data
        obs_ce <- measure_cross_entropy(fitted_values[C == 1], y[C == 1], na.rm = na.rm)
        obs_p <- colMeans(y_mult[C == 1, ], na.rm = TRUE)
        obs_denom <- (-1) * sum(log(obs_p))
        obs_ic_denom <- rowSums(-1 / obs_p * ((y_mult[C == 1, ] == 1) - obs_p))
        obs_grad <- as.vector(matrix(c(1 / obs_denom, obs_ce$point_est / (obs_denom ^ 2)), nrow = 1) %*% t(cbind(obs_ce$ic, obs_ic_denom)))
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
          ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = x[C == 1, , drop = FALSE], ...)
          ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        est <- measure_cross_entropy(fitted_values, C / ipc_weights * y, na.rm = na.rm)$point_est / ((-1) * sum(log(colMeans(C / ipc_weights * y_mult, na.rm = na.rm)))) + mean(grad)
    } else {
        cross_entropy_meas <- measure_cross_entropy(fitted_values, y, na.rm = na.rm)
        p <- apply(y_mult, 2, mean, na.rm = na.rm)
        denom_point_est <- (-1)*sum(log(p))
        est <- cross_entropy_meas$point_est / denom_point_est
        ic_denom <- rowSums(-1/p*((y_mult == 1) - p))
        grad <- as.vector(matrix(c(1/denom_point_est, -cross_entropy_meas$point_est/(denom_point_est^2)), nrow = 1) %*% t(cbind(cross_entropy_meas$ic, ic_denom)))
    }
    return(list(point_est = est, ic = grad))
}
