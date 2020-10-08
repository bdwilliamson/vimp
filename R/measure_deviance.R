#' Estimate the deviance
#'
#' Compute nonparametric estimate of deviance.
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
#' @return A named list of: (1) the estimated deviance of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_deviance <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    z_names <- names(Z)
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
        obs_pi_0 <- mean(y[C == 1], na.rm = na.rm)
        obs_denom <- measure_cross_entropy(fitted_values = rep(obs_pi_0, sum(C == 1)), y[C == 1], na.rm = na.rm)
        obs_grad <- as.vector(matrix(c(1 / obs_denom$point_est, obs_ce$point_est / (obs_denom$point_est ^ 2)), nrow = 1) %*% t(cbind(obs_ce$eif, obs_denom$eif)))
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
          df <- data.frame(y = y, x)[, z_names]
          ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = df, ...)
          ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        est <- 1 - measure_cross_entropy(fitted_values, C / ipc_weights * y, na.rm = na.rm)$point_est / measure_cross_entropy(fitted_values = mean(C / ipc_weights * y, na.rm = na.rm), C / ipc_weights * y, na.rm = na.rm) + mean(grad)
    } else {
        cross_entropy_meas <- measure_cross_entropy(fitted_values, y, na.rm = na.rm)
        pi_0 <- mean(y, na.rm = na.rm)
        denom <- measure_cross_entropy(fitted_values = rep(pi_0, length(y)), y, na.rm = na.rm)
        est <- 1 - cross_entropy_meas$point_est / denom$point_est
        grad <- as.vector(matrix(c(1/denom$point_est, -cross_entropy_meas$point_est/(denom$point_est^2)), nrow = 1) %*% t(cbind(cross_entropy_meas$eif, denom$eif)))
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
