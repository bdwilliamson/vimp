#' Estimate the deviance
#'
#' Compute nonparametric estimate of deviance.
#'
#' @inheritParams measure_accuracy
#'
#' @return A named list of: (1) the estimated deviance of the fitted regression
#'   function; (2) the estimated influence function; and
#'   (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @export
measure_deviance <- function(fitted_values, y, full_y = NULL,
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
    } else if (dim(y)[2] == 1) {
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    # estimate the probability of observing a "case"
    if (is.null(full_y)) {
        pi_0 <- mean(y, na.rm = na.rm)
    } else {
        pi_0 <- mean(full_y, na.rm = na.rm)
    }
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # get full-data gradient on fully-observed data
        obs_ce <- measure_cross_entropy(fitted_values, y, na.rm = na.rm)
        obs_denom <- measure_cross_entropy(
            fitted_values = rep(pi_0, length(y)), y, na.rm = na.rm
        )
        obs_grad <- as.vector(
            matrix(c(1 / obs_denom$point_est,
                     obs_ce$point_est / (obs_denom$point_est ^ 2)),
                   nrow = 1) %*% t(cbind(obs_ce$eif, obs_denom$eif))
        )
        # if IPC EIF preds aren't entered, estimate the regression
        ipc_eif_preds <- estimate_eif_projection(obs_grad = obs_grad, C = C,
                                                 Z = Z, ipc_fit_type = ipc_fit_type,
                                                 ipc_eif_preds = ipc_eif_preds, ...)
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        obs_est <- 1 - measure_cross_entropy(
            fitted_values, 1 * ipc_weights[C == 1] * y, na.rm = na.rm
        )$point_est / measure_cross_entropy(
            fitted_values = mean(1 * ipc_weights[C == 1] * y, na.rm = na.rm),
            1 * ipc_weights[C == 1] * y, na.rm = na.rm
        )
        if (ipc_est_type == "ipw") {
            est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
            est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        cross_entropy_meas <- measure_cross_entropy(
            fitted_values, y, na.rm = na.rm
        )
        denom <- measure_cross_entropy(
            fitted_values = rep(pi_0, length(y)), y, na.rm = na.rm
        )
        est <- 1 - cross_entropy_meas$point_est / denom$point_est
        grad <- as.vector(
            matrix(
                c(1/denom$point_est,
                  -cross_entropy_meas$point_est/(denom$point_est^2)), nrow = 1
            ) %*% t(cbind(cross_entropy_meas$eif, denom$eif))
        )
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
