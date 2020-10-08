#' Estimate ANOVA
#' Compute nonparametric estimate of the ANOVA decomposition.
#'
#' @param full fitted values from a regression function of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression on the reduced set of covariates.
#' @param y the outcome.
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either (i) NULL (the default, in which case the argument \code{C} above must be all ones), or (ii) a character vector specifying the variable(s) among y and x that are thought to play a role in the coarsening mechanism.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated ANOVA (based on a one-step correction) of the fitted regression functions; (2) the estimated influence function; (3) the naive ANOVA estimate; and (4) the IPC EIF predictions.
#' @importFrom data.table data.table as.data.table
#' @export
measure_anova <- function(full, reduced, y, x = NULL, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
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
        obs_num <- mean(((full - reduced) ^ 2)[C == 1], na.rm = na.rm)
        obs_denom <- mean(((y - mean(y, na.rm = na.rm)) ^ 2)[C == 1], na.rm = na.rm)
        obs_eif_num <- (2 * (y - full) * (full - reduced) + (full - reduced) ^ 2 - obs_num)[C == 1]
        obs_eif_denom <- ((y - mean(y, na.rm = TRUE)) ^ 2 - obs_denom)[C == 1]
        obs_grad <- obs_eif_num / obs_denom - obs_num / (obs_denom ^ 2) * obs_eif_denom
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            df <- get_dt(y, x, Z)
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, subset(df, C == 1), ...)
            ipc_eif_preds <- predict(ipc_eif_mod, df)$pred
        }
        weighted_obs_grad <- rep(0, length(y))
        weighted_obs_grad[C == 1] <- obs_grad / ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        num <- mean((C / ipc_weights) * ((full - reduced) ^ 2), na.rm = na.rm)
        denom <- mean((C / ipc_weights) * (y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        est <- num / denom + mean(grad)
    } else {
        num <- mean((full - reduced) ^ 2, na.rm = na.rm)
        denom <- mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        num_eif <- 2 * (y - full) * (full - reduced) + (full - reduced) ^ 2 - num
        denom_eif <- (y - mean(y, na.rm = TRUE)) ^ 2 - denom
        grad <- num_eif / denom - num / (denom ^ 2) * denom_eif
        est <- num / denom + mean(grad)
    }
    return(list(point_est = est, eif = grad, naive = num / denom, ipc_eif_preds = ipc_eif_preds))
}
