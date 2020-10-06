#' Estimate variable importance
#'
#' Compute nonparametric estimates of the chosen variable importance parameter, with a correction for using data-adaptive techniques to estimate the conditional means only if necessary.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param folds the folds for hypothesis testing
#' @param type which parameter are you estimating (defaults to \code{r_squared}, for R-squared-based variable importance)?
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
est_vimp <- function(full, reduced, y, folds, type = "r_squared", x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    # compute plug-in point estimates of predictiveness
    point_est_full <- est_predictiveness(fitted_values = full, y = y[folds == 1, , drop = FALSE], type = full_type, x = x[folds == 1, , drop = FALSE], C = C[folds == 1], ipc_weights = ipc_weights[folds == 1], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds == 1], na.rm = na.rm, ...)
    point_est_redu <- est_predictiveness(fitted_values = reduced, y = y[folds == 2, , drop = FALSE], type = full_type, x = x[folds == 2, , drop = FALSE], C = C[folds == 2], ipc_weights = ipc_weights[folds == 2], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds == 2], na.rm = na.rm, ...)

    # if type isn't anova, return the plug-in; otherwise, get plug-in and corrected
    if (full_type != "anova") {
        point_est <- point_est_full - point_est_redu
        corrected_est <- NA
    } else {
        # point_est <- mean((full - reduced) ^ 2, na.rm = na.rm)/mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        # corrected_est <- point_est + mean(vimp_update(full, reduced, y, folds = folds, weights = weights, type = type, na.rm = na.rm), na.rm = na.rm)
        est <- measure_anova(full, reduced, y, x = x, C = C, ipc_weights = ipc_weights, ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds, na.rm = na.rm, ...)
        point_est <- est$naive
        corrected_est <- est$point_est
    }
    return(c(corrected_est, point_est))
}
