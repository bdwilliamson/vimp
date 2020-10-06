#' Estimate variable importance using cross-validation
#'
#' Compute nonparametric estimates of the chosen variable importance parameter, with a correction for using data-adaptive techniques to estimate the conditional means only if necessary.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds a list of outer and inner folds (outer for hypothesis testing, inner for cross-validation)
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
est_vimp_cv <- function(full, reduced, y, folds, type = "r_squared", x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", na.rm = FALSE, ...) {
    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    V <- length(unique(folds))
    # compute plug-in point estimates of predictiveness
    point_est_full_lst <- est_predictiveness_cv(fitted_values = full, y = y[folds[[1]] == 1, , drop = FALSE], folds = folds[[2]][[1]], type = full_type, x = x[folds[[1]] == 1, , drop = FALSE], C = C[folds[[1]] == 1], ipc_weights = ipc_weights[folds[[1]] == 1], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds[[1]] == 1], na.rm = na.rm)
    point_est_redu_lst <- est_predictiveness_cv(fitted_values = reduced, y = y[folds[[1]] == 2, , drop = FALSE], folds = folds[[2]][[2]], type = full_type, x = x[folds[[1]] == 2, , drop = FALSE], C = C[folds[[1]] == 2], ipc_weights = ipc_weights[folds[[1]] == 2], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds[[1]] == 2], na.rm = na.rm)
    # if type isn't anova, return the plug-in; otherwise, get plug-in and corrected
    if (full_type != "anova") {
        point_est <- point_est_full_lst$point_est - point_est_redu_lst$point_est
        point_ests <- point_est_full_lst$all_ests - point_est_redu_lst$all_ests
        corrected_est <- NA
    } else {
        var <- mean(mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm))
        point_est <- mean(sapply(seq_len(V), function(v) mean((full[[v]] - reduced[[v]]) ^ 2, na.rm = na.rm))) / var
        corrected_est <- point_est + mean(cv_vimp_update(full, reduced, y, folds = folds, weights = weights, type = type, na.rm = na.rm)$ic, na.rm = na.rm)
        point_ests <- NA
    }
    return(list(point_est = c(corrected_est, point_est), all_ests = point_ests))
}
