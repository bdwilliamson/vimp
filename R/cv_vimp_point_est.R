#' Estimate variable importance using cross-validation
#'
#' Compute nonparametric estimates of the chosen variable importance parameter, with a correction for using data-adaptive techniques to estimate the conditional means only if necessary.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds a list of outer and inner folds (outer for hypothesis testing, inner for cross-validation)
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
cv_vimp_point_est <- function(full, reduced, y, folds, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    V <- length(unique(folds))
    ## compute plug-in point estimates of predictiveness
    point_est_full_lst <- cv_predictiveness_point_est(fitted_values = full, y = y[folds[[1]] == 1, , drop = FALSE], weights = weights[folds[[1]] == 1], folds = folds[[2]][[1]], type = full_type, na.rm = na.rm)
    point_est_redu_lst <- cv_predictiveness_point_est(fitted_values = reduced, y = y[folds[[1]] == 2, , drop = FALSE], weights = weights[folds[[1]] == 2], folds = folds[[2]][[2]], type = full_type, na.rm = na.rm)
    ## if type isn't anova, return the plug-in; otherwise, get plug-in and corrected
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
