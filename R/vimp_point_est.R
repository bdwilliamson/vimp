#' Estimate variable importance
#'
#' Compute nonparametric estimates of the chosen variable importance parameter, with a correction for using data-adaptive techniques to estimate the conditional means only if necessary.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param folds the folds for hypothesis testing
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
vimp_point_est <- function(full, reduced, y, folds, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## compute plug-in point estimates of predictiveness
    point_est_full <- predictiveness_point_est(fitted_values = full, y = y[folds == 1, , drop = FALSE], weights = weights[folds == 1], type = full_type, na.rm = na.rm)
    point_est_redu <- predictiveness_point_est(fitted_values = reduced, y = y[folds == 2, , drop = FALSE], weights = weights[folds == 2], type = full_type, na.rm = na.rm)

    ## if type isn't anova, return the plug-in; otherwise, get plug-in and corrected
    if (full_type != "anova") {
        point_est <- point_est_full - point_est_redu
        corrected_est <- NA
    } else {
        point_est <- mean((full - reduced) ^ 2, na.rm = na.rm)/mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
        corrected_est <- point_est + mean(vimp_update(full, reduced, y, folds = folds, weights = weights, type = type, na.rm = na.rm), na.rm = na.rm)
    }
    return(c(corrected_est, point_est))
}
