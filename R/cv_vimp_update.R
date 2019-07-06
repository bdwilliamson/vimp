#' Estimate the influence function for variable importance parameters
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return The influence function values for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
cv_vimp_update <- function(full, reduced, y, folds, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## get ICs
    ic_full_lst <- cv_predictiveness_update(fitted_values = full, y = y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)
    ic_redu_lst <- cv_predictiveness_update(fitted_values = reduced, y = y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)
    ic_full <- ic_full_lst$ic
    ic_redu <- ic_redu_lst$ic
    ics_full <- ic_full_lst$all_ics
    ics_redu <- ic_redu_lst$all_ics

    ## if type isn't anova, return
    if (full_type != "anova") {
        ic <- ic_full - ic_redu
        ics <- ics_full - ics_redu
    } else {
        V <- length(unique(folds))
        max_nrow <- max(apply(matrix(1:V), 1, function(x) length(y[folds == x])))
        ics <- matrix(NA, nrow = max_nrow, ncol = V)
        for (v in 1:V) {
            ics[1:length(y[folds == v]), v] <- weights[folds == v]*(2*(y[folds == v] - full[[v]])*(full[[v]] - reduced[[v]]) + (full[[v]] - reduced[[v]]) ^ 2 - mean((full[[v]] - reduced[[v]]) ^ 2, na.rm = na.rm))
        }
        ic <- rowMeans(ics, na.rm = TRUE)
    }
    return(list(ic = ic, all_ics = ics))
}
