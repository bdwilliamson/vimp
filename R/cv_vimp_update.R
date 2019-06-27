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
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## get ICs
    V <- length(unique(folds))
    ics_full <- vector("numeric", V)
    ics_redu <- vector("numeric", V)
    ## if r_squared or deviance, change to MSE or cross-entropy
    if (full_type == "r_squared") full_type <- "mse"
    if (full_type == "deviance") full_type <- "cross_entropy"
    for (v in 1:V) {
        ics_full[v] <- predictiveness_update(full[[v]], y[folds == v], weights, full_type, na.rm)
        ics_redu[v] <- predictiveness_update(reduced[[v]], y[folds == v], weights, full_type, na.rm)    
    }
    ic_full <- mean(ics_full)
    ic_redu <- mean(ic_redu)

    ## if type isn't anova, return
    if (full_type != "anova" & full_type != "mse" & full_type != "cross_entropy") {
        ic <- ic_full - ic_redu
    } else if (full_type == "cross_entropy") {
        ic <- 
    } else if (full_type == "mse") {
        var <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        mse <- cv_predictiveness_point_est()
    } else {
        ic <- 2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - mean((full - reduced) ^ 2, na.rm = na.rm)
    }
    return(weights*ic)
}
