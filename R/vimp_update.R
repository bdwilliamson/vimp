#' Estimate the influence function for variable importance parameters
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
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
vimp_update <- function(full, reduced, y, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {
    
    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## get ICs
    ic_full <- predictiveness_update(full, y, weights, full_type, na.rm)
    ic_redu <- predictiveness_update(reduced, y, weights, full_type, na.rm)

    ## if type isn't anova, return
    if (full_type != "anova") {
        ic <- ic_full - ic_redu
    } else {
        ic <- 2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - mean((full - reduced) ^ 2, na.rm = na.rm)
    }
    return(weights*ic)
}
