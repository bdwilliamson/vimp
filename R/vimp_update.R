#' Estimate the influence function for variable importance parameters
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param folds the folds for hypothesis testing.
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
vimp_update <- function(full, reduced, y, folds = folds, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (full_type == "regression") stop("Type 'regression' has been deprecated. Please enter type = 'anova' instead.")
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## get ICs
    ic_full <- predictiveness_update(full, y[folds == 1, , drop = FALSE], weights[folds == 1], full_type, na.rm)
    ic_redu <- predictiveness_update(reduced, y[folds == 2, , drop = FALSE], weights[folds == 2], full_type, na.rm)

    ## if type isn't anova, return
    if (full_type != "anova") {
        ## make sure that they have the same length
        len_full <- length(ic_full)
        len_redu <- length(ic_redu)
        if (len_full != len_redu) {
            max_length <- max(len_full, len_redu)
            ic_full <- c(ic_full, rep(0, max_length - len_full))
            ic_redu <- c(ic_redu, rep(0, max_length - len_redu))
        }
        ic <- ic_full - ic_redu
    } else {
        if (length(full) < length(reduced)) {
            full <- c(full, rep(NA, length(reduced) - length(full)))
        }
        if (length(reduced) < length(full)) {
            reduced <- c(reduced, rep(NA, length(reduced) - length(full)))
        }
        num <- mean((full - reduced) ^ 2, na.rm = na.rm)
        denom <- mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm )
        ic_num <- 2*(y[folds == 1, , drop = FALSE] - full)*(full - reduced) + (full - reduced) ^ 2 - num
        ic_denom <- (y[folds == 1] - mean(y, na.rm = na.rm)) ^ 2 - denom
        ic <- ic_num / denom - num / (denom ^ 2) * ic_denom
    }
    return(ic)
}
