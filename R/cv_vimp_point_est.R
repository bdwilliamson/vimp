#' Estimate variable importance using cross-validation
#'
#' Compute nonparametric estimates of the chosen variable importance parameter, with a correction for using data-adaptive techniques to estimate the conditional means only if necessary.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds the cross-validation folds
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
    point_ests_full <- vector("numeric", length = V)
    point_ests_redu <- vector("numeric", length = V)
    ## only do CV on MSE, cross-entropy if full_type is r_squared or deviance
    if (full_type == "r_squared") full_type <- "mse"
    if (full_type == "deviance") full_type <- "cross_entropy"
    for (v in 1:V) {
        point_ests_full[v] <- predictiveness_point_est(full[[v]], y[folds == v], full_type, na.rm)
        point_ests_redu[v] <- predictiveness_point_est(reduced[[v]], y[folds == v], full_type, na.rm)    
    }    

    ## if type isn't anova, return the plug-in; otherwise, get plug-in and corrected
    if (full_type != "anova" & full_type != "r_squared" & full_type != "deviance") {
        point_est <- point_est_full - point_est_redu
        corrected_est <- NA
    } else if (full_type == "deviance" ) {
        if (is.null(dim(y))) { # assume that zero is in first column
            y_mult <- cbind(1 - y, y)
        } else {
            y_mult <- y
        }
        p <- apply(y_mult, 2, mean, na.rm = na.rm)
        denom_point_est <- (-1)*sum(log(p))   
        cv_diff_cross_entropy <- (mean(point_ests_full) - mean(point_ests_redu))
        point_est <- cv_cross_entropy/denom_point_est
        corrected_est <- NA
    } else if (full_type == "r_squared") {
        cv_mse_full <- mean(point_ests_full)
        cv_mse_redu <- mean(point_ests_redu)
        denom_point_est <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        point_est <- (1 - cv_mse_full/denom_point_est) - (1 - cv_mse_redu/denom_point_est)
        corrected_est <- NA
    } else {
        point_est <- mean((full - reduced) ^ 2, na.rm = na.rm)
        corrected_est <- point_est + mean(vimp_update(full, reduced, y, weights = weights, type = type, na.rm = na.rm), na.rm = na.rm)   
    }
    return(c(corrected_est, point_est))
}
