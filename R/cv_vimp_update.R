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
    if (full_type == "r_squared") full_type <- "mse"
    if (full_type == "deviance") full_type <- "cross_entropy"
    ic_full_lst <- cv_predictiveness_update(full, y, folds, weights, type, na.rm)
    ic_redu_lst <- cv_predictiveness_update(reduced, y, folds, weights, type, na.rm)
    ic_full <- ic_full_lst$ic
    ic_redu <- ic_redu_lst$ic
    ics_full <- ic_full_lst$all_ics
    ics_redu <- ic_redu_lst$all_ics

    ## if type isn't anova, return
    V <- length(unique(folds))
    if (full_type != "anova" & full_type != "mse" & full_type != "cross_entropy") {
        ic <- ic_full - ic_redu
        ics <- ics_full - ics_redu
    } else if (full_type == "cross_entropy") {
        ## denominator
        if (is.null(dim(y))) { # assume that zero is in first column
            y_mult <- cbind(1 - y, y)
        } else {
            y_mult <- y
        }
        p <- apply(y_mult, 2, mean, na.rm = na.rm)
        denom_point_est <- (-1)*sum(log(p))
        ic_denom <- rowSums(-1/p*((y_mult == 1) - p))

        ## cross-entropies
        cross_entropy_full <- cv_predictiveness_point_est(full, y, folds, full_type, na.rm)$point_est
        cross_entropy_redu <- cv_predictiveness_point_est(reduced, y, folds, full_type, na.rm)$point_est

        ## influence curve
        grad <- matrix(c(1/denom_point_est, -cross_entropy_full/denom_point_est^2,
                         -1/denom_point_est, cross_entropy_redu/denom_point_est^2), nrow = 1)
        ic <- as.vector(grad %*% t(cbind(ic_full, ic_denom, ic_redu, ic_denom)))
        ics <- cbind(as.vector(apply(matrix(1:V), 1, function(x) as.vector(grad %*% t(cbind(ics_full[, x], ic_denom, ics_redu[, x], ic_denom))))))
    } else if (full_type == "mse") {
        ## variance
        var <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        ic_var <- (y - mean(y, na.rm = na.rm))^2 - var

        ## mses
        mse_full <- cv_predictiveness_point_est(full, y, folds, full_type, na.rm)$point_est
        mse_redu <- cv_predictiveness_point_est(reduced, y, folds, full_type, na.rm)$point_est

        ## influence curve
        grad <- matrix(c(1/var, -mse_full/var^2, -1/var, mse_redu/var^2), nrow = 1)
        ic <- as.vector((-1)*(grad %*% t(cbind(ic_full, ic_var, ic_redu, ic_var))))
        ics <- cbind(as.vector(apply(matrix(1:V), 1, function(x) as.vector(grad %*% t(cbind(ics_full[, x], ic_denom, ics_redu[, x], ic_denom))))))
    } else {
        max_nrow <- max(apply(matrix(1:V), 1, function(x) length(y[folds == x])))
        ics <- matrix(NA, nrow = max_nrow, ncol = V)
        # ics_full <- matrix(NA, nrow = max_nrow, ncol = V)
        # ics_redu <- matrix(NA, nrow = max_nrow, ncol = V)
        # ## if r_squared or deviance, change to MSE or cross-entropy
        # if (full_type == "r_squared") full_type <- "mse"
        # if (full_type == "deviance") full_type <- "cross_entropy"
        for (v in 1:V) {
        #     ics_full[1:length(y[folds == v]), v] <- predictiveness_update(full[[v]], y[folds == v], weights[folds == v], full_type, na.rm)
        #     ics_redu[1:length(y[folds == v]), v] <- predictiveness_update(reduced[[v]], y[folds == v], weights[folds == v], full_type, na.rm)    
            ics[1:length(y[folds == v]), v] <- 2*(y[folds == v] - full[[v]])*(full[[v]] - reduced[[v]]) + (full[[v]] - reduced[[v]]) ^ 2 - mean((full[[v]] - reduced[[v]]) ^ 2, na.rm = na.rm)
        }
        # ic_full <- rowMeans(ics_full, na.rm = TRUE)
        # ic_redu <- rowMeans(ics_redu, na.rm = TRUE)
        ic <- rowMeans(ics, na.rm = TRUE)
    }
    return(list(ic = weights*ic, all_ics = ics))
}
