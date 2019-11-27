#' Estimate the influence function for variable importance parameters
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds a list of outer and inner folds (outer for hypothesis testing, inner for cross-validation)
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
    ic_full_lst <- cv_predictiveness_update(fitted_values = full, y = y[folds[[1]] == 1, , drop = FALSE], folds = folds[[2]][[1]], weights = weights[folds[[1]] == 1], type = full_type, na.rm = na.rm)
    ic_redu_lst <- cv_predictiveness_update(fitted_values = reduced, y = y[folds[[1]] == 2, , drop = FALSE], folds = folds[[2]][[2]], weights = weights[folds[[1]] == 2], type = full_type, na.rm = na.rm)
    ic_full <- ic_full_lst$ic
    ic_redu <- ic_redu_lst$ic
    ics_full <- ic_full_lst$all_ics
    ics_redu <- ic_redu_lst$all_ics

    ## if type isn't anova, return
    if (full_type != "anova") {
        ## make sure that they have the same length
        len_full <- length(ic_full)
        len_redu <- length(ic_redu)
        if (len_full != len_redu) {
            max_length <- max(len_full, len_redu)
            ic_full <- c(ic_full, rep(0, max_length - len_full))
            ic_redu <- c(ic_redu, rep(0, max_length - len_redu))
            dim_full <- dim(ics_full)
            dim_redu <- dim(ics_redu)
            max_dim <- max(dim_full[1], dim_redu[1])
            ics_full <- rbind(ics_full, matrix(0, nrow = max_dim - dim_full[1], ncol = dim_full[2]))
            ics_redu <- rbind(ics_redu, matrix(0, nrow = max_dim - dim_redu[1], ncol = dim_redu[2]))
        }
        ic <- ic_full - ic_redu
        ics <- ics_full - ics_redu
    } else {
        V <- length(unique(folds[[2]][[1]]))
        max_nrow <- max(apply(matrix(1:V), 1, function(x) length(y[folds[[1]] == 1, , drop = FALSE][folds[[2]][[1]] == x])))
        ics <- matrix(NA, nrow = max_nrow, ncol = V)
        ic <- vector("numeric", dim(y[folds[[1]] == 1, , drop = FALSE])[1])
        for (v in 1:V) {
            ics[1:length(y[folds[[1]] == 1, , drop = FALSE][folds[[2]][[1]] == v]), v] <- weights[folds[[1]] == 1][folds[[2]][[1]] == v]*(2*(y[folds[[1]] == 1, , drop = FALSE][folds[[2]][[1]] == v] - full[[v]])*(full[[v]] - reduced[[v]]) + (full[[v]] - reduced[[v]]) ^ 2 - mean((full[[v]] - reduced[[v]]) ^ 2, na.rm = na.rm))
            ic[folds[[2]][[1]] == v] <- ics[1:length(y[folds[[1]] == 1, , drop = FALSE][folds[[2]][[1]] == v]), v]
        }
    }
    return(list(ic = ic, all_ics = ics))
}
