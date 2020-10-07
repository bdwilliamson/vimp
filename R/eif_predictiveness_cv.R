#' Estimate the influence function for an estimator of predictiveness
#'
#' Estimate the influence function for the given measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds the cross-validation folds
#' @param type which parameter are you estimating (defaults to \code{r_squared}, for R-squared-based variable importance)?
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return The estimated influence function values for the given measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
eif_predictiveness_cv <- function(fitted_values, y, folds, type = "r_squared", x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_cross_entropy, measure_mse, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    V <- length(unique(folds))
    max_nrow <- max(apply(matrix(1:V), 1, function(x) length(y[folds == x])))
    ics <- matrix(NA, nrow = max_nrow, ncol = V)
    ## calculate the necessary pieces for the influence curve
    if (!is.na(measure_func)) {
        ic <- vector("numeric", length(y))
        for (v in 1:V) {
            measures[[v]] <- measure_func[[1]](fitted_values = fitted_values[[v]], y = y[folds == v], x = x[folds == v, , drop =  FALSE], C = C[folds == v], ipc_weights = ipc_weights[folds == v], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds == v], na.rm = na.rm, ...)
            ics[1:length(y[folds == v]), v] <- measures[[v]]$ic
            ic[folds == v] <- measures[[v]]$ic
        }
        ipc_eif_preds <- sapply(1:V, function(v) measures[[v]]$ipc_eif_preds, simplify = FALSE)
    } else { # if type is anova, no plug-in from predictiveness
        ic <- rep(NA, length(y))
    }
    ## if full_type is "r_squared" or "deviance", post-hoc computing from "mse" or "cross_entropy"
    do_ipcw <- as.numeric(!all(ipc_weights == 1))
    mn_y <- mean(y, na.rm = na.rm)
    if (full_type == "r_squared") {
        var <- measure_mse(fitted_values = rep(mn_y, length(y)), y, x, C, ipc_weights, switch(do_ipcw + 1, ipc_fit_type, "SL"), ipc_eif_preds, na.rm = na.rm, ...)

        grad <- (-1) * as.vector(matrix(c(1 / var$point_est, -point_est / (var$point_est ^ 2)), nrow = 1) %*% t(cbind(ic, var$ic)))
        grads <- matrix(NA, nrow = max_nrow, ncol = V)
        for (v in 1:V) {
            grads[1:length(y[folds == v]), v] <- (-1) * as.vector(matrix(c(1 / var$point_est, -point_ests[v] / (var$point_est ^ 2)), nrow = 1) %*% t(cbind(ics[1:length(y[folds == v]), v], var$ic)))
        }
    }
    if (full_type == "deviance") {
        var <- measure_cross_entropy(fitted_values = rep(mn_y, length(y)), y, x, C, ipc_weights, switch(do_ipcw + 1, ipc_fit_type, "SL"), ipc_eif_preds, na.rm = na.rm, ...)

        grad <- (-1) * as.vector(matrix(c(1 / var$point_est, -point_est / (var$point_est ^ 2)), nrow = 1) %*% t(cbind(ic, var$ic)))
        grads <- matrix(NA, nrow = max_nrow, ncol = V)
        for (v in 1:V) {
            grads[1:length(y[folds == v]), v] <- (-1) * as.vector(matrix(c(1 / var$point_est, -point_ests[v] / (var$point_est ^ 2)), nrow = 1) %*% t(cbind(ics[1:length(y[folds == v]), v], var$ic)))
        }
    }
    return(list(ic = grad, all_ics = grads))
}
