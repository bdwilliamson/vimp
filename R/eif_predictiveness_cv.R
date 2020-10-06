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
eif_predictiveness_cv <- function(fitted_values, y, folds, type = "r_squared", x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", na.rm = FALSE, ...) {

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
            ics[1:length(y[folds == v]), v] <- measure_func[[1]](fitted_values = fitted_values[[v]], y = y[folds == v], x = x[folds == v, ], C = C[folds == v], ipc_weights = ipc_weights[folds == v], ipc_fit_type = ipc_fit_type, na.rm = na.rm, ...)$ic
            ic[folds == v] <- ics[1:length(y[folds == v]), v]
        }
        # ic <- rowMeans(ics, na.rm = TRUE)
    } else { # if type is anova, no plug-in from predictiveness
        ic <- rep(NA, length(y))
    }
    ## if full_type is "r_squared" or "deviance", post-hoc computing from "mse" or "cross_entropy"
    if (full_type == "r_squared") {
        denom_point_est <- mean(weights*(y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        denom_ic <- weights*((y - mean(y, na.rm = na.rm))^2 - denom_point_est)
        mse_lst <- cv_predictiveness_point_est(fitted_values = fitted_values, y = y, weights = weights, folds = folds, type = type, na.rm = na.rm)

        point_ests <- 1 - mse_lst$all_ests/denom_point_est
        point_est <- mean(point_ests)

        ## influence curve
        grad <- matrix(c(1/denom_point_est, -mse_lst$point_est/denom_point_est^2), nrow = 1)
        ic <- as.vector((-1)*(grad %*% t(cbind(ic, denom_ic))))
        grads <- cbind(1/denom_point_est, -mse_lst$all_ests/denom_point_est^2)
        tmp_ics <- matrix(NA, nrow = max_nrow, ncol = V)
        for (v in 1:V) {
            tmp_ics[1:length(y[folds == v]), v] <- as.vector((-1)*(grads[v, ] %*% t(cbind(ics[1:length(y[folds == v]), v], denom_ic[folds == v]))))
        }
        ics <- tmp_ics
    }
    if (full_type == "deviance") {
        if (is.null(dim(y)) | dim(y)[2] == 1) { # assume that zero is in first column
            y_mult <- cbind(1 - y, y)
        } else {
            y_mult <- y
        }
        p <- apply(weights*y_mult, 2, mean, na.rm = na.rm)
        denom_point_est <- (-1)*sum(log(p))
        denom_ic <- weights*rowSums(-1/p*((y_mult == 1) - p))
        ce_lst <- cv_predictiveness_point_est(fitted_values = fitted_values, y = y, weights = weights, folds = folds, type = type, na.rm = na.rm)

        point_ests <- ce_lst$all_ests/denom_point_est
        point_est <- mean(point_ests)

        ## influence curve
        grad <- matrix(c(1/denom_point_est, -ce_lst$point_est/denom_point_est^2), nrow = 1)
        ic <- as.vector(grad %*% t(cbind(ic, denom_ic)))
        grads <- cbind(1/denom_point_est, -ce_lst$all_ests/denom_point_est^2)
        tmp_ics <- matrix(NA, nrow = max_nrow, ncol = V)
        for (v in 1:V) {
            tmp_ics[1:length(y[folds == v]), v] <- as.vector(grads[v, ] %*% t(cbind(ics[1:length(y[folds == v]), v], denom_ic[folds == v])))
        }
        ics <- tmp_ics
    }
    return(list(ic = ic, all_ics = ics))
}
