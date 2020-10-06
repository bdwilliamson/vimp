#' Estimate area under the receiver operating characteristic curve (AUC)
#'
#' Compute nonparametric estimate of AUC.
#'
#' @param fitted_values fitted values from a regression function.
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated AUC of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_auc <- function(fitted_values, y, x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", na.rm = FALSE, ...) {
    # compute the point estimate (on only data with all obs, if IPC weights are entered)
    preds <- ROCR::prediction(predictions = fitted_values[C == 1], labels = y[C == 1])
    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # marginal probabilities
        p_0 <- mean(y[C == 1] == 0)
        p_1 <- mean(y[C == 1] == 1)
        # sensitivity and specificity
        sens <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[(y == 0) && (C == 1)] < x, na.rm = na.rm)))
        spec <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[(y == 1) && (C == 1)] > x, na.rm = na.rm)))

        # contributions from cases and controls
        contrib_1 <- (y == 1)/p_1*sens
        contrib_0 <- (y == 0)/p_0*spec

        # gradient
        obs_grad <- (contrib_1 + contrib_0 - ((y == 0)/p_0 + (y == 1)/p_1)*est)
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = x[C == 1, , drop = FALSE], ...)
            ipc_eif_preds <- predict(ipc_eif_mod)$pred
        }
        grad <- (C / ipc_weights) * obs_grad - (C / ipc_weights - 1) * ipc_eif_preds
        # one-step correction to the estimate
        cases <- y == 1
        controls <- y == 0
        case_control_comparison <- apply(matrix(fitted_values[cases]), 1, function(x) x >= fitted_values[controls])
        numerator <- sum(sweep(sweep(case_control_comparison, 2, C / ipc_weights[cases], "*"), 1, C / weights[controls], "*"))
        denominator <- sum(sweep(sweep(matrix(1, nrow = nrow(case_control_comparison), ncol = ncol(case_control_comparison)), 1, C / weights[controls], "*"), 2, C / weights[cases], "*"))
        est <- numerator/denominator + mean(grad)
    } else {
        # marginal probabilities
        p_0 <- mean(y == 0)
        p_1 <- mean(y == 1)

        # sensitivity and specificity
        sens <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 0] < x, na.rm = na.rm)))
        spec <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 1] > x, na.rm = na.rm)))

        # contributions from cases and controls
        contrib_1 <- (y == 1)/p_1*sens
        contrib_0 <- (y == 0)/p_0*spec

        # gradient
        grad <- (contrib_1 + contrib_0 - ((y == 0)/p_0 + (y == 1)/p_1)*est)
    }
    return(list(point_est = est, ic = grad, ipc_eif_preds = ipc_eif_preds))
}
