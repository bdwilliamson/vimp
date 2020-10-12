#' Estimate area under the receiver operating characteristic curve (AUC)
#'
#' Compute nonparametric estimate of AUC.
#'
#' @param fitted_values fitted values from a regression function using the observed data.
#' @param y the observed outcome.
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation. Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated AUC of the fitted regression function; (2) the estimated influence function; and (3) the IPC EIF predictions.
#' @export
measure_auc <- function(fitted_values, y, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    # compute the point estimate (on only data with all obs, if IPC weights are entered)
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)
    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # marginal probabilities
        p_0 <- mean(y == 0)
        p_1 <- mean(y == 1)
        # sensitivity and specificity
        sens <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[(y == 0)] < x, na.rm = na.rm)))
        spec <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[(y == 1)] > x, na.rm = na.rm)))

        # contributions from cases and controls
        contrib_1 <- (y == 1)/p_1*sens
        contrib_0 <- (y == 0)/p_0*spec

        # gradient
        obs_grad <- (contrib_1 + contrib_0 - ((y == 0)/p_0 + (y == 1)/p_1)*est)
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_grad, X = subset(Z, C == 1, drop = FALSE), ...)
            ipc_eif_preds <- predict(ipc_eif_mod, newdata = Z)$pred
        }
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        # one-step correction to the estimate
        cases <- y == 1
        controls <- y == 0
        case_control_comparison <- apply(matrix(fitted_values[cases]), 1, function(x) x >= fitted_values[controls])
        numerator <- sum(sweep(sweep(case_control_comparison, 2, 1 * ipc_weights[C == 1][cases], "*"), 1, 1 * ipc_weights[C == 1][controls], "*"))
        denominator <- sum(sweep(sweep(matrix(1, nrow = nrow(case_control_comparison), ncol = ncol(case_control_comparison)), 1, 1 * ipc_weights[C == 1][controls], "*"), 2, 1 * ipc_weights[C == 1][cases], "*"))
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
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
