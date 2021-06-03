#' Estimate area under the receiver operating characteristic curve (AUC)
#'
#' Compute nonparametric estimate of AUC.
#'
#' @param fitted_values fitted values from a regression function using the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#' @param y the observed outcome (may be within a specified fold, for
#'   cross-fitted estimates).
#' @param full_y the observed outcome (from the entire dataset, for
#'   cross-fitted estimates).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object
#'   containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g.,
#'   inverse weights from a two-phase sample) weighted estimation.
#'   Assumed to be already inverted
#'   (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL",
#'   fit a SuperLearner to determine the correction to the efficient
#'   influence function.
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values
#'   from a regression of the full-data EIF on the fully observed
#'   covariates/outcome; otherwise, not used.
#' @param ipc_est_type IPC correction, either \code{"ipw"} (for classical
#'   inverse probability weighting) or \code{"aipw"} (for augmented inverse
#'   probability weighting; the default).
#' @param scale if doing an IPC correction, then the scale that the correction
#'   should be computed on (e.g., "identity"; or "logit" to logit-transform,
#'   apply the correction, and back-transform).
#' @param na.rm logical; should \code{NA}s be removed in computation?
#'   (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A named list of: (1) the estimated AUC of the fitted regression
#' function; (2) the estimated influence function; and
#' (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @importFrom data.table data.table `:=`
#' @export
measure_auc <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "identity",
                        na.rm = FALSE, ...) {
    # bind "global vars" to pass R CMD check
    initial_rownums <- label <- pred <- sens <- spec <- NULL
    # compute the point estimate (on only data with all obs, if IPC
    # weights are entered)
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)
    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc",
                                    x.measure = "cutoff")@y.values)
    # compute sensitivity and specificity
    n_0 <- sum(y == 0)    
    n_1 <- sum(y == 1)
    n_0_weighted <- sum((y == 0) * ipc_weights[C == 1])
    n_1_weighted <- sum((y == 1) * ipc_weights[C == 1])
    if (is.null(full_y)) {
        p_0 <- mean(y == 0)
        p_1 <- mean(y == 1)
    } else {
        p_0 <- mean(full_y == 0)
        p_1 <- mean(full_y == 1)
    }
    dt <- data.table::data.table(pred = as.numeric(fitted_values), label = as.numeric(y), 
                                 initial_rownums = 1:length(as.numeric(y)))
    # sort by ascending pred within descending label, i.e., all Y = 1 followed by all Y = 0
    dt <- dt[order(pred, -xtfrm(label))]
    dt[, sens := cumsum(label == 0) / n_0]
    # sort by descending pred within ascending label
    dt <- dt[order(-pred, label)]
    dt[, spec := cumsum(label == 1) / n_1]
    dt <- dt[order(initial_rownums)]
    # compute the EIF: if there is coarsening, do a correction
    if (!all(ipc_weights == 1)) {
        # gradient
        obs_grad <- ((y == 0) / p_0) * (dt$spec - est) +
            ((y == 1) / p_1) * (dt$sens - est)
        # if IPC EIF preds aren't entered, estimate the regression
        if (ipc_fit_type != "external") {
            ipc_eif_mod <- SuperLearner::SuperLearner(
                Y = obs_grad, X = subset(Z, C == 1, drop = FALSE),
                method = "method.CC_LS", ...
            )
            ipc_eif_preds <- SuperLearner::predict.SuperLearner(
                ipc_eif_mod, newdata = Z, onlySL = TRUE
            )$pred
        }
        weighted_obs_grad <- rep(0, length(C))
        weighted_obs_grad[C == 1] <- obs_grad * ipc_weights[C == 1]
        grad <- weighted_obs_grad - (C * ipc_weights - 1) * ipc_eif_preds
        # compute weighted AUC
        pred_order <- order(as.numeric(fitted_values), decreasing = TRUE)
        ordered_preds <- as.numeric(fitted_values)[pred_order]
        tp <- cumsum(ipc_weights[C == 1][pred_order] * (y[pred_order] == 1))
        fp <- cumsum(ipc_weights[C == 1][pred_order] * (y[pred_order] == 0))
        dups <- rev(duplicated(rev(ordered_preds)))
        fp <- c(0, fp[!dups])
        tp <- c(0, tp[!dups])
        fp_x <- fp / n_0_weighted
        tp_y <- tp / n_1_weighted
        obs_est <- 0
        for (i in 2:length(fp_x)) {
            obs_est <- obs_est + 0.5 * (fp_x[i] - fp_x[i - 1]) * (tp_y[i] + tp_y[i - 1])
        }
        obs_est <- as.numeric(obs_est)
        if (ipc_est_type == "ipw") {
            est <- scale_est(obs_est, rep(0, length(grad)), scale = scale)
        } else {
            est <- scale_est(obs_est, grad, scale = scale)
        }
    } else {
        # gradient
        grad <- ((y == 0) / p_0) * (dt$spec - est) +
            ((y == 1) / p_1) * (dt$sens - est)
    }
    return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
