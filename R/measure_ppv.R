#' Estimate the positive predictive value (PPV)
#'
#' Compute nonparametric estimate of PPV.
#'
#' @inheritParams measure_accuracy
#' @param cutoff The risk score cutoff at which the PPV is evaluated.
#' Fitted values above \code{cutoff} are interpreted as positive tests.
#'
#' @return A named list of: (1) the estimated PPV of the fitted regression
#' function using specified \code{cutoff}; (2) the estimated influence function; and
#' (3) the IPC EIF predictions.
#' @importFrom SuperLearner predict.SuperLearner SuperLearner
#' @importFrom data.table data.table `:=`
#' @export
measure_ppv <- function(fitted_values, y, full_y = NULL,
                        C = rep(1, length(y)), Z = NULL,
                        ipc_weights = rep(1, length(y)),
                        ipc_fit_type = "external",
                        ipc_eif_preds = rep(1, length(y)),
                        ipc_est_type = "aipw", scale = "logit",
                        na.rm = FALSE, nuisance_estimators = NULL,
                        a = NULL, cutoff = 0.5, ...) {

  theta_1 <- mean(fitted_values > cutoff)

  est <- sum(y == 1 & fitted_values > cutoff) / sum(fitted_values > cutoff)
  grad <- (y == 1 & fitted_values > cutoff)/theta_1 - (fitted_values > cutoff)*est/(theta_1)

  ipc_eif_preds <- NULL

  return(list(point_est = est, eif = grad, ipc_eif_preds = ipc_eif_preds))
}
