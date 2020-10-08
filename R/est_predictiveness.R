#' Estimate a nonparametric predictiveness functional
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{r_squared}, for R-squared-based variable importance)?
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either (i) NULL (the default, in which case the argument \code{C} above must be all ones), or (ii) a character list specifying the variable(s) among Y and X that are thought to play a role in the coarsening mechanism.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A list, with: the estimated predictiveness; the estimated efficient influence function; and the predictions of the EIF based on inverse probability of censoring.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
est_predictiveness <- function(fitted_values, y, type = "r_squared", x = NULL, C = rep(1, length(y)), Z = NULL, ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {

    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_deviance, measure_r_squared, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    # compute plug-in point estimate, EIF, inverse-weighted EIF predictions
    if (!is.na(measure_func)) {
        est_lst <- measure_func[[1]](fitted_values = fitted_values, y = y, x = x, C = C, ipc_weights = ipc_weights, ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds, na.rm = na.rm, ...)
    } else { # if type is anova, no plug-in from predictiveness
        est_lst <- list(point_est = NA, ic = NA, ipc_eif_preds = rep(NA, length(y)))
    }
    # return it
    return(list(point_est = est_lst$point_est, eif = est_lst$eif, ipc_eif_preds = est_lst$ipc_eif_preds))
}
