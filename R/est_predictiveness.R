#' Estimate a nonparametric predictiveness functional
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function using the 
#'   observed data.
#' @param y the observed outcome.
#' @param full_y the observed outcome (from the entire dataset, for 
#'   cross-fitted estimates).
#' @param type which parameter are you estimating (defaults to \code{r_squared}, 
#'   for R-squared-based variable importance)?
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes 
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object 
#'   containing the fully observed data.
#' @param ipc_weights weights for inverse probability of coarsening (e.g., 
#'   inverse weights from a two-phase sample) weighted estimation. Assumed to 
#'   be already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", 
#'   fit a SuperLearner to determine the correction to the efficient influence 
#'   function.
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values 
#'   from a regression of the full-data EIF on the fully observed 
#'   covariates/outcome; otherwise, not used.
#' @param ipc_est_type IPC correction, either \code{"ipw"} (for classical 
#'   inverse probability weighting) or \code{"aipw"} (for augmented inverse
#'   probability weighting; the default).
#' @param scale if doing an IPC correction, then the scale that the correction 
#'   should be computed on (e.g., "identity"; or "logit" to logit-transform, 
#'   apply the correction, and back-transform).
#' @param na.rm logical; should NA's be removed in computation? 
#'   (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return A list, with: the estimated predictiveness; the estimated efficient 
#'   influence function; and the predictions of the EIF based on inverse 
#'   probability of censoring.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#'   details on the mathematics behind this function and the definition of the 
#'   parameter of interest.
#' @export
est_predictiveness <- function(fitted_values, y, full_y = NULL, 
                               type = "r_squared", 
                               C = rep(1, length(y)), Z = NULL, 
                               ipc_weights = rep(1, length(C)), 
                               ipc_fit_type = "external", 
                               ipc_eif_preds = rep(1, length(C)), 
                               ipc_est_type = "aipw", scale = "identity", 
                               na.rm = FALSE, ...) {

    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", 
               "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop(
        paste0("We currently do not support the entered variable importance ", 
               "parameter.")
    )
    measure_funcs <- c(measure_accuracy, measure_auc, measure_deviance, 
                       measure_r_squared, NA, measure_mse, 
                       measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    # compute plug-in point estimate, EIF, inverse-weighted EIF predictions
    if (!is.na(measure_func)) {
        est_lst <- measure_func[[1]](
            fitted_values = fitted_values, y = y, full_y = full_y, C = C, Z = Z, 
            ipc_weights = ipc_weights, ipc_fit_type = ipc_fit_type, 
            ipc_eif_preds = ipc_eif_preds, ipc_est_type = ipc_est_type, 
            scale = scale, na.rm = na.rm, ...
        )
    } else { # if type is anova, no plug-in from predictiveness
        est_lst <- list(point_est = NA, ic = NA, 
                        ipc_eif_preds = rep(NA, length(y)))
    }
    # return it
    return(list(point_est = est_lst$point_est, eif = est_lst$eif, 
                ipc_eif_preds = est_lst$ipc_eif_preds))
}
