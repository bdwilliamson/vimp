#' Estimate the Influence Curve Based on Coarsening at Random
#' 
#' Estimate the influence function for the given measure of predictiveness, based on a coarsened-at-random data structure (e.g., two-phase sampling). 
#' 
#' @param fitted_values fitted values from a regression function: either a list of length V, where each object is a set of predictions on the validation data (for cross-fitted estimates); or a vector (for non-cross-fitted estimates).
#' @param y the outcome.
#' @param x the covariates.
#' @param C an indicator of whether the observation was censored (0) or observed (1).
#' @param eif_preds 
#' @param folds the cross-validation folds (not used if V = 1)
#' @param ipc_weights weights for the computed influence curve (i.e., inverse probability weights for coarsened-at-random settings)
#' @param type which risk parameter are you estimating (defaults to \code{r_squared}, for the $R^2$)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to the Super Learner, used for a regression of the fitted values on the observed data
#'
#' @return The estimated influence function values for the given measure of predictiveness, based on data from a two-phase sample.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
car_predictiveness_update <- function(fitted_values, y, x, C = rep(1, length(y)), eif_preds, folds = rep(1, length(y)), 
                       ipc_weights = rep(1, length(y)), type = "r_squared",
                       na.rm = FALSE, ...) {
  V <- length(unique(folds))
  # get the influence function based on the full data
  if (V > 1) {
    obs_fitted_values <- sapply(1:V, function(v) fitted_values[[v]][C[folds == v] == 1], simplify = FALSE)
    ic_full_obs <- cv_predictiveness_update(fitted_values = obs_fitted_values, y = y[C == 1], weights = rep(1, length(y))[C == 1],
                                            folds = folds[C == 1], type = type, na.rm = na.rm)
    # do the regression onto the observed data 
    eif_preds <- vector("list", length = V)
    for (v in 1:V) {
      eif_sl <- SuperLearner::SuperLearner(Y = y[(C == 1) & (folds != v)], X = x[(C == 1) & (folds != v), , drop = FALSE], ...)  
      eif_preds[[v]] <- predict(eif_sl, newdata = x[(C == 1) & (folds == v), , drop = FALSE])
    }
    ic <- sapply(1:V, function(v) (C / ipc_weights) * ic_full_obs - (C / ipc_weights - 1) * eif_preds[[v]], simplify = FALSE)
  } else {
    ic_full_obs <- predictiveness_update(fitted_values = fitted_values[C == 1], y = y[C == 1], weights = rep(1, length(y))[C == 1],
                                         type = type, na.rm = na.rm)
    # do the regression onto the observed data 
    eif_sl <- SuperLearner::SuperLearner(Y = y[C == 1], X = x[C == 1, , drop = FALSE], ...)
    eif_preds <- predict(eif_sl)$pred
    ic <- (C / ipc_weights) * ic_full_obs - (C / ipc_weights - 1) * eif_preds
  }
  # return the final influence function
  ic
}

