#' Estimate the Influence Curve for VIMs Based on Coarsening at Random
#' 
#' Compute the value of the influence function for the given group of left-out covariates based on coarsened-at-random data structure.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; a list of length V, where each object is a set of predictions on the validation data.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates; a list of length V, where each object is a set of predictions on the validation data.
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
car_vimp_update <- function(full, reduced, y, x, C = rep(1, length(y)), folds = rep(1, length(y)), 
                                      ipc_weights = rep(1, length(y)), type = "r_squared",
                                      na.rm = FALSE, ...) {
  # get the influence function based on the full data
  if (length(unique(folds)) > 1) {
    obs_full <- sapply(1:V, function(v) full[[v]][C[folds == v] == 1], simplify = FALSE)
    obs_redu <- sapply(1:V, function(v) full[[v]][C[folds == v] == 1], simplify = FALSE)
    ic_full_obs <- cv_vimp_update(obs_full, obs_redu, y[C == 1], folds = folds[C == 1], weights = rep(1, sum(C == 1)),
                                  type = type, na.rm = na.rm)
  } else {
    ic_full_obs <- vimp_update(full[C == 1], reduced[C == 1], y[C == 1], folds = folds[C == 1],
                               weights = rep(1, sum(C == 1)), type = type, na.rm = na.rm)
  }
  # do the regression onto the observed data 
  
  # return the final influence function
  ic <- 
}