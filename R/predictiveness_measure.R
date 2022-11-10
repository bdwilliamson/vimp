#' Construct a Predictiveness Measure
#'
#' @param type the measure of interest (e.g., "accuracy", "auc", "r_squared")
#' @param y the outcome of interest
#' @param a the exposure of interest (only used if \code{type = "average_value"})
#' @param fitted_values fitted values from a regression function using the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#' @param cross_fitting_folds folds for cross-fitting, if used to obtain the
#'   fitted values. If not used, a vector of ones.
#' @param full_y the observed outcome (not used, defaults to \code{NULL}).
#' @param nuisance_estimators a list of nuisance function estimators on the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#'   For the average value measure: an estimator of the optimal treatment rule (\code{f_n}); an estimator of the
#'   propensity score under the estimated optimal treatment rule (\code{g_n}); and an estimator
#'   of the outcome regression when treatment is assigned according to the estimated optimal rule (\code{q_n}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object
#'   containing the fully observed data.
#' @param folds_Z either the cross-validation folds for the observed data 
#'   (no coarsening) or a vector of folds for the fully observed data Z.
#' @param ipc_weights weights for inverse probability of coarsening (IPC)
#'   (e.g., inverse weights from a two-phase sample) weighted estimation.
#'   Assumed to be already inverted.
#'   (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL",
#'   fit a SuperLearner to determine the IPC correction to the efficient
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
#' @return An object of class \code{"predictiveness_measure"}, with the following
#'  attributes:
#' @export
predictiveness_measure <- function(type = character(),
                                   y = numeric(),
                                   a = numeric(),
                                   fitted_values = numeric(),
                                   cross_fitting_folds = rep(1, length(fitted_values)),
                                   full_y = NULL,
                                   nuisance_estimators = list(),
                                   C = rep(1, length(y)),
                                   Z = NULL,
                                   folds_Z = cross_fitting_folds,
                                   ipc_weights = rep(1, length(y)),
                                   ipc_fit_type = "SL",
                                   ipc_eif_preds = numeric(),
                                   ipc_est_type = "aipw",
                                   scale = "identity",
                                   na.rm = TRUE,
                                   ...) {
  validate_predictiveness_measure(new_predictiveness_measure(
    type = type, y = y, a = a, fitted_values = fitted_values, cross_fitting_folds = cross_fitting_folds,
    full_y = full_y, nuisance_estimators = nuisance_estimators, C = C, Z = Z, folds_Z = folds_Z,
    ipc_weights = ipc_weights, ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds,
    ipc_est_type = ipc_est_type, scale = scale, na.rm = na.rm, ...
  ))
}


new_predictiveness_measure <- function(type = character(),
                                       y = numeric(),
                                       a = numeric(),
                                       fitted_values = numeric(),
                                       cross_fitting_folds = numeric(),
                                       full_y = NULL,
                                       nuisance_estimators = list(),
                                       C = numeric(),
                                       Z = NULL,
                                       folds_Z = NULL,
                                       ipc_weights = numeric(),
                                       ipc_fit_type = character(),
                                       ipc_eif_preds = numeric(),
                                       ipc_est_type = character(),
                                       scale = character(),
                                       na.rm = logical(),
                                       ...) {
  stopifnot(type %in% c("accuracy", "anova", "auc", "average_value",
                        "cross_entropy", "deviance", "mse", "r_squared"))
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(fitted_values))
  stopifnot(is.numeric(cross_fitting_folds))
  stopifnot(is.numeric(C))
  stopifnot(is.numeric(ipc_weights))
  stopifnot(is.character(ipc_fit_type))
  stopifnot(is.numeric(ipc_eif_preds))
  stopifnot(is.character(ipc_est_type))
  stopifnot(scale %in% c("identity", "log", "logit"))
  stopifnot(is.logical(na.rm))

  arg_lst <- list(...)
  if (length(ipc_weights) == 0) {
    ipc_weights <- rep(1, length(y))
  }
  if (length(C) == 0) {
    C <- rep(1, length(y))
    folds_Z <- cross_fitting_folds
  }
  structure(
    c(list(y = y, a = a, fitted_values = fitted_values, cross_fitting_folds = cross_fitting_folds,
         K = length(unique(cross_fitting_folds)), full_y = full_y,
         nuisance_estimators = nuisance_estimators, point_est = NA, eif = rep(NA, length(y)),
         C = C, Z = Z, folds_Z = folds_Z, ipc_weights = ipc_weights, ipc_eif_preds = ipc_eif_preds),
      arg_lst),
    type = type, ipc_fit_type = ipc_fit_type, ipc_est_type = ipc_est_type,
    scale = scale, na.rm = na.rm,
    class = "predictiveness_measure"
  )
}

validate_predictiveness_measure <- function(x) {
  input_data <- unclass(x)
  type <- attr(x, "type")
  ipc_fit_type <- attr(x, "ipc_fit_type")
  ipc_est_type <- attr(x, "ipc_est_type")
  scale <- attr(x, "scale")
  na.rm <- attr(x, "na.rm")

  if (!any(grepl("average_value", type))) {
    if (length(input_data$y) != length(input_data$fitted_values)) {
      stop("The outcome data must have the same dimension as the fitted values",
           call. = FALSE)
    }
  } else {
    if (length(input_data$nuisance_estimators) == 0) {
      stop(paste0(
        "To estimate the average value, the following must be estimated:",
        " the optimal treatment rule (pass this in as named element f_n of the list);",
        " the propensity score under the optimal treatment rule (pass this in as named element g_n of the list);",
        " and the outcome regression when treatment is assigned according to the optimal rule (pass this in as named element q_n of the list)."
      ), call. = FALSE)
    } else {
      if (length(input_data$nuisance_estimators$f_n) != length(input_data$y)) {
        stop("The optimal treatment assignment must have the same dimension as the outcome.", call. = FALSE)
      }
      if (length(input_data$nuisance_estimators$g_n) != length(input_data$y)) {
        stop("The estimated propensity score must have the same dimension as the outcome.", call. = FALSE)
      }
      if (length(input_data$nuisance_estimators$q_n) != length(input_data$y)) {
        stop("The estimated outcome regression must have the same dimension as the outcome.", call. = FALSE)
      }
    }
  }
  if (length(input_data$cross_fitting_folds) != length(input_data$fitted_values)) {
    stop("If cross-fitting is desired, each observation must be put into a fold.")
  }
  if (length(input_data$a) != 0) {
    if (length(input_data$y) != length(input_data$a)) {
      stop("The outcome data must have the same dimension as the exposure data",
           call. = FALSE)
    }
  }
  if (length(input_data$ipc_weights) != length(input_data$C)) {
    stop("The full dataset must have the same dimension as the inverse probability weights",
         call. = FALSE)
  }
  if (!is.null(input_data$Z)) {
    if (nrow(input_data$Z) != length(input_data$C)) {
      stop("The data that are always measured (i.e., are not coarsened) must be the same dimension as the coarsening variable",
           call. = FALSE)
    }
  }
  x
}

is.predictiveness_measure <- function(x) {
  inherits(x, "predictiveness_measure")
}
