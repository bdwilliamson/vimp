#' Construct a Predictiveness Measure
#'
#' @param type the measure of interest (e.g., "accuracy", "auc", "r_squared")
#' @param y the outcome of interest
#' @param a the exposure of interest (only used if \code{type = "average_value"})
#' @param fitted_values fitted values from a regression function using the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#' @param full_y the observed outcome (not used, defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object
#'   containing the fully observed data.
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
                                   a = NULL,
                                   fitted_values = numeric(),
                                   full_y = NULL,
                                   C = numeric(),
                                   Z = NULL,
                                   ipc_weights = numeric(),
                                   ipc_fit_type = character(),
                                   ipc_eif_preds = numeric(),
                                   ipc_est_type = character(),
                                   scale = character(),
                                   na.rm = logical(),
                                   ...) {
  validate_predictiveness_measure(new_predictiveness_measure(
    type = type, y = y, a = a, fitted_values = fitted_values,
    full_y = full_y, C = C, Z = Z, ipc_weights = ipc_weights,
    ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds,
    ipc_est_type = ipc_est_type, scale = scale, na.rm = na.rm, ...
  ))
}


new_predictiveness_measure <- function(type = character(),
                                       y = numeric(),
                                       a = NULL,
                                       fitted_values = numeric(),
                                       full_y = NULL,
                                       C = numeric(),
                                       Z = NULL,
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
  stopifnot(is.null(a) || (!is.null(a) & is.numeric(a)))
  stopifnot(is.numeric(fitted_values))
  stopifnot(is.numeric(C))
  stopifnot(is.numeric(ipc_weights))
  stopifnot(is.character(ipc_fit_type))
  stopifnot(is.numeric(ipc_eif_preds))
  stopifnot(is.character(ipc_est_type))
  stopifnot(scale %in% c("identity", "log", "logit"))
  stopifnot(is.logical(na.rm))

  arg_lst <- list(...)
  structure(
    c(list(y = y, a = a, fitted_values = fitted_values, full_y = full_y,
         C = C, Z = Z, ipc_weights = ipc_weights, ipc_eif_preds = ipc_eif_preds),
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

  if (length(input_data$y) != length(input_data$fitted_values)) {
    stop("The outcome data must have the same dimension as the fitted values",
         call. = FALSE)
  }
  if (!is.null(input_data$a)) {
    if (length(input_data$y) != length(input_data$a)) {
      stop("The outcome data must have the same dimension as the exposure data",
           call. = FALSE)
    }
  }
  if (length(input_data$ipc_weights) != length(input_data$y)) {
    stop("The outcome data must have the same dimension as the inverse probability weights",
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
