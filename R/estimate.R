#' Estimate a Predictiveness Measure
#'
#' Generic function for estimating a predictiveness measure (e.g., R-squared or classification accuracy).
#'
#' @param x An R object. Currently, there are methods for \code{predictiveness_measure} objects only.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
estimate <- function(x, ...) {
  UseMethod("estimate")
}
