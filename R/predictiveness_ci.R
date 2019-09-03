#' Confidence intervals for measures of predictiveness
#'
#' Compute confidence intervals for the true measure of predictiveness.
#'
#' @param est estimate of predictiveness, e.g., from a call to \code{predictiveness_point_est}.
#' @param se estimate of the standard error of \code{est}, e.g., from a call to \code{vimp_se}.
#' @param level confidence interval type (defaults to 0.95).
#'
#' @return The Wald-based confidence interval for the true predictiveness of the given group of covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
predictiveness_ci <- function(est, se, level = 0.95) {
  ci <- vimp_ci(est, se, scale = "identity", level = level)
  return(ci)
}
