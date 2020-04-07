#' Confidence intervals for measures of predictiveness
#'
#' Compute confidence intervals for the true measure of predictiveness.
#'
#' @param est estimate of predictiveness, e.g., from a call to \code{predictiveness_point_est}.
#' @param se estimate of the standard error of \code{est}, e.g., from a call to \code{vimp_se}.
#' @param level confidence interval type (defaults to 0.95).
#' @param one_sided should one-sided intervals be returned? (defaults to \code{FALSE})
#'
#' @return The Wald-based confidence interval for the true predictiveness of the given group of covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
predictiveness_ci <- function(est, se, level = 0.95, one_sided = FALSE) {
  ## set up the level\
  if (one_sided) {
      a <- (1 - level)/2
  } else {
      a <- 1 - level
  }
  a <- c(a, 1 - a)

  ## get the quantiles
  fac <- stats::qnorm(a)

  ## create the ci
  ci <- array(NA, dim = c(length(est), 2L), dimnames = list(names(est)))
  ci[] <- est + (se) %o% fac
  return(ci)
}
