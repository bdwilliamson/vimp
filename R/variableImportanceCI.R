#' Confidence intervals for variable importance
#'
#' Compute confidence intervals for the estimate of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param est estimate of variable importance from a call to \code{variableImportance}.
#' @param se estimate of the standard error of \code{est}, from a call to \code{variableImportanceSE}
#' @param n the sample size.
#' @param level confidence interval type (defaults to 0.95).
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
variableImportanceCI <- function(est, se, n, level = 0.95) {
  ## set up the level
  a <- (1 - level)/2
  a <- c(a, 1 - a)

  ## get the quantiles
  fac <- stats::qnorm(a)

  ## create the ci
  ci <- array(NA, dim = c(length(est), 2L), dimnames = list(names(est)))
  ci[] <- est + (se) %o% fac
  return(ci)
}
