#' Estimate deviance-based variable importance using a one-step estimator-based approach
#'
#' Compute nonparametric estimates of deviance-based variable importance in multi-class problems, interpreted as the ratio of (1) the deviance between a full and reduced set of covariates and (2) the log-likelihood of the outcome.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates; an \code{n \times K} matrix.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates; a \code{n \times K} matrix.
#' @param y the outcome, a K-length vector of zeros and ones.
#' @param n the sample size.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @export
deviance_variable_importance <- function(full, reduced, y, n = length(y), na.rm = FALSE) {

  ## calculate the log-likelihood of each, based on the empirical

  ## calculate the naive
  naive_num <- -2*mean(apply(full*log(full/reduced), 1, sum, na.rm = na.rm), na.rm = na.rm)
  naive_denom <- -1*sum(log(probs))
  naive <- naive_num/naive_denom

  ## now add on the mean of the ic
  onestep <- naive + mean(deviance_ic(full, reduced, y, na.rm = na.rm), na.rm = na.rm)

  ## return
  ret <- onestep
  return(ret)
}
