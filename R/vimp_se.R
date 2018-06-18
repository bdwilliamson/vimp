#' Estimate standard errors
#'
#' Compute standard error estimates for estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param update the influence curve-based update
#' @param n the sample size
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE}) 
#'
#' @return The standard error for the estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
vimp_se <- function(update, n = length(update), na.rm = FALSE) {

  ## calculate the variance of the influence curve
  var <- mean(update ^ 2, na.rm = na.rm)

  ## calculate se
  if (na.rm) {
  	n <- length(update[!is.na(update)])
  }
  se <- sqrt(var/n)

  return(se)
}
