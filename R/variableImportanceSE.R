#' Estimate standard errors
#'
#' Compute standard error estimates for estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param n the sample size.
#' @param standardized logical; should we estimate the standardized parameter? (defaults to \code{TRUE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
variableImportanceSE <- function(full, reduced, y, n = length(y), standardized = TRUE) {

  ## calculate the influence curve
  ic <- variableImportanceIC(full, reduced, y, standardized)

  ## calculate the variance
  var <- mean(ic ^ 2)

  ## calculate se
  # se <- sqrt(var)
  se <- sqrt(var/n)

  return(se)
}
