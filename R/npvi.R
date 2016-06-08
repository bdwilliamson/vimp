#' Nonparametric Variable Importance
#'
#' This function computes estimates and confidence intervals for the nonparametric variable importance parameter of interest.
#'
#' @param f the regression function to estimate, in the usual \code{lm} format.
#' @param j the covariate(s) to calculate variable importance for, defaults to 1.
#' @param type the nonparametric estimation tool to use, defaults to \code{mgcv}. Partial strings can be used, if they are unique identifiers of the tool.
#' @param ... other arguments to the estimation tool.
#' @export

npvi <- function(f, j = 1, type = "mgcv", ...) {
  ## get the type, if not fully specified
  ## if not mgcv, loess, np, then stop and return error
  findx <-  pmatch(type,c("mgcv", "loess", "np"))
  if (is.na(findx)) stop("Unsupported estimation tool. Please supply a different tool.")
  type <- c("mgcv", "loess", "np")[findx]

  ##
}
