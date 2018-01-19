#' Estimate the influence function
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param standardized logical; should we estimate the standardized parameter? (defaults to \code{TRUE})
#'
#' @return The influence function values for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
variableImportanceIC <- function(full, reduced, y, standardized = TRUE, na.rm = TRUE) {

  ## first calculate the naive estimates of the parameters
  naive.j <- mean((full - reduced) ^ 2, na.rm)
  naive.var <- mean((y - mean(y))^2, na.rm)

  ## now calculate the influence curve using these
  if (standardized) {
    contrib.num <- (2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - naive.j)/naive.var
    contrib.denom <- ((y - mean(y))^2 - naive.var)*naive.j/(naive.var ^ 2)
    ret <-  contrib.num - contrib.denom
  } else {
    ret <- 2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - naive.j
    contrib.num <- ret
    contrib.denom <- NA
  }
  return(ret)
}
