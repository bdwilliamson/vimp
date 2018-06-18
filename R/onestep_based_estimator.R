#' Estimate variable importance using a one-step estimator-based approach
#'
#' Compute nonparametric estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{regression}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
onestep_based_estimator <- function(full, reduced, y, type = "regression", na.rm = FALSE) {

  ## first calculate the naive
  if (type == "regression") {
    naive <- mean((full - reduced) ^ 2, na.rm = na.rm)/mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
  } else if (type == "deviance") { 
    p <- apply(y, 2, mean)
    naive_num <- 2*sum(diag(t(full)%*%log(full/reduced)))/dim(y)[1]
    naive_denom <- -1*sum(log(p))
    naive <- naive_num/naive_denom
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }

  ## now add on the mean of the ic
  onestep <- naive + mean(vimp_update(full, reduced, y, type, na.rm = na.rm), na.rm = na.rm)

  ## return
  ret <- c(onestep, naive)
  
  return(ret)
}
