#' Estimate standard errors
#'
#' Compute standard error estimates for estimates of variable importance.
#'
#' @param eif the estimated efficient influence function.
#' @param n the sample size.
#' @param na.rm logical; should NA's be removed in computation? 
#'   (defaults to \code{FALSE}).
#'
#' @return The standard error for the estimated variable importance for the 
#'   given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#'   details on the mathematics behind this function and the definition of the 
#'   parameter of interest.
#'
#' @importFrom stats complete.cases
#' @export
vimp_se <- function(eif, n = length(eif), na.rm = FALSE) {
    if (na.rm) {
        n <- length(eif[!is.na(eif)])
    }
    ic <- matrix(eif, ncol = 1)
    # compute the variance
    var <- as.numeric((t(ic) %*% ic) / n)
    # compute the se
    se <- sqrt(var / n)
    return(se)
}
