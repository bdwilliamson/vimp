#' Estimate standard errors
#'
#' Compute standard error estimates for estimates of variable importance.
#'
#' @param est a list (e.g., from a call to \code{est_predictiveness}) with 
#'   the point estimate of variable importance and the estimated efficient 
#'   influence function.
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
vimp_se <- function(est, n = length(est$eif), na.rm = FALSE) {
    if (na.rm) {
        n <- length(est$eif[!is.na(est$eif)])
    }
    # get the influence curve; if est has an element named "all_eifs", use this
    if (!is.null(est$all_eifs)) {
        var <- mean(unlist(lapply(est$all_eifs, 
                           function(x) mean(x ^ 2, na.rm = na.rm))))
    } else {
        ic <- matrix(est$eif, ncol = 1)
        # compute the variance
        var <- (t(ic) %*% ic)/n
    }
    # compute the se
    se <- sqrt(var/n)
    return(se)
}
