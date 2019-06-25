#' Confidence intervals for variable importance
#'
#' Compute confidence intervals for the true variable importance parameter.
#'
#' @param est estimate of variable importance, e.g., from a call to \code{vimp_point_est}.
#' @param se estimate of the standard error of \code{est}, e.g., from a call to \code{vimp_se}.
#' @param scale scale to compute interval estimate on (defaults to "log": compute SE and CI on log scale and back-transform).
#' @param level confidence interval type (defaults to 0.95).
#'
#' @return The Wald-based confidence interval for the true importance of the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
vimp_ci <- function(est, se, scale = "log", level = 0.95) {
    ## set up the level
    a <- (1 - level)/2
    a <- c(a, 1 - a)

    ## get the quantiles
    fac <- stats::qnorm(a)

    ## create the ci
    ci <- array(NA, dim = c(length(est), 2L), dimnames = list(names(est)))
    ## get scale
    scales <- c("log", "identity")
    full_scale <- scales[pmatch(scale, scales)]
    if (full_scale == "log") {
        ci[] <- 1 - exp(log(est) + (se) %o% rev(fac))
    } else {
        ci[] <- est + (se) %o% fac
    }
    
    return(ci)
}
