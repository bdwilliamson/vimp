#' Estimate variable importance using a TMLE-based approach
#'
#' Compute nonparametric estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param full fitted values from an initial regression of the outcome on the full set of covariates.
#' @param reduced fitted values from an initial regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome variable
#' @param x the covariates (ordered)
#' @param s the indices for variable importance
#' @param lib the library of candidate learners passed to SuperLearner
#' @param tol the tolerance level for convergence
#' @param ... other arguments, passed to SuperLearner
#'
#' @return The estimated variable importance for the given group of left-out covariates, along with the SE and a CI.
#'
#' @details This differs from estimates returned by variableImportance() in that the procedure used to target the estimate to the parameter of interest is based on a TMLE approach.
#' @export
variableImportanceTMLE <- function(full, reduced, y, x, s, lib, tol = .Machine$double.eps, max.iter = 500, ...) {
    ## helper functions
    logit <- function(x) log(x/(1-x))
    expit <- function(x) exp(x)/(1+exp(x))
    ## initialize the epsilon return vector
    epss <- vector("numeric", max.iter)

    ## change y to between 0 and 1 if it isn't already
    if (max(y) > 1 | min(y) < 0) {
        ystar <- (y - min(y))/(max(y) - min(y))
    } else {
        ystar <- y
    }

    ## calculate the covariate
    covar <- full - reduced

    ## get initial epsilon (with no intercept)
    eps.init <- suppressWarnings(glm(ystar ~ covar - 1, offset = full, family = binomial(link = "logit"))$coefficients[1])
    epss[1] <- eps.init
    ## get update
    new.f <- expit(logit(full) + eps.init*covar)
    new.r <- SuperLearner::SuperLearner(Y = new.f, X = x[-s], family = gaussian(), SL.library = lib, ...)$SL.predict

    ## now repeat until convergence
    if (eps.init == 0) {
        f <- new.f
        r <- new.r
        eps <- eps.init
        return(list(est = est, full = new.f, reduced = new.r, eps = eps.init))
    } else {
        f <- new.f
        r <- new.r
        eps <- eps.init
        k <- 1
        while(abs(eps) > tol | k < max.iter) {
            ## get the covariate
            covar <- f - r
            ## update epsilon
            eps <- suppressWarnings(glm(ystar ~ covar - 1, offset = f, family = binomial(link = "logit"))$coefficients[1])
            ## update the fitted values
            f <- expit(logit(f) + eps*covar)
            r <- SuperLearner::SuperLearner(Y = f, X = x[-s], family = gaussian(), SL.library = lib, ...)$SL.predict
            k <- k+1
            epss[k] <- eps
        }
    }
    est <- mean((f - r)^2)/mean((y - mean(y))^2)
    se <- variableImportanceSE(full = f, reduced = r, y = y, n = length(y))
    ci <- variableImportanceCI(est = est, se = se, n = length(y))
    return(list(est = est, se = se, ci = ci, full = f, reduced = r, eps = eps, epss = epss))
}