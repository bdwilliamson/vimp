#' Estimate variable importance using a TMLE-based approach
#'
#' Compute nonparametric estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param full fitted values from an initial regression of the outcome on the full set of covariates.
#' @param reduced fitted values from an initial regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates. \code{reduced} may be a matrix or data frame, in which case multivariate updates will be performed.
#' @param y the outcome variable
#' @param x the covariates (ordered)
#' @param s the indices for variable importance, or a list of indices (where the length of the list matches the number of columns in \code{reduced}.)
#' @param lib the library of candidate learners passed to Super Learner
#' @param tol the tolerance level for convergence
#' @param max_iter the maximum number of iterations for each Super Learner fit
#' @param ... other arguments, passed to SuperLearner
#'
#' @return The estimated variable importance for the given group of left-out covariates, along with the SE and a CI.
#'
#' @details This differs from estimates returned by variableImportance() in that the procedure used to target the estimate to the parameter of interest is based on a TMLE approach.
#' @export
variableImportanceTMLE <- function(full, reduced, y, x, s, lib, tol = .Machine$double.eps, max_iter = 500, ...) {
    ## helper functions
    logit <- function(x) log(x/(1-x))
    # expit <- function(x) exp(x)/(1+exp(x))
    expit <- function(x) 1/(1+exp(-x))
    ## initialize the criterion return vector
    avgs <- matrix(0, nrow = ifelse(!is.null(dim(reduced)), dim(reduced)[2], 1), ncol = max_iter)
    ## initialize updated reduced
    new.r <- NULL

    ## change y to between 0 and 1 if it isn't already
    if (max(y) > 1 | min(y) < 0) {
        ystar <- (y - min(y))/(max(y) - min(y))
    } else {
        ystar <- y
    }

    ## calculate the covariate
    covar <- as.vector(full) - reduced
    ## calculate the offset
    off <- logit(full)

    ## get initial epsilon (with no intercept)
    fluctuation <- suppressWarnings(stats::glm.fit(covar, ystar, offset = off, family = stats::binomial(link = "logit"), intercept = FALSE))
    eps.init <- fluctuation$coefficients[1:length(fluctuation$coefficients)]
    ## get update
    new.f <- expit(logit(full) + covar%*%matrix(eps.init))
    ## update all of the reduced ones
    if (is.list(s)) {
        for (i in 1:length(s)){
            new.r <- cbind(new.r, SuperLearner::SuperLearner(Y = new.f, X = x[, -s[[i]], drop = FALSE], family = stats::gaussian(), SL.library = lib, ...)$SL.predict)    
        }
    } else {
        new.r <- SuperLearner::SuperLearner(Y = new.f, X = x[, -s, drop = FALSE], family = stats::gaussian(), SL.library = lib, ...)$SL.predict
    }
    
    
    ## check the stopping criterion 
    if (!is.null(dim(new.r))) {
        avg <- colMeans(apply(new.r, 2, variableImportanceIC, full = new.f, y = ystar))
    } else { 
        avg <- variableImportanceIC(full = new.f, reduced = new.r, y = ystar)
    }    
    avgs[, 1] <- avg
    if (max(abs(avg)) < tol) { # criterion should be empirical average zero
        f <- new.f
        r <- new.r
        eps <- eps.init
    } else {## now repeat until convergence
        f <- new.f
        r <- new.r
        eps <- eps.init
        k <- 1
        avgs[, k] <- avg
        while(max(abs(avg)) > tol & k < max_iter) {
            ## if we didn't change by tol, break
            # if (k > 1) {
            #     if (abs(epss[, k] - epss[, k-1]) < tol) {
            #         break
            #     }
            # } 
            ## get the covariate
            covar <- as.vector(f) - r
            ## calculate the offset
            off <- logit(f)
            ## update epsilon
            fluctuation <- suppressWarnings(stats::glm.fit(covar, ystar, offset = off, family = stats::binomial(link = "logit"), intercept = FALSE))
            eps <- fluctuation$coefficients[1:length(fluctuation$coefficients)]
            ## get update
            new.f <- expit(logit(f) + covar%*%matrix(eps))
            new.r <- NULL
            ## update all of the reduced ones
            if (is.list(s)) {
                for (i in 1:length(s)){
                    new.r <- cbind(new.r, SuperLearner::SuperLearner(Y = new.f, X = x[, -s[[i]], drop = FALSE], family = stats::gaussian(), SL.library = lib, ...)$SL.predict)    
                }
            } else {
                new.r <- SuperLearner::SuperLearner(Y = new.f, X = x[, -s, drop = FALSE], family = stats::gaussian(), SL.library = lib, ...)$SL.predict
            }
            f <- new.f
            r <- new.r
            ## now repeat until convergence
            if (!is.null(dim(r))) {
                avg <- colMeans(apply(r, 2, variableImportanceIC, full = f, y = ystar))
            } else { # only entered a vector  
                avg <- variableImportanceIC(full = f, reduced = r, y = ystar)    
            }
            k <- k+1
            avgs[, k] <- avg

        }
    }
    ## if we had to transform, then back transform
    if (!is.null(dim(r))) { 
        est <- colMeans((as.vector(f) - r)^2)/mean((ystar - mean(ystar))^2)
        se <- apply(r, 2, variableImportanceSE, full = f, y = ystar, n = length(ystar))
    } else {
        est <- mean((as.vector(f) - r)^2)/mean((ystar - mean(ystar))^2)
        se <- variableImportanceSE(full = f, reduced = r, y = ystar, n = length(ystar))
    }
    ci <- variableImportanceCI(est = est, se = se, n = length(y))
    return(list(est = est, se = se, ci = ci, full = f, reduced = r, avg = avg, avgs = avgs))
}