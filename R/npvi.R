#' Nonparametric Variable Importance
#'
#' This function computes estimates and confidence intervals for the nonparametric variable importance parameter of interest.
#'
#' @param f1 the regression function to estimate for the full fit, in the usual \code{lm} format.
#' @param f2 the regression function to estimate for the reduced fit, in the usual \code{lm} format.
#' @param data the dataset. Must be in $n \times (p+1)$, where the first column is $Y$.
#' @param j the covariate(s) to calculate variable importance for, defaults to 1.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95% confidence interval.
#' @param type the nonparametric estimation tool to use, defaults to \code{mgcv}. Partial strings can be used, if they are unique identifiers of the tool.
#' @param ... other arguments to the estimation tool.
#' @export

npvi <- function(f, data, j = 1, alpha = 0.05, type = "mgcv", ...) {
  ## get the type, if not fully specified
  ## if not mgcv, loess, np, then stop and return error
  findx <-  pmatch(type,c("mgcv", "loess", "np"))
  if (is.na(findx)) stop("Unsupported estimation tool. Please supply a different tool.")
  type <- c("mgcv", "loess", "np")[findx]

  ## if either of the functions are missing, stop and throw an error
  if (missing(f1)) stop("You must enter a formula for the full fit.")
  if (missing(f2)) stop("You must enter a formula for the reduced fit.")
  if (missing(data)) stop("You must enter data.")

  ## add one to j, since Y is the first column
  j <- j + 1

  ## get the call for later
  cl <- match.call()

  ## split into type
  if (type == "loess") {
    ## calculate the full fit
    full <- loess(f1, data, ...)

    ## calculate the reduced fit
    reduced <- loess(f2, data[, -j], ...)
  } else if (type == "mgcv") {
    ## calculate the full fit
    full <- mgcv(f1, data, ...)

    ## calculate the reduced fit
    reduced <- mgcv(f2, data[, -j], ...)
  } else if (type == "np") {
    ## calculate the full fit
    full <- np(f1, data, ...)

    ## calculate the reduced fit
    reduced <- np(f2, data[, -j], ...)
  } else {
    stop("Unsupported estimation tool. Please supply a different tool.") # shouldn't make it here, but include for completeness
  }

  ## now calculate the one-step estimator
  fhat.ful <- fitted(full)
  fhat.red <- fitted(reduced)

  naive <- mean((fhat.ful - fhat.red) ^ 2)
  onestep <- 2*mean((fhat.ful - fhat.red)*(data$y - fhat.ful)) + naive

  ## Function to calculate the onestep SE
  ## Function: onestepSE
  ## Args: y - the y data
  ##       f - the full fit
  ##       m - the reduced fit
  ##       n - the naive estimator
  ## Returns: the standard error, P_n(D^*(P_n)^2)
  onestepSE <- function(y, f, m, n) {
    ret <- 4*mean((y - f) ^ 2 * (f - m) ^ 2) + mean((f - m) ^ 4)  - n ^ 2 + 4*mean((y - f)*(f - m) ^ 3) - 4*n*mean((y - f)*(f - m))
    return(ret)
  }
  se <- onestepSE(data[, 1], fhat.ful, fhat.red, naive)

  ## Function to calculate a confidence interval
  ## Function: myConfint
  ## Args: est - the estimate to compute the confidence interval for
  ##        se - the standard error for that estimate
  ##         n - the sample size
  ##     level - the confidence level (1-alpha)
  myConfint <- function(est, se, n, level = 0.95) {
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(est), 2L), dimnames = list(names(est)))
    ci[] <- est + (se/sqrt(n)) %o% fac
    ci
  }
  n <- dim(data)[1]

  ## create confidence interval
  ci <- myConfint(onestep, se, n, 1 - alpha)

  ## create the output and return it
  output <- list(call = cl, full.f = f1, red.f = f2, data = data, j = j - 1, type = type, full.fit = fhat.ful,
                 red.fit = fhat.red, est = onestep, se = se, ci = ci, full.mod = full, red.mod = reduced, alpha = alpha)

  ## make it also an npvi object
  tmp.cls <- class(full)
  class(output) <- c("npvi", tmp.cls)
  return(output)
}
