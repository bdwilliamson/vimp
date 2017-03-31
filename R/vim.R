#' Leave Covariate(s) Out Nonparametric Variable Importance
#'
#' Compute estimates and confidence intervals for the
#' nonparametric variable importance parameter of interest.
#'
#' @param f1 the regression function to estimate for the full fit,
#' in the usual \code{lm} format.
#' @param f2 the regression function to estimate for the reduced fit,
#' in the usual \code{lm} format.
#' @param data the dataset. Must be in \eqn{n x (p+1)}, where the first
#' column is \eqn{Y}.
#' @param y the outcome; by default is the first column in \code{data}.
#' @param n the sample size.
#' @param j the covariate(s) to calculate variable importance for,
#'  defaults to 1.
#' @param standardized should we estimate the standardized parameter? (defaults to \code{TRUE})
#' @param alpha the level to compute the confidence interval at.
#' Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are formulas.
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{npvi}. See Details for more information.
#'
#' @details See the paper by Williamson, Carone, and Simon for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' In the interest of transparency, we return most of the calculations
#' within the \code{npvi} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{vim}}
#'  \item{full.f}{ - the model for the full data}
#'  \item{red.f}{ - the model for the reduced data}
#'  \item{data}{ - the data used by the function}
#'  \item{j}{ - the column(s) to calculate variable importance for}
#'  \item{type}{ - the method to estimate the regressions}
#'  \item{full.fit}{ - the fitted values of the chosen method fit to the full data}
#'  \item{red.fit}{ - the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{ - the estimated variable importance}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) x 100}\% confidence interval for the variable importance}
#'  \item{full.mod}{ - the object returned by the estimation procedure for the full data regression}
#'  \item{red.mod}{ - the object returned by the estimation procedure for the reduced data regression}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }
#'
#' @examples
#' ## generate the data
#' testdat <- generateData(n = 100, p = 2)
#'
#' ## get the estimate
#' est <- lco(y ~ V1 + V2, y ~ V1, data = testdat, j = 2, type = "np", regtype = "ll", cktertype = "epanechnikov")
#'
#' @seealso \code{\link{np}} for specific usage of the nonparametric kernel smoothing methods,
#' \code{\link{mgcv}} for specific usage of the GAM methods, and
#' \code{\link{loess}} for local polynomial fitting.
#' @export


vim <- function(f1, f2, data = NULL, y = data[, 1], n = length(y), j = 1, standardized = TRUE, alpha = 0.05, SL.library = NULL, ...) {
  ## check to see if f1 and f2 are missing
  ## if the data is missing, stop and throw an error
  if (missing(f1)) stop("You must enter a formula or fitted values for the full regression.")
  if (missing(f2)) stop("You must enter a formula or fitted values for the reduced regression.")

  ## check to see if f1 and f2 are formulas or fitted values
  ## if formula, fit Super Learner with the given library
  if (class(f1) == "formula") {
    ## if formula is entered, data can't be null
    if (is.null(data)) stop("You must enter data if f1 and f2 are formulas.")

    ## if formula is entered, need a library for Super Learner
    if (is.null(SL.library)) stop("You must enter a library of learners for the Super Learner.")
    ## get the reduced dataset
    dat.reduced <- data[, -j]
  } else {
    ## otherwise they are fitted values

    ## calculate the estimate
    est <- variableImportance(f1, f2, y, n, standardized)

    ## calculate the standard error
    se <- variableImportanceSE(f1, f2, y, n, standardized)

    ## calculate the confidence interval
    ci <- variableImportanceCI(est, se, n, 1 - alpha)

  }



  ## get the call for later
  cl <- match.call()





  ## create the output and return it
  output <- list(call = cl, full.f = f1, red.f = f2, data = data, j = j, type = type, full.fit = fhat.ful,
                 red.fit = fhat.red, est = onestep, se = se, ci = ci, full.mod = full, red.mod = reduced, alpha = alpha)

  ## make it also an npvi object
  tmp.cls <- class(full)
  class(output) <- c("npvi", tmp.cls)
  return(output)
}
