#' Leave Covariate(s) Out Nonparametric Variable Importance
#'
#' Compute estimates and confidence intervals for the
#' nonparametric variable importance parameter of interest.
#'
#' @param f1 either: (1) the regression function to estimate for the full fit,
#' in the form \code{y ~ x} if using Super Learner; or (2) the fitted values from a flexible estimation technique.
#' @param f2 either: (1) the regression function to estimate for the reduced fit,
#' in the form \code{y ~ x} or \code{fit ~ x} if using Super Learner without or with the two-step estimating procedure (recommended); or (2) the fitted values from a flexible estimation technique.
#' @param data the dataset. Must be \eqn{n x (p+1)}, where the first
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
#'  \item{full.f}{ - either the formula for the full regression or the fitted values for the full regression, based on the \code{call}}
#'  \item{red.f}{ - either the formula for the reduced regression or the fitted values for the reduced regression, based on the \code{call}}
#'  \item{data}{ - the data used by the function}
#'  \item{j}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{full.fit}{ - the fitted values of the chosen method fit to the full data}
#'  \item{red.fit}{ - the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{ - the estimated variable importance}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) x 100}\% confidence interval for the variable importance estimate}
#'  \item{full.mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red.mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }
#'
#' @examples
#' \dontrun{
#' require(SuperLearner)
#' ## generate the data
#' ## generate X
#' x <- replicate(p, stats::runif(n, -5, 5))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' testdat <- as.data.frame(cbind(x, y))
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.gam"
#'
#' ## using class "formula"
#' est <- vim(y ~ x, fit ~ x, data = testdat, y = testdat[, 3],
#'            n = length(y), j = 2, standardized = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' ## using pre-computed fitted values
#' full <- SuperLearner(Y = testdat$y, X = testdat[, 1:2],
#' SL.library = learners, cvControl = list(V = 10))
#' full.fit <- predict(full)$pred
#' reduced <- SuperLearner(Y = full.fit, X = testdat[, 1],
#' SL.library = learners, cvControl = list(V = 10))
#' red.fit <- predict(reduced)$pred
#'
#' est <- vim(full.fit, reduced.fit, y = testdat$y, j = 2,
#' standardized = TRUE, alpha = 0.05)
#' }
#'
#' @seealso \code{\link{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


vim <- function(f1, f2, data = NULL, y = data[, 1], n = length(y), j = 1, standardized = TRUE, alpha = 0.05, SL.library = NULL, ...) {
  ## check to see if f1 and f2 are missing
  ## if the data is missing, stop and throw an error
  if (missing(f1)) stop("You must enter a formula or fitted values for the full regression.")
  if (missing(f2)) stop("You must enter a formula or fitted values for the reduced regression.")

  ## check to see if f1 and f2 are formulas or fitted values
  ## if formula, fit Super Learner with the given library
  if (class(f1) == "formula" & class(f2) == "formula") {
    ## if formula is entered, data can't be null
    if (is.null(data)) stop("You must enter data if f1 and f2 are formulas.")

    ## if formula is entered, need a library for Super Learner
    if (is.null(SL.library)) stop("You must enter a library of learners for the Super Learner.")

    ## set up X
    X <- data[, -1]

    ## set up the reduced X
    X.minus.j <- X[, -j]

    ## fit the Super Learner given the specified library
    full <- SuperLearner::SuperLearner(Y = y, X = X, SL.library = SL.library, ...)

    ## get the fitted values
    fhat.ful <- SuperLearner::predict.SuperLearner(full)$pred

    ## non-two-step (not recommended)
    if (sum(as.character(f2) == c("~", "y", "x")) == 3) {
      reduced <- SuperLearner::SuperLearner(Y = y, X = X.minus.j, SL.library = SL.library,...)
    } else { ## two-step, recommended
      reduced <- SuperLearner::SuperLearner(fhat.ful, X = X.minus.j, SL.library = SL.library,...)
    }

    ## get the fitted values
    fhat.red <- SuperLearner::predict.SuperLearner(reduced)$pred

  } else if (class(f2) == "formula") { ## can use fitted in full and formula in reduced

    ## if formula is entered, data can't be null
    if (is.null(data)) stop("You must enter data if f2 is a formula.")

    ## if formula is entered, need a library for Super Learner
    if (is.null(SL.library)) stop("You must enter a library of learners for the Super Learner.")

    ## need f1 to be the same length as Y
    if (length(y) != dim(data)[1] | length(f1) != dim(data)[1])

    ## fitted values from full regression
    fhat.ful <- f1

    ## set up X
    X <- data[, -1]

    ## set up the reduced X
    X.minus.j <- X[, -j]

    ## non-two-step (not recommended)
    if (sum(as.character(f2) == c("~", "y", "x")) == 3) {
      reduced <- SuperLearner::SuperLearner(Y = y, X = X.minus.j, SL.library = SL.library,...)
    } else { ## two-step, recommended
      reduced <- SuperLearner::SuperLearner(fhat.ful, X = X.minus.j, SL.library = SL.library,...)
    }

    ## get the fitted values
    fhat.red <- SuperLearner::predict.SuperLearner(reduced)$pred

  } else { ## otherwise they are fitted values

    ## check to make sure they are the same length as y
    if (length(f1) != length(y)) stop("Fitted values from the full regression must be the same length as y.")
    if (length(f2) != length(y)) stop("Fitted values from the reduced regression must be the same length as y.")

    ## set up the fitted value objects
    fhat.ful <- f1
    fhat.red <- f2

    full.mod <- red.mod <- NA
  }

  ## calculate the estimate
  est <- variableImportance(fhat.ful, fhat.red, y, n, standardized)

  ## calculate the standard error
  se <- variableImportanceSE(fhat.ful, fhat.red, y, n, standardized)

  ## calculate the confidence interval
  ci <- variableImportanceCI(est, se, n, 1 - alpha)

  ## get the call
  cl <- match.call()

  ## create the output and return it
  output <- list(call = cl, full.f = f1, red.f = f2, data = data, j = j,
                 SL.library = SL.library,
                 full.fit = fhat.ful, red.fit = fhat.red, est = est,
                 se = se, ci = ci, full.mod = full, red.mod = reduced,
                 alpha = alpha)

  ## make it also an npvi object
  tmp.cls <- class(full)
  class(output) <- c("npvi", tmp.cls)
  return(output)
}
