#' Nonparametric Variable Importance Estimates using Cross-validation
#'
#' Compute estimates and confidence intervals for the
#' nonparametric variable importance parameter of interest, using cross-validation in the updating procedure.
#' This essentially involves splitting the data into V train/test splits; train the learners on the training data, evaluate importance on the test data; and average over these splits.
#'
#' @param f1 either: (1) the regression function to estimate for the full fit,
#' in the form \code{y ~ x} if using Super Learner; or (2) the fitted values from a flexible estimation technique.
#' @param f2 either: (1) the regression function to estimate for the reduced fit,
#' in the form \code{y ~ x} or \code{fit ~ x} if using Super Learner without or with the sequential regression (recommended), respectively; or (2) the fitted values from a flexible estimation technique.
#' @param data the dataset. Must be \eqn{n x (p+1)}, where the first
#' column is \eqn{Y}.
#' @param y the outcome; by default is the first column in \code{data}.
#' @param n the sample size.
#' @param indx the indices of the covariate(s) to calculate variable importance for,
#'  defaults to 1.
#' @param standardized should we estimate the standardized parameter? (defaults to \code{TRUE})
#' @param two_phase did the data come from a two-phase sample? (defaults to \code{FALSE})
#' @param tmle should we use the one-step-based estimator (\code{FALSE}) or the TMLE-based estimator (\code{TRUE}) (defaults to \code{FALSE}).
#' @param update_denom logical; was smoothing used to estimate the denominator of the parameter of interest, if standardized? (defaults to \code{TRUE})
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param alpha the level to compute the confidence interval at.
#' Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are formulas.
#' @param tol numerical error tolerance (only used if \code{tmle = TRUE}).
#' @param max_iter maximum number of TMLE iterations (only used if \code{tmle = TRUE}).
#' @param V the number of folds for cross-validation, defaults to 10.
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{vim}}
#'  \item{full.f}{ - either the formula for the full regression or the fitted values for the full regression, based on the \code{call}}
#'  \item{red.f}{ - either the formula for the reduced regression or the fitted values for the reduced regression, based on the \code{call}}
#'  \item{data}{ - the data used by the function}
#'  \item{s}{ - the column(s) to calculate variable importance for}
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
#' p <- 2
#' n <- 100
#' x <- replicate(p, stats::runif(n, -5, 5))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' testdat <- as.data.frame(cbind(y, x))
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.gam"
#'
#' ## using class "formula"
#' est <- cv_vim(y ~ x, fit ~ x, data = testdat, y = testdat[, 1],
#'            n = length(y), indx = 2, V = 5, standardized = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' ## using pre-computed fitted values
#' full <- SuperLearner(Y = testdat$y, X = testdat[, 2:3],
#' SL.library = learners, cvControl = list(V = 10))
#' full.fit <- predict(full)$pred
#' reduced <- SuperLearner(Y = full.fit, X = testdat[, 3],
#' SL.library = learners, cvControl = list(V = 10))
#' red.fit <- predict(reduced)$pred
#'
#' est <- cv_vim(full.fit, reduced.fit, y = testdat$y, indx = 2, V = 5,
#' standardized = TRUE, alpha = 0.05)
#' }
#'
#' @seealso \code{\link{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


cv_vim <- function(f1, f2, data = NULL, y = data[, 1], n = length(unlist(y)), indx = 1, standardized = TRUE, two_phase = FALSE, tmle = FALSE, update_denom = TRUE, na.rm = FALSE, alpha = 0.05, SL.library = NULL, tol = .Machine$double.eps, max_iter = 500, V = 10, ...) {
  ## check to see if f1 and f2 are missing
  ## if the data is missing, stop and throw an error
  if (missing(f1)) stop("You must enter a formula or fitted values for the full regression.")
  if (missing(f2)) stop("You must enter a formula or fitted values for the reduced regression.")

  ## check to see if f1 and f2 are formulas or fitted values
  ## if formula, fit Super Learner with the given library
  if (class(f1) == "formula" & class(f2) == "formula") {
    stop("cv_vim() is not yet implemented for objects of class 'formula'.")

  } else if (class(f2) == "formula") { ## can use fitted in full and formula in reduced
    stop("cv_vim() is not yet implemented for objects of class 'formula'.")

  } else { ## otherwise they are fitted values

    ## check to make sure they are the same length as y
    if (is.null(y)) stop("Y must be entered.")
    if (length(f1) != length(y)) stop("The number of folds from the full regression must be the same length as the number of folds in y.")
    if (length(f2) != length(y)) stop("The number of folds from the reduced regression must be the same length as the number of folds in y.")

    ## set up the fitted value objects (both are lists!)
    fhat.ful <- f1
    fhat.red <- f2

    full <- reduced <- NA    
  }

  ## calculate the estimate
  if (tmle) {
    stop("cv_vim() is not yet implemented with a TMLE.")
    
  } else {
    ## loop over the folds
    naive_cv <- vector("numeric", V)
    updates <- vector("numeric", V)
    ses <- vector("numeric", V)
    for (v in 1:V) {
      if (standardized) {
        naive_cv[v] <- mean((fhat.ful[[v]] - fhat.red[[v]])^2, na.rm = na.rm)/mean((y[[v]] - mean(y[[v]], na.rm = na.rm))^2, na.rm = na.rm)
      } else {
        naive_cv[v] <- mean((fhat.ful[[v]] - fhat.red[[v]])^2, na.rm = na.rm)
      }
      
      if (update_denom) { ## here, use the IC of the full standardized parameter
        updates[v] <- mean(variableImportanceIC(fhat.ful[[v]], fhat.red[[v]], y[[v]], standardized = standardized, na.rm = na.rm), na.rm = na.rm)
        ses[v] <- mean(variableImportanceIC(fhat.ful[[v]], fhat.red[[v]], y[[v]], standardized = standardized, na.rm = na.rm)^2, na.rm = na.rm)  
      } else { ## here, use the fact that the unstandardized and variance are jointly normal, along with the delta method
        ## naive estimators of numerator (based on subset, due to smoothing), denominator (based on all data, no smoothing)
        naive.j <- mean((fhat.ful[[v]] - fhat.red[[v]]) ^ 2, na.rm = na.rm)
        naive.var <- mean((unlist(y) - mean(unlist(y), na.rm = na.rm))^2, na.rm = na.rm)
        ## influence curves
        contrib.denom <- ((unlist(y) - mean(unlist(y), na.rm = na.rm))^2 - naive.var)
        contrib.num <- variableImportanceIC(fhat.ful[[v]], fhat.red[[v]], y[[v]], standardized = FALSE, na.rm = na.rm)
        ## update
        updates[v] <- (mean(contrib.num, na.rm = na.rm) - mean(contrib.denom, na.rm = na.rm))/naive.var
        ## standard deviation, based on delta method
        ses[v] <- sqrt(mean((1/naive.var^2)*contrib.num^2, na.rm = na.rm) + mean((naive.j/(naive.var)^2)^2*contrib.denom^2, na.rm = na.rm))
      }
      
    }
    est <- mean(naive_cv) + mean(updates)
    ## calculate the standard error
    se <- mean(ses)/sqrt(n)
    ## calculate the confidence interval
    ci <- variableImportanceCI(est, se, n, 1 - alpha)
  }
  
  ## get the call
  cl <- match.call()

  ## create the output and return it
  output <- list(call = cl, full.f = f1, red.f = f2, data = data, s = indx,
                 SL.library = SL.library,
                 full.fit = fhat.ful, red.fit = fhat.red, est = est,
                 se = se, ci = ci, full.mod = full, red.mod = reduced,
                 alpha = alpha)

  ## make it also an vim object
  tmp.cls <- class(output)
  class(output) <- c("vim", tmp.cls)
  return(output)
}