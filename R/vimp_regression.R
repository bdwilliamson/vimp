#' Nonparametric Variable Importance Estimates
#'
#' Compute estimates of and confidence intervals for nonparametric ANOVA-based variable importance.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the fitted values from a flexible estimation technique regressing Y on X.
#' @param f2 the fitted values from a flexible estimation technique regressing the fitted values in \code{f1} on X withholding the columns in \code{indx}.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param run_regression if outcome Y and covariates X are passed to \code{vimp_regression}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values. 
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of classes \code{vim} and \code{vim_regression}. See Details for more information.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{vim}}
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance}
#'  \item{update}{ - the influence curve-based update}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }
#'
#' @examples
#' library(SuperLearner)
#' library(gam)
#' ## generate the data
#' ## generate X
#' p <- 2
#' n <- 100
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.gam"
#'
#' ## using Y and X
#' est <- vimp_regression(y, x, indx = 2, 
#'            alpha = 0.05, run_regression = TRUE, 
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' ## using pre-computed fitted values
#' full <- SuperLearner(Y = y, X = x,
#' SL.library = learners, cvControl = list(V = 10))
#' full.fit <- predict(full)$pred
#' reduced <- SuperLearner(Y = full.fit, X = x[, 2, drop = FALSE],
#' SL.library = learners, cvControl = list(V = 10))
#' red.fit <- predict(reduced)$pred
#'
#' est <- vimp_regression(Y = y, f1 = full.fit, f2 = red.fit, 
#'             indx = 2, run_regression = FALSE, alpha = 0.05)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


vimp_regression <- function(Y, X, f1 = NULL, f2 = NULL, indx = 1, run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, na.rm = FALSE, ...) {
  ## check to see if f1 and f2 are missing
  ## if the data is missing, stop and throw an error
  if (missing(f1) & missing(Y)) stop("You must enter either Y or fitted values for the full regression.")
  if (missing(f2) & missing(X)) stop("You must enter either X or fitted values for the reduced regression.")

  ## if run_regression = TRUE, then fit SuperLearner
  if (run_regression) {
    
    ## if formula is entered, need a library for Super Learner
    if (is.null(SL.library)) stop("You must enter a library of learners for the Super Learner.")

    ## set up the reduced X
    X_minus_s <- X[, -indx, drop = FALSE]

    ## fit the Super Learner given the specified library
    full <- SuperLearner::SuperLearner(Y = Y, X = X, SL.library = SL.library, ...)

    ## get the fitted values
    fhat_ful <- SuperLearner::predict.SuperLearner(full)$pred

    ## fit the super learner on the reduced covariates:
    ## always use gaussian; if first regression was mean, use Y instead
    arg_lst <- list(...)
    if (length(unique(fhat_ful)) == 1) {
        arg_lst$Y <- Y
    } else {
        arg_lst$family <- stats::gaussian()
        arg_lst$Y <- fhat_ful 
    }
    arg_lst$X <- X_minus_s
    arg_lst$SL.library <- SL.library
    reduced <- do.call(SuperLearner::SuperLearner, arg_lst)    

    ## get the fitted values
    fhat_red <- SuperLearner::predict.SuperLearner(reduced)$pred

  } else { ## otherwise they are fitted values

    ## check to make sure they are the same length as y
    if (is.null(Y)) stop("Y must be entered.")
    if (length(f1) != length(Y)) stop("Fitted values from the full regression must be the same length as Y.")
    if (length(f2) != length(Y)) stop("Fitted values from the reduced regression must be the same length as Y.")

    ## set up the fitted value objects
    fhat_ful <- f1
    fhat_red <- f2

    full <- reduced <- NA    
  }

  ## calculate the estimators 
  ests <- onestep_based_estimator(fhat_ful, fhat_red, Y, type = "regression", na.rm = na.rm)
  
  ## compute the update
  update <- vimp_update(fhat_ful, fhat_red, Y, type = "regression", na.rm = na.rm)

  ## compute the standard error
  se <- vimp_se(update, na.rm = na.rm)

  ## compute the confidence interval
  ci <- vimp_ci(ests[1], se, level = 1 - alpha)
  
  ## get the call
  cl <- match.call()

  ## create the output and return it
  output <- list(call = cl, s = indx,
                 SL.library = SL.library,
                 full_fit = fhat_ful, red_fit = fhat_red, 
                 est = ests[1],
                 naive = ests[2],
                 update = update,
                 se = se, ci = ci, 
                 full_mod = full, 
                 red_mod = reduced,
                 alpha = alpha)

  ## make it also an vim and vim_regression object
  tmp.cls <- class(output)
  class(output) <- c("vim", "vim_regression", tmp.cls)
  return(output)
}
