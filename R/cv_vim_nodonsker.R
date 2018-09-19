#' Nonparametric Variable Importance Estimates using Cross-validation, without Donsker class relaxation
#'
#' Compute estimates and confidence intervals for the
#' nonparametric variable importance parameter of interest, using cross-validation with a single validation fold in the updating procedure.
#' This procedure differs from \code{cv_vim} in that this procedure uses the same data for the naive estimator and the update, and thus does not relax Donsker class conditions necessary for valid confidence intervals.
#'
#' @param Y the outcome.
#' @param X the covariates. 
#' @param f1 the fitted values from a flexible estimation technique regressing Y on X; a list of length V, where each object is one set of predictions on a validation set.
#' @param f2 the fitted values from a flexible estimation technique regressing the fitted values in \code{f1} on X withholding the columns in \code{indx}; a list of length V, where each object is one set of predictions on a validation set.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param V the number of folds for cross-validation, defaults to 10.
#' @param folds the folds to use, if f1 and f2 are supplied.
#' @param type the type of parameter (e.g., ANOVA-based is \code{"regression"}).
#' @param run_regression if outcome Y and covariates X are passed to \code{vimp_regression}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values. 
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
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
#'  \item{call}{ - the call to \code{cv_vim}}
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data (a list, for train and test data)}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data (a list, for train and test data)}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance}
#'  \item{naives}{ - the naive estimator on each fold}
#'  \item{updates}{ - the influence curve-based update for each fold}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }
#'
#' @examples
#' \donttest{
#' library(SuperLearner)
#' library(gam)
#' n <- 100
#' p <- 2
#' ## generate the data
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#' 
#' ## set up a library for SuperLearner
#' learners <- c("SL.mean", "SL.gam")
#' 
#' ## -----------------------------------------
#' ## using Super Learner
#' ## -----------------------------------------
#' est <- cv_vim(Y = y, X = x, indx = 2, V = 5, 
#' type = "regression", run_regression = TRUE, 
#' SL.library = learners, alpha = 0.05)
#' 
#' ## ------------------------------------------
#' ## doing things by hand, and plugging them in
#' ## ------------------------------------------
#' ## set up the folds
#' V <- 5
#' folds <- rep(seq_len(V), length = n)
#' folds <- sample(folds)
#' ## get the fitted values by fitting the super learner on each pair
#' fhat_ful <- list()
#' fhat_red <- list()
#' for (v in 1:V) {
#'     ## fit super learner
#'     fit <- SuperLearner::SuperLearner(Y = Y[folds != v, , drop = FALSE],
#'      X = X[folds != v, , drop = FALSE], SL.library = SL.library, ...)
#'     fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
#'     ## get predictions on the validation fold
#'     fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, 
#'      newdata = X[folds == v, , drop = FALSE])$pred
#'     ## fit the super learner on the reduced covariates
#'     red <- SuperLearner::SuperLearner(Y = fitted_v,
#'      X = X[folds != v, -indx, drop = FALSE], SL.library = SL.library, ...)
#'     ## get predictions on the validation fold
#'     fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red, 
#'      newdata = X[folds == v, -indx, drop = FALSE])$pred
#' }
#' est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
#' V = 5, type = "regression", run_regression = FALSE, alpha = 0.05)
#' }
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


cv_vim_nodonsker <- function(Y, X, f1, f2, indx = 1, V = 10, folds = NULL, type = "regression", run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, na.rm = FALSE, ...) {
  ## check to see if f1 and f2 are missing
  ## if the data is missing, stop and throw an error
  if (missing(f1) & missing(Y)) stop("You must enter either Y or fitted values for the full regression.")
  if (missing(f2) & missing(X)) stop("You must enter either X or fitted values for the reduced regression.")

  ## check to see if Y is a matrix or data.frame; if not, make it one (just for ease of reading)
  if(is.null(dim(Y))) Y <- as.matrix(Y)

  ## if we need to run the regression, fit Super Learner with the given library
  if (run_regression) {
    ## set up the cross-validation
    folds <- rep(seq_len(V), length = dim(Y)[1])
    folds <- sample(folds)
    ## fit the super learner on each full/reduced pair
    fhat_ful <- list()
    fhat_red <- list()
    preds_ful <- list()
    preds_red <- list()
    for (v in 1:V) {
        ## fit super learner
        fit <- SuperLearner::SuperLearner(Y = Y[folds != v, , drop = FALSE],
                                          X = X[folds != v, , drop = FALSE], SL.library = SL.library, ...)
        fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
        ## get predictions on the validation fold
        fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, 
                                                            newdata = X[folds == v, , drop = FALSE])$pred
        ## fit the super learner on the reduced covariates
        red <- SuperLearner::SuperLearner(Y = fitted_v,
                                          X = X[folds != v, -indx, drop = FALSE], SL.library = SL.library, ...)
        ## get predictions on the validation fold
        fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red, 
                                                            newdata = X[folds == v, -indx, drop = FALSE])$pred
    }
    full <- reduced <- NA

  } else { ## otherwise they are fitted values

    ## check to make sure they are the same length as y
    if (is.null(Y)) stop("Y must be entered.")
    if (is.null(f1)) stop("You must specify a list of predicted values from a regression of Y on X.")
    if (is.null(f2)) stop("You must specify a list of predicted values from a regression of the fitted values from the Y on X regression on the reduced set of covariates.")
    if (is.null(folds)) stop("You must specify a vector of folds.")
    if (length(f1) != length(V)) stop("The number of folds from the full regression must be the same length as the number of folds.")
    if (length(f2) != length(V)) stop("The number of folds from the reduced regression must be the same length as the number of folds.")

    ## set up the fitted value objects (both are lists of lists!)
    fhat_ful <- f1
    fhat_red <- f2

    full <- reduced <- NA    
  }

  ## calculate the estimate
  ## loop over the folds
  naive_cv <- vector("numeric", V)
  updates <- vector("numeric", V)
  ses <- vector("numeric", V)
  for (v in 1:V) {
    naive_cv[v] <- onestep_based_estimator(fhat_ful[[v]], fhat_red[[v]], Y[folds == v, ], type = type, na.rm = na.rm)[2]
    updates[v] <- mean(vimp_update(fhat_ful[[v]], fhat_red[[v]], Y[folds == v, ], type = type, na.rm = na.rm), na.rm = na.rm)
    ses[v] <- sqrt(mean(vimp_update(fhat_ful[[v]], fhat_red[[v]], Y[folds == v, ], type = type, na.rm = na.rm)^2, na.rm = na.rm))
  }
  ## corrected estimator
  est <- mean(naive_cv) + mean(updates)
  ## naive estimator
  naive <- mean(naive_cv)
  ## calculate the standard error
  se <- mean(ses)/sqrt(length(Y))
  ## calculate the confidence interval
  ci <- vimp_ci(est, se, 1 - alpha)
  
  ## get the call
  cl <- match.call()

  ## create the output and return it
  output <- list(call = cl, s = indx,
                 SL.library = SL.library,
                 full_fit = list("train" = fhat_ful, "test" = preds_ful), red_fit = list("train" = fhat_red, "test" = preds_red), 
                 est = est,
                 naive = naive,
                 naives = naive_cv,
                 update = updates,
                 se = se, ci = ci, 
                 full_mod = full, 
                 red_mod = reduced,
                 alpha = alpha)

  ## make it also an vim object
  tmp.cls <- class(output)
  class(output) <- c("vim", c("vim_regression", "vim_deviance")[as.numeric(type == "deviance") + 1], tmp.cls)
  return(output)
}