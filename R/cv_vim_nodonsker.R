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
#'  \item{folds}{ - the folds used for cross-validation}
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
#' y <- as.matrix(smooth + stats::rnorm(n, 0, 1))
#' 
#' ## set up a library for SuperLearner
#' learners <- c("SL.mean", "SL.gam")
#' 
#' ## -----------------------------------------
#' ## using Super Learner
#' ## -----------------------------------------
#' set.seed(4747)
#' est <- cv_vim_nodonsker(Y = y, X = x, indx = 2, V = 5, 
#' type = "regression", run_regression = TRUE, 
#' SL.library = learners, alpha = 0.05)
#' 
#' ## ------------------------------------------
#' ## doing things by hand, and plugging them in
#' ## ------------------------------------------
#' ## set up the folds
#' indx <- 2
#' V <- 5
#' set.seed(4747)
#' folds <- rep(seq_len(V), length = n)
#' folds <- sample(folds)
#' ## get the fitted values by fitting the super learner on each pair
#' fhat_ful <- list()
#' fhat_red <- list()
#' for (v in 1:V) {
#'     ## fit super learner
#'     fit <- SuperLearner::SuperLearner(Y = y[folds != v, , drop = FALSE],
#'      X = x[folds != v, , drop = FALSE], SL.library = learners, cvControl = list(V = 5))
#'     fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
#'     ## get predictions on the validation fold
#'     fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, 
#'      newdata = x[folds == v, , drop = FALSE])$pred
#'     ## fit the super learner on the reduced covariates
#'     red <- SuperLearner::SuperLearner(Y = fitted_v,
#'      X = x[folds != v, -indx, drop = FALSE], SL.library = learners, cvControl = list(V = 5))
#'     ## get predictions on the validation fold
#'     fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red, 
#'      newdata = x[folds == v, -indx, drop = FALSE])$pred
#' }
#' est <- cv_vim_nodonsker(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
#' V = 5, folds = folds, type = "regression", run_regression = FALSE, alpha = 0.05)
#' }
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


cv_vim_nodonsker <- function(Y, X, f1, f2, indx = 1, V = 10, folds = NULL, type = "r_squared", run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, na.rm = FALSE, ...) {
  .Deprecated("cv_vim", package = "vimp", msg = "cv_vim now performs all functionality of cv_vim_nodonsker; please update any code to reflect this change!")
  cv_vim(Y = Y, X = X, f1 = f1, f2 = f2, indx = indx, V = V, folds = folds, type = type, run_regression = run_regression, SL.library = SL.library, alpha = alpha, na.rm = na.rm, ...)
}