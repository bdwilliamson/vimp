#' Nonparametric Variable Importance Estimates: Classification accuracy
#'
#' Compute estimates of and confidence intervals for nonparametric difference in misclassification rate-based variable importance. This is a wrapper function for \code{vim}, with \code{type = "accuracy"}.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the fitted values from a flexible estimation technique regressing Y on X.
#' @param f2 the fitted values from a flexible estimation technique regressing Y on X withholding the columns in \code{indx}.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param run_regression if outcome Y and covariates X are passed to \code{vimp_accuracy}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param f1_split the fitted values from a flexible estimation technique regressing Y on X in one independent split of the data (for hypothesis testing).
#' @param f2_split the fitted values from a flexible estimation technique regressing Y on X witholding the columns in \code{indx}, in a separate independent split from \code{f1_split} (for hypothesis testing).
#' @param folds the folds used for \code{f1_split} and \code{f2_split}; assumed to be 1 for the observations used in \code{f1_split} and 2 for the observations used in \code{f2_split}.
#' @param scale scale should CIs be computed on original ("identity") or logit ("logit") scale? (defaults to "logit")
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of classes \code{vim} and \code{vim_accuracy}. See Details for more information.
#'
#' @details In the interest of transparency, we return most of the calculations
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
#'  \item{y}{ - the outcome}
#' }
#'
#' @examples
#' library(SuperLearner)
#' library(gam)
#' ## generate the data
#' ## generate X
#' p <- 2
#' n <- 100
#' x <- data.frame(replicate(p, stats::runif(n, -1, 1)))
#'
#' ## apply the function to the x's
#' f <- function(x) 0.5 + 0.3*x[1] + 0.2*x[2]
#' smooth <- apply(x, 1, function(z) f(z))
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- matrix(rbinom(n, size = 1, prob = smooth))
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.gam"
#'
#' ## using Y and X
#' est <- vimp_accuracy(y, x, indx = 2,
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' ## using pre-computed fitted values
#' full <- SuperLearner(Y = y, X = x,
#' SL.library = learners, cvControl = list(V = 10))
#' full.fit <- predict(full)$pred
#' reduced <- SuperLearner(Y = y, X = x[, -2, drop = FALSE],
#' SL.library = learners, cvControl = list(V = 10))
#' red.fit <- predict(reduced)$pred
#'
#' est <- vimp_accuracy(Y = y, f1 = full.fit, f2 = red.fit,
#'             indx = 2, run_regression = FALSE, alpha = 0.05)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


vimp_accuracy <- function(Y, X, f1 = NULL, f2 = NULL, indx = 1, weights = rep(1, length(Y)), run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, na.rm = FALSE,
                          f1_split = NULL, f2_split = NULL, folds = NULL, scale = "logit", ...) {
    vim(Y = Y, X = X, f1 = f1, f2 = f2, indx = indx, weights = weights, type = "accuracy", run_regression = run_regression, SL.library = SL.library, alpha = alpha, na.rm = na.rm,
        f1_split = f1_split, f2_split = f2_split, folds = folds, scale = scale,...)
}
