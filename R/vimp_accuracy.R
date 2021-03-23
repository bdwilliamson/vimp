#' Nonparametric Intrinsic Variable Importance Estimates: Classification accuracy
#'
#' Compute estimates of and confidence intervals for nonparametric
#' difference in classification accuracy-based intrinsic variable importance.
#' This is a wrapper function for \code{cv_vim}, with \code{type = "accuracy"}.
#'
#' @inheritParams cv_vim
#'
#' @return An object of classes \code{vim} and \code{vim_accuracy}.
#'   See Details for more information.
#'
#' @inherit cv_vim details
#'
#' @examples
#' # generate the data
#' # generate X
#' p <- 2
#' n <- 100
#' x <- data.frame(replicate(p, stats::runif(n, -1, 1)))
#'
#' # apply the function to the x's
#' f <- function(x) 0.5 + 0.3*x[1] + 0.2*x[2]
#' smooth <- apply(x, 1, function(z) f(z))
#'
#' # generate Y ~ Normal (smooth, 1)
#' y <- matrix(rbinom(n, size = 1, prob = smooth))
#'
#' # set up a library for SuperLearner; note simple library for speed
#' library("SuperLearner")
#' learners <- c("SL.glm", "SL.mean")
#'
#' # estimate (with a small number of folds, for illustration only)
#' est <- vimp_accuracy(y, x, indx = 2,
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, V = 2, cvControl = list(V = 2))
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
vimp_accuracy <- function(Y = NULL, X = NULL, f1 = NULL, f2 = NULL, indx = 1,
                          V = 10, run_regression = TRUE,
                          SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                          alpha = 0.05, delta = 0, na.rm = FALSE, 
                          cross_fitting_folds = NULL,
                          sample_splitting_folds = NULL,
                          stratified = TRUE, scale = "identity",
                          C = rep(1, length(Y)), Z = NULL,
                          ipc_weights = rep(1, length(Y)),
                          ipc_est_type = "aipw", ...) {
    cv_vim(Y = Y, X = X, f1 = f1, f2 = f2, indx = indx, V = V,
           type = "accuracy", run_regression = run_regression,
           SL.library = SL.library, alpha = alpha, delta = delta,
           na.rm = na.rm, cross_fitting_folds = cross_fitting_folds, 
           sample_splitting_folds = sample_splitting_folds, stratified = stratified,
           scale = scale, C = C, Z = Z, ipc_weights = ipc_weights,
           ipc_est_type = ipc_est_type, ...)
}
