#' Nonparametric Intrinsic Variable Importance Estimates: ANOVA
#'
#' Compute estimates of and confidence intervals for nonparametric
#' ANOVA-based intrinsic variable importance. This is a wrapper function for
#' \code{cv_vim}, with \code{type = "anova"}.
#' This function is deprecated in \code{vimp} version 2.0.0.
#'
#' @inheritParams vimp_anova
#'
#' @return An object of classes \code{vim} and \code{vim_regression}.
#'   See Details for more information.
#'
#' @inherit vimp_anova details
#'
#' @examples
#' # generate the data
#' # generate X
#' p <- 2
#' n <- 100
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' # apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' # generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' # set up a library for SuperLearner; note simple library for speed
#' library("SuperLearner")
#' learners <- c("SL.glm", "SL.mean")
#'
#' # estimate (with a small number of folds, for illustration only)
#' est <- vimp_regression(y, x, indx = 2,
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, V = 2, cvControl = list(V = 2))
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
vimp_regression <- function(Y = NULL, X = NULL, cross_fitted_f1 = NULL,
                       cross_fitted_f2 = NULL,  indx = 1,
                       V = 10, run_regression = TRUE,
                       SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                       alpha = 0.05, delta = 0, na.rm = FALSE,
                       cross_fitting_folds = NULL,
                       stratified = FALSE, C = rep(1, length(Y)), Z = NULL,
                       ipc_weights = rep(1, length(Y)), scale = "identity",
                       ipc_est_type = "aipw", scale_est = TRUE,
                       cross_fitted_se = TRUE, ...) {
  .Deprecated("vimp_anova", package = "vimp",
              msg = paste0(
                "vimp_anova now performs all functionality of vimp_regression; "
                , "please update any code to reflect this change!"
              ))
  vimp_anova(Y = Y, X = X, cross_fitted_f1 = cross_fitted_f1,
             indx = indx, V = V,
             run_regression = run_regression, SL.library = SL.library,
             alpha = alpha, delta = delta, na.rm = na.rm,
             cross_fitting_folds = cross_fitting_folds,
             stratified = stratified, C = C, Z = Z, ipc_weights = ipc_weights,
             ipc_est_type = ipc_est_type, scale = scale, scale_est = scale_est, cross_fitted_se = cross_fitted_se, ...)
}
