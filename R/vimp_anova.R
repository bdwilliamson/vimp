#' Nonparametric Intrinsic Variable Importance Estimates: ANOVA
#'
#' Compute estimates of and confidence intervals for nonparametric ANOVA-based
#' intrinsic variable importance. This is a wrapper function for \code{cv_vim},
#' with \code{type = "anova"}. This type
#' has limited functionality compared to other
#' types; in particular, null hypothesis tests
#' are not possible using \code{type = "anova"}.
#' If you want to do null hypothesis testing
#' on an equivalent population parameter, use
#' \code{vimp_rsquared} instead.
#'
#' @inheritParams cv_vim
#'
#' @return An object of classes \code{vim} and \code{vim_anova}.
#'   See Details for more information.
#'
#' @details We define the population ANOVA
#' parameter for the group of features (or  single feature) \eqn{s} by
#' \deqn{\psi_{0,s} := E_0\{f_0(X) - f_{0,s}(X)\}^2/var_0(Y),}
#' where \eqn{f_0} is the population conditional mean using all features,
#' \eqn{f_{0,s}} is the population conditional mean using the features with
#' index not in \eqn{s}, and \eqn{E_0} and \eqn{var_0} denote expectation and
#' variance under the true data-generating distribution, respectively.
#'
#' Cross-fitted ANOVA estimates are computed by first
#' splitting the data into \eqn{K} folds; then using each fold in turn as a
#' hold-out set, constructing estimators \eqn{f_{n,k}} and \eqn{f_{n,k,s}} of
#' \eqn{f_0} and \eqn{f_{0,s}}, respectively on the training data and estimator
#' \eqn{E_{n,k}} of \eqn{E_0} using the test data; and finally, computing
#' \deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K E_{n,k}\{f_{n,k}(X) - f_{n,k,s}(X)\}^2/var_n(Y),}
#' where \eqn{var_n} is the empirical variance.
#' See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function.
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
#' est <- vimp_anova(y, x, indx = 2,
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, V = 2, cvControl = list(V = 2))
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the
#'   \code{SuperLearner} function and package.
#' @export
vimp_anova <- function(Y = NULL, X = NULL, cross_fitted_f1 = NULL,
                       cross_fitted_f2 = NULL,  indx = 1,
                       V = 10, run_regression = TRUE,
                       SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                       alpha = 0.05, delta = 0, na.rm = FALSE,
                       cross_fitting_folds = NULL,
                       stratified = FALSE, C = rep(1, length(Y)), Z = NULL,
                       ipc_weights = rep(1, length(Y)), scale = "identity",
                       ipc_est_type = "aipw", scale_est = TRUE,
                       cross_fitted_se = TRUE, ...) {
  cv_vim(type = "anova", Y = Y, X = X, cross_fitted_f1 = cross_fitted_f1,
         cross_fitted_f2 = cross_fitted_f2, f1 = NULL, f2 = NULL, indx = indx,
         V = V, run_regression = run_regression, SL.library = SL.library,
         alpha = alpha, delta = delta, na.rm = na.rm, stratified = stratified,
         cross_fitting_folds = cross_fitting_folds, ipc_weights = ipc_weights,
         sample_splitting = FALSE, sample_splitting_folds = NULL,
         C = C, Z = Z, scale = scale, ipc_est_type = ipc_est_type,
         scale_est = scale_est, cross_fitted_se = cross_fitted_se, ...)
}
