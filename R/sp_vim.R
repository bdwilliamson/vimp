#' Shapley Population Variable Importance Measure (SPVIM) Estimates and Inference
#'
#' Compute estimates and confidence intervals for the SPVIMs, using cross-fitting.
#' This essentially involves splitting the data into V train/test splits; train the learners on the training data, evaluate importance on the test data; and average over these splits.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param V the number of folds for cross-validation, defaults to 10.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type the type of parameter (e.g., R-squared-based is \code{"r_squared"}).
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param univariate_SL.library (optional) a character vector of learners to pass to \code{SuperLearner} for estimating univariate regression functions. Defaults to \code{SL.polymars}
#' @param gamma the fraction of the sample size to use when sampling subsets (e.g., \code{gamma = 1} samples the same number of subsets as the sample size)
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param stratified should the generated folds be stratified based on the outcome (helps to ensure class balance across cross-validation folds)?
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details We define the SPVIM as the weighted average of the population difference in 
#' predictiveness over all subsets of features not containing feature \eqn{j}.
#' 
#' This is equivalent to finding the solution to a population weighted least squares problem. This 
#' key fact allows us to estimate the SPVIM using weighted least squares, where we first 
#' sample subsets from the power set of all possible features using the Shapley sampling distribution; then
#' use cross-fitting to obtain estimators of the predictiveness of each sampled subset; and finally, solve the 
#' least squares problem given in Williamson and Feng (2020).
#' 
#' See the paper by Williamson and Feng (2020) for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' The function works by estimating
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{cv_vim}}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#' \item{v}{- the estimated predictiveness measure for each sampled subset}
#'  \item{preds_lst}{ - the predicted values from the chosen method for each sampled subset}
#'  \item{est}{ - the estimated SPVIM value for each feature}
#'  \item{ic_lst}{ - the influence functions for each sampled subset}
#'  \item{ic}{- a list of the SPVIM influence function contributions}
#'  \item{se}{ - the standard errors for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence intervals based on the variable importance estimates}
#'  \item{gamma}{- the fraction of the sample size used when sampling subsets}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{delta}{- the \code{delta} value used for hypothesis testing}
#'  \item{y}{ - the outcome}
#'  \item{weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimates, SEs, CIs, hypothesis testing decisions, and p-values}
#' }
#'
#' @examples
#' library(SuperLearner)
#' library(ranger)
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
#' learners <- c("SL.mean", "SL.ranger")
#'
#' ## -----------------------------------------
#' ## using Super Learner (with a small number of CV folds,
#' ## for illustration only)
#' ## -----------------------------------------
#' set.seed(4747)
#' est <- sp_vim(Y = y, X = x, V = 2, type = "r_squared", 
#' SL.library = learners, alpha = 0.05)
#' 
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @importFrom stats pnorm
#' @export
sp_vim <- function(Y, X, V = 5, weights = rep(1, length(Y)), type = "r_squared", 
                   SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), 
                   univariate_SL.library = NULL,
                   gamma = 1, alpha = 0.05, delta = 0, na.rm = FALSE,
                   stratified = FALSE, ...) {
    ## check to see if f1 and f2 are missing
    ## if the data is missing, stop and throw an error
    if (missing(Y)) stop("You must enter an outcome, Y.")
    if (missing(X)) stop("You must enter a matrix of predictors, X.")

    ## check to see if Y is a matrix or data.frame; if not, make it one (just for ease of reading)
    if(is.null(dim(Y))) Y <- as.matrix(Y)

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## set up the cross-validation
    outer_folds <- .make_folds(Y, V = 2, stratified = stratified, probs = c(0.25, 0.75))
    inner_folds_1 <- .make_folds(Y[outer_folds == 1, , drop = FALSE], V = V, stratified = stratified)
    inner_folds_2 <- .make_folds(Y[outer_folds == 2, , drop = FALSE], V = V, stratified = stratified)

    ## sample subsets, set up Z
    z_w_lst <- sample_subsets(p = dim(X)[2], n = dim(X)[1], gamma = gamma)
    Z <- z_w_lst$Z
    W <- z_w_lst$W
    z_counts <- z_w_lst$z_counts
    S <- z_w_lst$S

    ## get v, preds, ic for null set
    preds_none <- list()
    for (v in 1:V) {
        preds_none[[v]] <- rep(mean(Y[outer_folds == 2, ][inner_folds_2 == v]), sum(inner_folds_2 == v))
    }
    v_none <- cv_predictiveness_point_est(fitted_values = preds_none, y = Y[outer_folds == 2, , drop = FALSE], folds = inner_folds_2, weights = weights[outer_folds == 2], type = full_type, na.rm = na.rm)$point_est
    ic_none <- cv_predictiveness_update(preds_none, Y[outer_folds == 2, , drop = FALSE], inner_folds_2, weights[outer_folds == 2], type = full_type, na.rm = na.rm)$ic

    ## get v, preds, ic for remaining non-null groups in S
    preds_lst <- lapply(S[-1], function(s) run_sl(Y[outer_folds == 2, , drop = FALSE], X[outer_folds == 2, ], V = V, SL.library = SL.library, univariate_SL.library = univariate_SL.library, s = s, folds = inner_folds_2, ...))
    v_lst <- lapply(preds_lst, function(x) cv_predictiveness_point_est(fitted_values = x$preds, y = Y[outer_folds == 2, , drop = FALSE], folds = x$folds, weights = weights[outer_folds == 2], type = full_type, na.rm = na.rm)$point_est)
    ic_lst <- lapply(preds_lst, function(x) cv_predictiveness_update(fitted_values = x$preds, y = Y[outer_folds == 2, , drop = FALSE], folds = x$folds, weights = weights[outer_folds == 2], type = full_type, na.rm = na.rm)$ic)
    v <- matrix(c(v_none, unlist(v_lst)))
    ## do constrained wls
    A_W <- sqrt(W) %*% Z
    v_W <- sqrt(W) %*% v
    G <- rbind(c(1, rep(0, dim(X)[2])), rep(1, dim(X)[2] + 1) - c(1, rep(0, dim(X)[2])))
    c_n <- matrix(c(v_none, v[length(v)] - v_none), ncol = 1)
    kkt_matrix_11 <- 2 * t(A_W) %*% A_W
    kkt_matrix_12 <- t(G)
    kkt_matrix_21 <- G
    kkt_matrix_22 <- matrix(0, nrow = dim(kkt_matrix_21)[1],  ncol = dim(kkt_matrix_12)[2])
    kkt_matrix <- rbind(cbind(kkt_matrix_11, kkt_matrix_12), cbind(kkt_matrix_21, kkt_matrix_22))
    ls_matrix <- rbind(2 * t(A_W) %*% v_W, c_n)
    ls_solution <- MASS::ginv(kkt_matrix) %*% ls_matrix
    est <- ls_solution[1:(ncol(X) + 1), , drop = FALSE]
    lambdas <- ls_solution[(ncol(X) + 2):dim(ls_solution)[1], , drop = FALSE]

    ## compute the SPVIM ICs
    ic_mat <- do.call(rbind, c(list(ic_none), ic_lst))
    ics <- spvim_ics(Z, z_counts, W, v, est, G, c_n, ic_mat, full_type)

    ## calculate the standard error
    ses <- vector("numeric", ncol(X) + 1)
    for (j in 1:(ncol(X) + 1)) {
        ses[j] <- spvim_se(ics, j, gamma = gamma, na_rm = na.rm)
    }
    ## if est < 0, set to zero and print warning
    if (any(est < 0)) {
        est[est < 0] <- 0
        warning("One or more original estimates < 0; returning zero for these indices.")
    }

    ## calculate the confidence intervals
    cis <- vimp_ci(est[-1], ses[-1], scale = "identity", level = 1 - alpha)

    ## compute a hypothesis test against the null of zero importance
    preds_none_0 <- list()
    for (v in 1:V) {
        preds_none_0[[v]] <- rep(mean(Y[outer_folds == 1, ][inner_folds_1 == v]), sum(inner_folds_1 == v))
    }
    v_none_0 <- cv_predictiveness_point_est(fitted_values = preds_none_0, y = Y[outer_folds == 1, , drop = FALSE], folds = inner_folds_1, weights = weights[outer_folds == 1], type = full_type, na.rm = na.rm)$point_est
    ic_none_0 <- cv_predictiveness_update(preds_none_0, Y[outer_folds == 1, , drop = FALSE], inner_folds_1, weights[outer_folds == 1], type = full_type, na.rm = na.rm)$ic
    se_none_0 <- sqrt(mean(ic_none_0 ^ 2, na.rm = na.rm)) / sqrt(sum(outer_folds == 1))
    ## get shapley vals + null predictiveness
    shapley_vals_plus <- est + est[1]
    ses_one <- sqrt(ses ^ 2 + se_none_0 ^ 2)
    test_statistics <- sapply(2:length(est), function(j, ests, ses, est_0, se_0, delta) {
        (ests[j] - est_0 - delta) / sqrt(ses[j] ^ 2 + se_0 ^ 2)
    }, ests = shapley_vals_plus, ses = ses_one, est_0 = v_none_0, se_0 = se_none_0, delta = delta)
    p_values <- 1 - pnorm(test_statistics)
    hyp_tests <- p_values < alpha
    ## get the call
    cl <- match.call()

    ## create the output and return it
    ## create output tibble
    mat <- tibble::tibble(s = as.character(1:ncol(X)), est = est[-1], se = ses[-1], cil = cis[, 1],
                          ciu = cis[, 2], test = hyp_tests, p_value = p_values)
    output <- list(call = cl, s = as.character(1:ncol(X)),
                 SL.library = SL.library,
                 v = v,
                 preds_lst = c(list(preds_none), preds_lst),
                 est = est,
                 ic_lst = c(list(ic_none), ic_lst),
                 ic = ics,
                 se = ses, ci = cis,
                 test = hyp_tests,
                 p_value = p_values,
                 test_statistic = test_statistics,
                 gamma = gamma,
                 alpha = alpha,
                 delta = delta,
                 y = Y,
                 weights = weights,
                 scale = "identity",
                 mat = mat)

    ## make it also an vim object
    tmp.cls <- class(output)
    class(output) <- c("vim", type, tmp.cls)
    return(output)
}
