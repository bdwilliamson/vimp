#' Shapley Population Variable Importance Measure (SPVIM) estimates
#'
#' Compute estimates and confidence intervals for the
#' SPVIMs, using cross-validation.
#' This essentially involves splitting the data into V train/test splits; train the learners on the training data, evaluate importance on the test data; and average over these splits.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param V the number of folds for cross-validation, defaults to 10.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type the type of parameter (e.g., R-squared-based is \code{"r_squared"}).
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param gamma the fraction of the sample size to use when sampling subsets (e.g., \code{gamma = 1} samples the same number of subsets as the sample size)
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or logit ("logit") scale?
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details See the paper by Williamson and Feng (2020) for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' The function works by estimating
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{cv_vim}}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#' \item{v_lst}{- the estimated predictiveness measure for each sampled subset}
#'  \item{preds_lst}{ - the predicted values from the chosen method for each sampled subset}
#'  \item{est}{ - the estimated variable importance}
#'  \item{ic_lst}{ - the influence functions for each sampled subset}
#'  \item{ses}{ - the standard error for the estimated variable importance}
#'  \item{cis}{ - the \eqn{(1-\alpha) \times 100}\% confidence intervals based on the variable importance estimates}
#'  \item{gamma}{- the fraction of the sample size used when sampling subsets}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{delta}{- the \code{delta} value used for hypothesis testing}
#'  \item{y}{ - the outcome}
#'  \item{weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimates, SEs, CIs, hypothesis testing decisions, and p-values}
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
#' est <- sp_vim(Y = y, X = x, V = 5,
#' type = "r_squared",
#' SL.library = learners, alpha = 0.05)
#' }
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
sp_vim <- function(Y, X, V = 5, weights = rep(1, length(Y)), type = "r_squared", SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), gamma = 1, alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE, ...) {
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
    outer_folds <- sample(1:2, dim(Y)[1], replace = TRUE, prob = c(0.25, 0.75))
    inner_folds_1 <- rep_len(seq_len(V), dim(Y[outer_folds == 1, , drop = FALSE])[1])
    inner_folds_1 <- sample(inner_folds_1)
    inner_folds_2 <- rep_len(seq_len(V), dim(Y[outer_folds == 2, , drop = FALSE])[1])
    inner_folds_2 <- sample(inner_folds_2)

    ## sample subsets, set up Z
    z_w_lst <- sample_subsets(p = dim(X)[2], n = dim(X)[1], gamma = gamma)
    Z <- z_w_lst$Z
    W <- z_w_lst$W
    z_counts <- z_w_lst$z_counts
    S <- z_w_lst$S

    ## get v, preds, ic for null set
    preds_none <- list()
    for (v in 1:V) {
        preds_none[[v]] <- rep(mean(Y[outer_folds == 1, ][inner_folds_1 == v]), sum(inner_folds_1 == v))
    }
    v_none <- cv_predictiveness_point_est(fitted_values = preds_none, y = Y[outer_folds == 1, , drop = FALSE], folds = inner_folds_1, weights = weights[outer_folds == 1], type = full_type, na.rm = na.rm)$point_est
    ic_none <- cv_predictiveness_update(preds_none, Y[outer_folds == 1, , drop = FALSE], inner_folds_1, weights[outer_folds == 1], type = full_type, na.rm = na.rm)$ic

    ## get v, preds, ic for remaining non-null groups in S
    preds_lst <- lapply(S[-1], function(s) run_sl(Y[outer_folds == 2, , drop = FALSE], X[outer_folds == 2, ], V = V, SL.library = SL.library, s = s, folds = inner_folds_2, ...))
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
    ls_solution <- solve(kkt_matrix) %*% ls_matrix
    est <- ls_solution[2:dim(X)[2], ]
    lambdas <- ls_solution[(dim(X)[2] + 1):dim(ls_solution)[1], ]
    
    ## calculate the standard error
    se <- vimp_se(est, update, scale = scale, na.rm = na.rm)
    ses <- vector("numeric", length = V)
    for (v in 1:V) {
        ses[v] <- vimp_se(all_ests[v], updates[, v], scale = scale, na.rm = na.rm)
    }
    ## if est < 0, set to zero and print warning
    if (est < 0) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    }

    ## calculate the confidence interval
    ci <- vimp_ci(est, se, scale = scale, 1 - alpha)
    predictiveness_ci_full <- vimp_ci(predictiveness_full, se = vimp_se(predictiveness_full, cv_predictiveness_update(fhat_ful, Y[outer_folds == 1, , drop = FALSE], inner_folds_1, weights[outer_folds == 1], type = full_type, na.rm = na.rm)$ic, scale = scale), scale = scale, level = 1 - alpha)
    predictiveness_ci_redu <- vimp_ci(predictiveness_redu, se = vimp_se(predictiveness_redu, cv_predictiveness_update(fhat_red, Y[outer_folds == 2, , drop = FALSE], inner_folds_2, weights[outer_folds == 2], type = full_type, na.rm = na.rm)$ic, scale = scale), scale = scale, level = 1 - alpha)

    ## compute a hypothesis test against the null of zero importance
    ## note that for full risk for fold 1 is first-order independent of the V-1 other reduced-fold risks
    if (type == "regression" | type == "anova") {
        hyp_test <- list(test = NA, p_value = NA, predictiveness_full = NA, predictiveness_reduced = NA, predictiveness_ci_full = rep(NA, 2), predictiveness_ci_reduced = rep(NA, 2), se_full = NA, se_redu = NA, test_statistic = NA)
    } else {
        ## reject iff ALL pairwise comparisons with the V-1 other risk CIs don't overlap
        hyp_test <- vimp_hypothesis_test(fhat_ful, fhat_red, Y, folds, delta = delta, weights = weights, type = type, alpha = alpha, cv = TRUE, scale = scale, na.rm = na.rm)
    }

    ## get the call
    cl <- match.call()

    ## create the output and return it
    ## create output tibble
    chr_indx <- paste(as.character(indx), collapse = ",")
    mat <- tibble::tibble(s = chr_indx, est = est, se = se[1], cil = ci[1], ciu = ci[2], test = hyp_test$test, p_value = hyp_test$p_value)
    output <- list(call = cl, s = chr_indx,
                 SL.library = SL.library,
                 full_fit = fhat_ful, red_fit = fhat_red,
                 est = est,
                 naive = naive,
                 update = update,
                 se = se, ci = ci,
                 predictiveness_full = predictiveness_full,
                 predictiveness_reduced = predictiveness_redu,
                 predictiveness_ci_full = predictiveness_ci_full,
                 predictiveness_ci_reduced = predictiveness_ci_redu,
                 test = hyp_test$test,
                 p_value = hyp_test$p_value,
                 hyp_test_predictiveness_full = hyp_test$predictiveness_full,
                 hyp_test_predictiveness_reduced = hyp_test$predictiveness_reduced,
                 hyp_test_predictiveness_ci_full = hyp_test$predictiveness_ci_full,
                 hyp_test_predictiveness_ci_reduced = hyp_test$predictiveness_ci_reduced,
                 hyp_test_se_full = hyp_test$se_full,
                 hyp_test_se_reduced = hyp_test$se_redu,
                 test_statistic = hyp_test$test_statistic,
                 full_mod = full,
                 red_mod = reduced,
                 alpha = alpha,
                 delta = delta,
                 folds = folds,
                 y = Y,
                 weights = weights,
                 scale = scale,
                 mat = mat)

    ## make it also an vim object
    tmp.cls <- class(output)
    class(output) <- c("vim", type, tmp.cls)
    return(output)
}
