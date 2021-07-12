#' Shapley Population Variable Importance Measure (SPVIM) Estimates and Inference
#'
#' Compute estimates and confidence intervals for the SPVIMs, using cross-fitting.
#'
#' @inheritParams cv_vim
#' @param univariate_SL.library (optional) a character vector of learners to
#'   pass to \code{SuperLearner} for estimating univariate regression functions.
#'   Defaults to \code{SL.polymars}
#' @param gamma the fraction of the sample size to use when sampling subsets
#'   (e.g., \code{gamma = 1} samples the same number of subsets as the sample
#'   size)
#' @param verbose should \code{sp_vim} and \code{SuperLearner} print out
#'   progress? (defaults to \code{FALSE})
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details We define the SPVIM as the weighted average of the population
#' difference in predictiveness over all subsets of features not containing
#' feature \eqn{j}.
#'
#' This is equivalent to finding the solution to a population weighted least
#' squares problem. This key fact allows us to estimate the SPVIM using weighted
#' least squares, where we first sample subsets from the power set of all
#' possible features using the Shapley sampling distribution; then
#' use cross-fitting to obtain estimators of the predictiveness of each
#' sampled subset; and finally, solve the least squares problem given in
#' Williamson and Feng (2020).
#'
#' See the paper by Williamson and Feng (2020) for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#'
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \describe{
#'  \item{SL.library}{the library of learners passed to \code{SuperLearner}}
#'  \item{v}{the estimated predictiveness measure for each sampled subset}
#'  \item{fit_lst}{the fitted values on the entire dataset from the chosen method for each sampled subset}
#'  \item{preds_lst}{the cross-fitted predicted values from the chosen method for each sampled subset}
#'  \item{est}{the estimated SPVIM value for each feature}
#'  \item{ics}{the influence functions for each sampled subset}
#'  \item{var_v_contribs}{the contibutions to the variance from estimating predictiveness}
#'  \item{var_s_contribs}{the contributions to the variance from sampling subsets}
#'  \item{ic_lst}{a list of the SPVIM influence function contributions}
#'  \item{se}{the standard errors for the estimated variable importance}
#'  \item{ci}{the \eqn{(1-\alpha) \times 100}\% confidence intervals based on the variable importance estimates}
#'  \item{p_value}{p-values for the null hypothesis test of zero importance for each variable}
#'  \item{test_statistic}{the test statistic for each null hypothesis test of zero importance}
#'  \item{test}{a hypothesis testing decision for each null hypothesis test (for each variable having zero importance)}
#'  \item{gamma}{the fraction of the sample size used when sampling subsets}
#'  \item{alpha}{the level, for confidence interval calculation}
#'  \item{delta}{the \code{delta} value used for hypothesis testing}
#'  \item{y}{the outcome}
#'  \item{ipc_weights}{the weights}
#'  \item{scale}{the scale on which CIs were computed}
#'  \item{mat}{- a tibble with the estimates, SEs, CIs, hypothesis testing decisions, and p-values}
#' }
#'
#' @examples
#' n <- 100
#' p <- 2
#' # generate the data
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' # apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' # generate Y ~ Normal (smooth, 1)
#' y <- as.matrix(smooth + stats::rnorm(n, 0, 1))
#'
#' # set up a library for SuperLearner; note simple library for speed
#' library("SuperLearner")
#' learners <- c("SL.glm")
#'
#' # -----------------------------------------
#' # using Super Learner (with a small number of CV folds,
#' # for illustration only)
#' # -----------------------------------------
#' set.seed(4747)
#' est <- sp_vim(Y = y, X = x, V = 2, type = "r_squared",
#' SL.library = learners, alpha = 0.05)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the
#'   \code{SuperLearner} function and package.
#' @importFrom stats pnorm gaussian
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom MASS ginv
#' @export
sp_vim <- function(Y = NULL, X = NULL, V = 5, type = "r_squared",
                   SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                   univariate_SL.library = NULL,
                   gamma = 1, alpha = 0.05, delta = 0, na.rm = FALSE,
                   stratified = FALSE, verbose = FALSE, sample_splitting = TRUE,
                   C = rep(1, length(Y)), Z = NULL, ipc_weights = rep(1, length(Y)),
                   ipc_est_type = "aipw", scale = "identity", scale_est = TRUE,
                   cross_fitted_se = TRUE, ...) {
    # if the data is missing, stop and throw an error
    if (is.null(Y)) stop("You must enter an outcome, Y.")
    if (is.null(X)) stop("You must enter a matrix of predictors, X.")

    if (sample_splitting) {
        ss_V <- 2 * V
    } else {
        ss_V <- V
    }

    # check to see if Y is a matrix or data.frame; if not, make it one
    # (just for ease of reading)
    if (is.null(dim(Y))) {
        Y <- as.matrix(Y)
    }

    # set up internal data -- based on complete cases only
    cc_lst <- create_z(Y, C, Z, X, ipc_weights)
    Y_cc <- cc_lst$Y
    weights_cc <- cc_lst$weights
    Z_in <- cc_lst$Z
    X_cc <- subset(X, C == 1)

    # get the correct measure function; if not one of the supported ones, say so
    full_type <- get_full_type(type)

    # set up the cross-fitting and sample-splitting folds, for fitting
    cross_fitting_folds <- make_folds(Y, V = ss_V, stratified = stratified,
                                      C = C)
    sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
    cross_fitting_folds_cc <- cross_fitting_folds[C == 1]
    # set up the cross-fitting folds, for predictiveness estimation
    if (sample_splitting) {
        # make new sets of folds, as if we had done V-fold within the two sets
        k_fold_lst <- make_kfold(cross_fitting_folds, sample_splitting_folds, C)
        full_test <- (k_fold_lst$sample_splitting_folds == 1)
        redu_test <- (k_fold_lst$sample_splitting_folds == 2)
    } else {
        # no need to do anything
        k_fold_lst <- list(
            full = cross_fitting_folds, reduced = cross_fitting_folds
        )
        full_test <- rep(TRUE, length(cross_fitting_folds))
        redu_test <- rep(TRUE, length(cross_fitting_folds))
    }
    cf_folds_full <- k_fold_lst$full
    cf_folds_redu <- k_fold_lst$reduced
    cf_folds_full_cc <- cf_folds_full[C[full_test] == 1]
    cf_folds_redu_cc <- cf_folds_redu[C[redu_test] == 1]
    full_test_cc <- full_test[C == 1]
    redu_test_cc <- redu_test[C == 1]

    # sample subsets, set up Z
    z_w_lst <- sample_subsets(p = ncol(X), n = nrow(X), gamma = gamma)
    Z <- z_w_lst$Z
    W <- z_w_lst$W
    z_counts <- z_w_lst$z_counts
    S <- z_w_lst$S

    arg_lst <- list(...)
    if (!is.null(names(arg_lst)) && any(grepl("cvControl", names(arg_lst)))) {
        arg_lst$cvControl$stratifyCV <- FALSE
    }
    if (is.null(arg_lst$family)) {
        arg_lst$family <- switch(
            (length(unique(Y_cc)) == 2) + 1, stats::gaussian(),
            stats::binomial()
        )
    }
    arg_lst_cv <- arg_lst
    if (is.null(arg_lst_cv$innerCvControl)) {
        arg_lst_cv$innerCvControl <- rep(list(list(V = V)), ss_V)
    }

    # get v, preds, ic for null set
    preds_none <- list()
    fitted_none <- list()
    none_index_v <- 1
    for (v in seq_len(ss_V)) {
        if (sample_splitting_folds[v] == 1 | !sample_splitting) {
            preds_none[[none_index_v]] <- rep(
                mean(Y_cc[cross_fitting_folds_cc == v]), sum(cross_fitting_folds_cc == v)
            )
            none_index_v <- none_index_v + 1
        }
        fitted_none[[v]] <- rep(
            mean(Y_cc[cross_fitting_folds_cc == v]), sum(cross_fitting_folds_cc == v)
        )
    }
    v_none_lst <- do.call(
        est_predictiveness_cv,
        args = c(
            list(fitted_values = preds_none,
                 y = Y_cc[full_test_cc], full_y = Y_cc,
                 folds = cf_folds_full_cc, C = C[full_test],
                 Z = Z_in[full_test, , drop = FALSE],
                 folds_Z = cf_folds_full,
                 ipc_weights = ipc_weights[full_test],
                 ipc_fit_type = "SL", type = full_type, scale = scale,
                 ipc_est_type = ipc_est_type, na.rm = na.rm,
                 SL.library = SL.library),
            arg_lst
        ), quote = TRUE
    )
    v_none <- v_none_lst$point_est
    ic_none <- v_none_lst$eif
    ics_none <- v_none_lst$all_eifs

    # get v, preds, ic for remaining non-null groups in S
    if (verbose) {
        message("Fitting learners. Progress:")
        progress_bar <- txtProgressBar(min = 0, max = length(S[-1]), style = 3)
    } else {
        progress_bar <- NULL
    }
    preds_lst <- sapply(
        1:length(S[-1]),
        function(i) {
            do.call(run_sl, args = c(
            list(Y_cc, X_cc, V = ss_V, SL.library = SL.library,
                 univariate_SL.library = univariate_SL.library, s = S[-1][[i]],
                 cv_folds = cross_fitting_folds_cc, sample_splitting = sample_splitting,
                 ss_folds = sample_splitting_folds, verbose = verbose,
                 progress_bar = progress_bar, indx = i, full = TRUE,
                 weights = weights_cc),
            arg_lst_cv
        ))}, simplify = FALSE
    )
    if (verbose) {
        close(progress_bar)
    }
    v_eif_lst <- lapply(
        preds_lst,
        function(l)
            do.call(
                est_predictiveness_cv,
                args = c(
                    list(fitted_values = l$preds,
                         y = Y_cc[full_test_cc], full_y = Y_cc,
                         folds = cf_folds_full_cc, C = C[full_test],
                         Z = Z_in[full_test, , drop = FALSE],
                         folds_Z = cf_folds_full,
                         ipc_weights = ipc_weights[full_test],
                         type = full_type, ipc_fit_type = "SL", scale = scale,
                         ipc_est_type = ipc_est_type, na.rm = na.rm,
                         SL.library = SL.library),
                    arg_lst
                ), quote = TRUE
            )
    )
    v_lst <- lapply(v_eif_lst, function(lst) lst$point_est)
    ics_lst <- lapply(v_eif_lst, function(lst) lst$all_eifs)
    ic_lst <- lapply(v_eif_lst, function(lst) lst$eif)
    if (!cross_fitted_se) {
        ic_all_lst <- lapply(
            preds_lst,
            function(l)
                do.call(
                    est_predictiveness_cv,
                    args = c(
                        list(fitted_values = l$preds_non_cf_se,
                             y = Y_cc, full_y = Y_cc, folds = cross_fitting_folds_cc,
                             C = C, Z = Z_in, folds_Z = cross_fitting_folds,
                             ipc_weights = ipc_weights,
                             type = full_type, ipc_fit_type = "SL", scale = scale,
                             ipc_est_type = ipc_est_type, na.rm = na.rm,
                             SL.library = SL.library),
                        arg_lst
                    ), quote = TRUE
                )
        )
        ics_lst <- lapply(ic_all_lst, function(l) l$all_eifs)
        ic_lst <- lapply(ic_all_lst, function(l) l$eif)
    } 
    v <- matrix(c(v_none, unlist(v_lst)))
    # do constrained wls
    if (verbose) {
        message("Fitting weighted least squares to estimate the SPVIM values.")
    }
    A_W <- sqrt(W) %*% Z
    v_W <- sqrt(W) %*% v
    G <- rbind(c(1, rep(0, ncol(X))), rep(1, ncol(X) + 1) - c(1, rep(0, ncol(X))))
    c_n <- matrix(c(v_none, v[length(v)] - v_none), ncol = 1)
    kkt_matrix_11 <- 2 * t(A_W) %*% A_W
    kkt_matrix_12 <- t(G)
    kkt_matrix_21 <- G
    kkt_matrix_22 <- matrix(0, nrow = nrow(kkt_matrix_21),
                            ncol = ncol(kkt_matrix_12))
    kkt_matrix <- rbind(cbind(kkt_matrix_11, kkt_matrix_12),
                        cbind(kkt_matrix_21, kkt_matrix_22))
    ls_matrix <- rbind(2 * t(A_W) %*% v_W, c_n)
    ls_solution <- MASS::ginv(kkt_matrix) %*% ls_matrix
    est <- ls_solution[1:(ncol(X) + 1), , drop = FALSE]
    lambdas <- ls_solution[(ncol(X) + 2):nrow(ls_solution), , drop = FALSE]

    # compute the SPVIM ICs and standard errors
    ses <- vector("numeric", ncol(X) + 1)
    var_v_contribs <- vector("numeric", ncol(X) + 1)
    var_s_contribs <- vector("numeric", ncol(X) + 1)
    if (cross_fitted_se) {
        all_ics_lst <- c(list(ics_none), ics_lst)
        ics <- lapply(as.list(seq_len(V)), function(l) {
            spvim_ics(Z, z_counts, W, v, est, G, c_n, lapply(all_ics_lst, function(k) k[[l]]),
                      full_type)
        })
        for (j in 1:(ncol(X) + 1)) {
            se_lst <- lapply(ics, spvim_se, idx = j, gamma = gamma, na_rm = na.rm)
            var <- mean(unlist(lapply(se_lst, function(l) l$se ^ 2)))
            ses[j] <- sqrt(var)
            var_v_contribs[j] <- mean(unlist(lapply(se_lst, function(l) l$var_v_contrib)))
            var_s_contribs[j] <- mean(unlist(lapply(se_lst, function(l) l$var_s_contrib)))
        }
        n_for_v <- min(unlist(lapply(ics, function(l) ncol(l$contrib_v))))
    } else {
        all_ics_lst <- c(list(ic_none), ic_lst)
        ics <- spvim_ics(Z, z_counts, W, v, est, G, c_n, all_ics_lst, full_type)
        for (j in 1:(ncol(X) + 1)) {
            ses_res <- spvim_se(ics, j, gamma = gamma, na_rm = na.rm)
            ses[j] <- ses_res$se
            var_v_contribs[j] <- ses_res$var_v_contrib
            var_s_contribs[j] <- ses_res$var_s_contrib
        }
        n_for_v <- ncol(ics)
    }

    # if est < 0, set to zero and print warning
    if (any(est < 0) & scale_est) {
        est[est < 0] <- 0
        warning("One or more original estimates < 0; returning zero for these indices.")
    }

    # calculate the confidence intervals
    cis <- vimp_ci(est[-1], ses[-1], scale = scale, level = 1 - alpha)

    # compute a hypothesis test against the null of zero importance
    preds_none_0 <- list()
    none_index_v <- 1
    for (v in seq_len(ss_V)) {
        if (sample_splitting_folds[v] == 2 | !sample_splitting) {
            preds_none_0[[none_index_v]] <- rep(
                mean(Y_cc[cross_fitting_folds_cc == v]), sum(cross_fitting_folds_cc == v)
            )
            none_index_v <- none_index_v + 1
        }
    }
    v_none_0_lst <- do.call(
        est_predictiveness_cv,
        args = c(
            list(fitted_values = preds_none_0,
                 y = Y_cc[redu_test_cc], full_y = Y_cc,
                 folds = cf_folds_redu_cc, C = C[redu_test],
                 Z = Z_in[redu_test, , drop = FALSE],
                 folds_Z = cf_folds_redu,
                 ipc_weights = ipc_weights[redu_test],
                 ipc_fit_type = "SL", type = full_type, scale = scale,
                 ipc_est_type = ipc_est_type, na.rm = na.rm,
                 SL.library = SL.library),
            arg_lst
        ), quote = TRUE
    )
    v_none_0 <- v_none_0_lst$point_est
    ics_none_0 <- v_none_0_lst$all_eifs
    ic_none_0 <- v_none_0_lst$eif
    if (cross_fitted_se) {
        var_none_0 <- mean(unlist(lapply(ics_none_0, 
                                         function(ic) mean(ic ^ 2, na.rm = na.rm))))
    } else {
        var_none_0 <- mean(ic_none_0 ^ 2, na.rm = na.rm)
    }
    se_none_0 <- sqrt(var_none_0) /
        sqrt(length(cross_fitting_folds_cc) / 2)
    # get shapley vals + null predictiveness
    shapley_vals_plus <- est + est[1]
    ses_one <- sqrt((var_v_contribs * n_for_v + var_s_contribs) /
                        (length(cross_fitting_folds_cc) / 2) +
                        se_none_0 ^ 2)
    test_statistics <- unlist(lapply(
        as.list(2:length(est)),
        function(j, ests, ses, est_0, se_0, delta) {
            var_j <- (var_v_contribs[j] * n_for_v + var_s_contribs[j]) /
                (length(cross_fitting_folds_cc) / 2)
            (ests[j] - est_0 - delta) / sqrt(var_j + se_0 ^ 2)
        }, ests = shapley_vals_plus, ses = ses_one, est_0 = v_none_0,
        se_0 = se_none_0, delta = delta
    ))
    p_values <- 1 - pnorm(test_statistics)
    hyp_tests <- p_values < alpha

    # create the output and return it
    mat <- tibble::tibble(
        s = as.character(1:ncol(X)), est = est[-1], se = ses[-1], cil = cis[, 1],
        ciu = cis[, 2], test = hyp_tests, p_value = p_values,
        var_v_contribs = var_v_contribs[-1], var_s_contribs = var_s_contribs[-1]
    )
    output <- list(s = as.character(1:ncol(X)),
                   SL.library = SL.library,
                   v = v,
                   preds_lst = c(list(preds_none), lapply(preds_lst,
                       function(l) l$preds
                   )),
                   est = est,
                   ic_lst = all_ics_lst,
                   ic = ics,
                   se = ses,
                   var_v_contribs = var_v_contribs,
                   var_s_contribs = var_s_contribs,
                   ci = cis,
                   test = hyp_tests,
                   p_value = p_values,
                   test_statistic = test_statistics,
                   gamma = gamma,
                   alpha = alpha,
                   delta = delta,
                   y = Y,
                   ipc_weights = ipc_weights,
                   scale = "identity",
                   mat = mat)

    # make it also an vim object
    tmp.cls <- class(output)
    class(output) <- c("vim", type, tmp.cls)
    return(output)
}
