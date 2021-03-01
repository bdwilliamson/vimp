#' Shapley Population Variable Importance Measure (SPVIM) Estimates and Inference
#'
#' Compute estimates and confidence intervals for the SPVIMs, using cross-fitting.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param V the number of folds for cross-fitting, defaults to 10.
#' @param type the type of parameter (e.g., R-squared-based is \code{"r_squared"}). 
#'   Note that \code{type = 'anova'} is not allowed for SPVIMs.
#' @param SL.library a character vector of learners to pass to 
#'   \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. 
#'   Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param univariate_SL.library (optional) a character vector of learners to 
#'   pass to \code{SuperLearner} for estimating univariate regression functions. 
#'   Defaults to \code{SL.polymars}
#' @param gamma the fraction of the sample size to use when sampling subsets 
#'   (e.g., \code{gamma = 1} samples the same number of subsets as the sample 
#'   size)
#' @param alpha the level to compute the confidence interval at. 
#'   Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if 
#'   importance < \eqn{\delta}); defaults to 0.
#' @param na.rm should we remove NA's in the outcome and fitted values in 
#'   computation? (defaults to \code{FALSE})
#' @param stratified should the generated folds be stratified based on the 
#'   outcome (helps to ensure class balance across cross-fitting folds)?
#' @param verbose should \code{sp_vim} and \code{SuperLearner} print out 
#'   progress? (defaults to \code{FALSE})
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes 
#'   unobserved).
#' @param Z either (i) NULL (the default, in which case the argument 
#'   \code{C} above must be all ones), or (ii) a character vector specifying 
#'   the variable(s) among Y and X that are thought to play a role in the 
#'   coarsening mechanism.
#' @param ipc_weights weights for the computed influence curve (i.e., inverse 
#'   probability weights for coarsened-at-random settings). Assumed to be 
#'   already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_est_type the type of procedure used for coarsened-at-random 
#'   settings; options are "ipw" (for inverse probability weighting) or 
#'   "aipw" (for augmented inverse probability weighting). 
#'   Only used if \code{C} is not all equal to 1.
#' @param scale should CIs be computed on original ("identity") or logit 
#'   ("logit") scale?
#' @param ... other arguments to the estimation tool, see "See also".
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
#' The function works by estimating
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
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
#'  \item{ipc_weights}{ - the weights}
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
#' learners <- c("SL.glm", "SL.mean")
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
                   stratified = FALSE, verbose = FALSE, C = rep(1, length(Y)),
                   Z = NULL, ipc_weights = rep(1, length(Y)), 
                   ipc_est_type = "aipw", scale = "identity", 
                   ...) {
    # if the data is missing, stop and throw an error
    if (is.null(Y)) stop("You must enter an outcome, Y.")
    if (is.null(X)) stop("You must enter a matrix of predictors, X.")

    # check to see if Y is a matrix or data.frame; if not, make it one 
    # (just for ease of reading)
    if(is.null(dim(Y))) Y <- as.matrix(Y)

    # set up internal data -- based on complete cases only
    Y_cc <- subset(Y, C == 1, drop = FALSE)
    X_cc <- subset(X, C == 1, drop = FALSE)
    weights_cc <- ipc_weights[C == 1]
    if (!all(C == 1) || !all(ipc_weights == 1)) {
        if (is.character(Z)) {
            tmp_Z <- Z[Z != "Y"]
            minus_X <- as.numeric(gsub("X", "", tmp_Z))
            # check to see if it is only part of X matrix
            if (any(sapply(seq_along(minus_X), function(j) length(minus_X[j]) > 0))) {
                if (any(grepl("Y", Z))) {
                    Z_in <- as.data.frame(mget("Y"))
                } else {
                    Z_in <- NULL
                }
                Z_in <- cbind.data.frame(Z_in, X[, minus_X])
            } else {
                Z_in <- as.data.frame(mget(Z))
            }
        } else {
            stop("Please enter a character vector corresponding to the names of the fully observed data.")
        }
    } else {
        Z_in <- NULL
    }

    # get the correct measure function; if not one of the supported ones, say so
    full_type <- get_full_type(type)

    # set up the cross-fitting
    outer_folds <- .make_folds(Y, V = 2, stratified = stratified, 
                               C = C, probs = c(0.25, 0.75))
    inner_folds_1 <- .make_folds(Y[outer_folds == 1, , drop = FALSE], 
                                 C = C[outer_folds == 1], V = V, 
                                 stratified = stratified)
    inner_folds_2 <- .make_folds(Y[outer_folds == 2, , drop = FALSE], 
                                 C = C[outer_folds == 2], V = V, 
                                 stratified = stratified)
    outer_folds_cc <- outer_folds[C == 1]
    inner_folds_1_cc <- inner_folds_1[C[outer_folds == 1] == 1]
    inner_folds_2_cc <- inner_folds_2[C[outer_folds == 2] == 1]

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

    # get v, preds, ic for null set
    preds_none <- list()
    for (v in 1:V) {
        preds_none[[v]] <- rep(
            mean(Y_cc[(outer_folds_cc == 2), ][inner_folds_2_cc == v]), 
            sum(inner_folds_2_cc == v)
        )
    }
    v_none_lst <- do.call(
        est_predictiveness_cv,
        args = c(
            list(fitted_values = preds_none, 
                 y = Y_cc[outer_folds_cc == 2, , drop = FALSE], full_y = Y_cc,
                 folds = inner_folds_2_cc, C = C[outer_folds == 2], 
                 Z = Z_in[outer_folds == 2, , drop = FALSE], 
                 folds_Z = inner_folds_2, ipc_weights = ipc_weights[outer_folds == 2], 
                 ipc_fit_type = "SL", type = full_type, scale = scale, 
                 ipc_est_type = ipc_est_type, na.rm = na.rm, 
                 SL.library = SL.library),
            arg_lst
        )
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
        function(i) run_sl(
            Y_cc[(outer_folds_cc == 2), , drop = FALSE], 
            X_cc[(outer_folds_cc == 2), ], V = V, SL.library = SL.library, 
            univariate_SL.library = univariate_SL.library, s = S[-1][[i]], 
            folds = inner_folds_2_cc, verbose = verbose, 
            progress_bar = progress_bar, indx = i, 
            weights = weights_cc[(outer_folds_cc == 2)], ...
        ), simplify = FALSE
    )
    if (verbose) {
        close(progress_bar)
    }
    v_full_lst <- lapply(
        preds_lst, 
        function(l) 
            do.call(
                est_predictiveness_cv,
                args = c(
                    list(fitted_values = l$preds, 
                         y = Y_cc[outer_folds_cc == 2, , drop = FALSE], 
                         full_y = Y_cc,
                         folds = l$folds, C = C[outer_folds == 2], 
                         Z = Z_in[outer_folds == 2, , drop = FALSE], 
                         folds_Z = inner_folds_2, 
                         ipc_weights = ipc_weights[outer_folds == 2], 
                         type = full_type, ipc_fit_type = "SL", scale = scale, 
                         ipc_est_type = ipc_est_type, na.rm = na.rm, 
                         SL.library = SL.library),
                    arg_lst
                )
            )
    )
    v_lst <- lapply(v_full_lst, function(l) l$point_est)
    ic_lst <- lapply(v_full_lst, function(l) l$eif)
    ics_lst <- lapply(v_full_lst, function(l) l$all_eifs)
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

    # compute the SPVIM ICs
    all_ics_lst <- c(list(ics_none), ics_lst)
    ics <- spvim_ics(Z, z_counts, W, v, est, G, c_n, all_ics_lst, full_type)

    # calculate the standard error
    ses <- vector("numeric", ncol(X) + 1)
    var_v_contribs <- vector("numeric", ncol(X) + 1)
    var_s_contribs <- vector("numeric", ncol(X) + 1)
    for (j in 1:(ncol(X) + 1)) {
        ses_res <- spvim_se(ics, j, gamma = gamma, na_rm = na.rm)
        ses[j] <- ses_res$se
        var_v_contribs[j] <- ses_res$var_v_contrib
        var_s_contribs[j] <- ses_res$var_s_contrib
    }
    # if est < 0, set to zero and print warning
    if (any(est < 0)) {
        est[est < 0] <- 0
        warning("One or more original estimates < 0; returning zero for these indices.")
    }

    # calculate the confidence intervals
    cis <- vimp_ci(est[-1], ses[-1], scale = scale, level = 1 - alpha)

    # compute a hypothesis test against the null of zero importance
    preds_none_0 <- list()
    for (v in 1:V) {
        preds_none_0[[v]] <- rep(
            mean(Y_cc[(outer_folds_cc == 1), ][inner_folds_1_cc == v]), 
            sum(inner_folds_1_cc == v)
        )
    }
    v_none_0_lst <- do.call(
        est_predictiveness_cv,
        args = c(
            list(fitted_values = preds_none_0, 
                 y = Y_cc[outer_folds_cc == 1, , drop = FALSE], 
                 full_y = Y_cc,
                 folds = inner_folds_1_cc, C = C[outer_folds == 1], 
                 Z = Z_in[outer_folds == 1, , drop = FALSE], 
                 folds_Z = inner_folds_1, 
                 ipc_weights = ipc_weights[outer_folds == 1], 
                 type = full_type, ipc_fit_type = "SL", scale = scale, 
                 ipc_est_type = ipc_est_type, na.rm = na.rm, 
                 SL.library = SL.library),
            arg_lst
        )
    )
    v_none_0 <- v_none_0_lst$point_est
    ic_none_0 <- v_none_0_lst$eif
    se_none_0 <- sqrt(mean(ic_none_0 ^ 2, na.rm = na.rm)) / 
        sqrt(sum(outer_folds == 1))
    # get shapley vals + null predictiveness
    shapley_vals_plus <- est + est[1]
    ses_one <- sqrt(ses ^ 2 + se_none_0 ^ 2)
    test_statistics <- sapply(
        2:length(est), 
        function(j, ests, ses, est_0, se_0, delta) {
            (ests[j] - est_0 - delta) / sqrt(ses[j] ^ 2 + se_0 ^ 2)
        }, ests = shapley_vals_plus, ses = ses_one, est_0 = v_none_0, 
        se_0 = se_none_0, delta = delta
    )
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
                   preds_lst = c(list(preds_none), preds_lst),
                   est = est,
                   ic_lst = c(list(ic_none), ic_lst),
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
