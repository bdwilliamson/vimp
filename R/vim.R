#' Nonparametric Intrinsic Variable Importance Estimates and Inference
#'
#' Compute estimates of and confidence intervals for nonparametric intrinsic
#' variable importance based on the population-level contrast between the oracle
#' predictiveness using the feature(s) of interest versus not.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the fitted values from a flexible estimation technique
#'   regressing Y on X.
#' @param f2 the fitted values from a flexible estimation technique
#'   regressing either (a) \code{f1} or (b) Y on X withholding the columns in
#'   \code{indx}.
#' @param indx the indices of the covariate(s) to calculate variable
#'   importance for; defaults to 1.
#' @param type the type of importance to compute; defaults to
#'   \code{r_squared}, but other supported options are \code{auc},
#'   \code{accuracy}, \code{deviance}, and \code{anova}.
#' @param run_regression if outcome Y and covariates X are passed to
#'   \code{vimp_accuracy}, and \code{run_regression} is \code{TRUE},
#'   then Super Learner will be used; otherwise, variable importance
#'   will be computed using the inputted fitted values.
#' @param SL.library a character vector of learners to pass to
#'   \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X,
#'   respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost},
#'   and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at.
#'   Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if
#'   importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or
#'   logit ("logit") scale?
#' @param na.rm should we remove NAs in the outcome and fitted values
#'   in computation? (defaults to \code{FALSE})
#' @param sample_splitting should we use sample-splitting to estimate the full and
#'   reduced predictiveness? Defaults to \code{TRUE}, since inferences made using
#'   \code{sample_splitting = FALSE} will be invalid for variable with truly zero
#'   importance.
#' @param sample_splitting_folds the folds used for sample-splitting;
#'   these identify the observations that should be used to evaluate
#'   predictiveness based on the full and reduced sets of covariates, respectively.
#'   Only used if \code{run_regression = FALSE}.
#' @param stratified if run_regression = TRUE, then should the generated
#'   folds be stratified based on the outcome (helps to ensure class balance
#'   across cross-validation folds)
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either (i) NULL (the default, in which case the argument
#'   \code{C} above must be all ones), or (ii) a character vector
#'   specifying the variable(s) among Y and X that are thought to play a
#'   role in the coarsening mechanism.
#' @param ipc_weights weights for the computed influence curve (i.e.,
#'   inverse probability weights for coarsened-at-random settings).
#'   Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated
#'   probability weights]).
#' @param ipc_est_type the type of procedure used for coarsened-at-random
#'   settings; options are "ipw" (for inverse probability weighting) or
#'   "aipw" (for augmented inverse probability weighting).
#'   Only used if \code{C} is not all equal to 1.
#' @param scale_est should the point estimate be scaled to be greater than 0?
#'   Defaults to \code{TRUE}.
#' @param bootstrap should bootstrap-based standard error estimates be computed?
#'   Defaults to \code{FALSE} (and currently may only be used if
#'   \code{sample_splitting = FALSE}).
#' @param b the number of bootstrap replicates (only used if \code{bootstrap = TRUE}
#'   and \code{sample_splitting = FALSE}).
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of classes \code{vim} and the type of risk-based measure.
#'   See Details for more information.
#'
#' @details We define the population variable importance measure (VIM) for the
#' group of features (or single feature) \eqn{s} with respect to the
#' predictiveness measure \eqn{V} by
#' \deqn{\psi_{0,s} := V(f_0, P_0) - V(f_{0,s}, P_0),} where \eqn{f_0} is
#' the population predictiveness maximizing function, \eqn{f_{0,s}} is the
#' population predictiveness maximizing function that is only allowed to access
#' the features with index not in \eqn{s}, and \eqn{P_0} is the true
#' data-generating distribution. VIM estimates are obtained by obtaining
#' estimators \eqn{f_n} and \eqn{f_{n,s}} of \eqn{f_0} and \eqn{f_{0,s}},
#' respectively; obtaining an estimator \eqn{P_n} of \eqn{P_0}; and finally,
#' setting \eqn{\psi_{n,s} := V(f_n, P_n) - V(f_{n,s}, P_n)}.
#'
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list including:
#' \describe{
#'  \item{s}{the column(s) to calculate variable importance for}
#'  \item{SL.library}{the library of learners passed to \code{SuperLearner}}
#'  \item{type}{the type of risk-based variable importance measured}
#'  \item{full_fit}{the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{the estimated variable importance}
#'  \item{naive}{the naive estimator of variable importance (only used if \code{type = "anova"})}
#'  \item{eif}{the estimated efficient influence function}
#'  \item{eif_full}{the estimated efficient influence function for the full regression}
#'  \item{eif_reduced}{the estimated efficient influence function for the reduced regression}
#'  \item{se}{the standard error for the estimated variable importance}
#'  \item{ci}{the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{test}{a decision to either reject (TRUE) or not reject (FALSE) the null hypothesis, based on a conservative test}
#'  \item{p_value}{a p-value based on the same test as \code{test}}
#'  \item{full_mod}{the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{the level, for confidence interval calculation}
#'  \item{sample_splitting_folds}{the folds used for sample-splitting (used for hypothesis testing)}
#'  \item{y}{the outcome}
#'  \item{ipc_weights}{the weights}
#'  \item{mat}{a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
#' }
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
#' # generate Y ~ Bernoulli (smooth)
#' y <- matrix(rbinom(n, size = 1, prob = smooth))
#'
#' # set up a library for SuperLearner; note simple library for speed
#' library("SuperLearner")
#' learners <- c("SL.glm")
#'
#' # using Y and X; use class-balanced folds
#' est_1 <- vim(y, x, indx = 2, type = "accuracy",
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, cvControl = list(V = 2),
#'            stratified = TRUE)
#'
#' # using pre-computed fitted values
#' set.seed(4747)
#' V <- 2
#' full_fit <- SuperLearner::SuperLearner(Y = y, X = x,
#'                                        SL.library = learners,
#'                                        cvControl = list(V = V))
#' full_fitted <- SuperLearner::predict.SuperLearner(full_fit)$pred
#' # fit the data with only X1
#' reduced_fit <- SuperLearner::SuperLearner(Y = full_fitted,
#'                                           X = x[, -2, drop = FALSE],
#'                                           SL.library = learners,
#'                                           cvControl = list(V = V))
#' reduced_fitted <- SuperLearner::predict.SuperLearner(reduced_fit)$pred
#'
#' est_2 <- vim(Y = y, f1 = full_fitted, f2 = reduced_fitted,
#'             indx = 2, run_regression = FALSE, alpha = 0.05,
#'             stratified = TRUE, type = "accuracy",
#'             sample_splitting_folds = est_1$sample_splitting_folds)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the
#'   \code{SuperLearner} function and package.
#' @export
vim <- function(Y = NULL, X = NULL, f1 = NULL, f2 = NULL, indx = 1,
                type = "r_squared", run_regression = TRUE,
                SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE,
                sample_splitting = TRUE, sample_splitting_folds = NULL, stratified = FALSE,
                C = rep(1, length(Y)), Z = NULL, ipc_weights = rep(1, length(Y)),
                ipc_est_type = "aipw", scale_est = TRUE, bootstrap = FALSE,
                b = 1000, ...) {
    # check to see if f1 and f2 are missing
    # if the data is missing, stop and throw an error
    check_inputs(Y, X, f1, f2, indx)

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

    # get the correct measure function; if not one of the supported ones, say so
    full_type <- get_full_type(type)

    if (is.null(sample_splitting_folds) | run_regression) {
        if (sample_splitting) {
            sample_splitting_folds <- make_folds(
                Y, V = 2, C = C, stratified = stratified
            )
        } else {
            sample_splitting_folds <- rep(1, length(Y))
        }
    }

    # if run_regression = TRUE, then fit SuperLearner
    if (run_regression) {
        arg_lst <- list(...)
        if (is.null(arg_lst$family)) {
            arg_lst$family <- switch(
                (length(unique(Y_cc)) == 2) + 1, stats::gaussian(),
                stats::binomial()
            )
        }
        X_cc <- subset(X, C == 1, drop = FALSE)
        sample_splitting_folds_cc <- sample_splitting_folds[C == 1]

        # set up the reduced X
        X_minus_s <- X_cc[, -indx, drop = FALSE]

        # fit the Super Learner given the specified library
        arg_lst_full <- c(arg_lst,
                          list(
                              Y = Y_cc,
                              X = X_cc,
                              SL.library = SL.library,
                              obsWeights = weights_cc
                          ))
        full <- do.call(SuperLearner::SuperLearner, arg_lst_full, quote = TRUE)

        # get the fitted values
        fhat_ful <- SuperLearner::predict.SuperLearner(full, onlySL = TRUE)$pred

        # fit the super learner on the reduced covariates:
        # if the reduced set of covariates is empty, return the mean
        # otherwise, if "r_squared" or "anova", regress the
        # fitted values on the remaining covariates
        arg_lst_redu <- arg_lst
        if (ncol(X_minus_s) == 0) {
            reduced <- NA
            fhat_red <- mean(Y_cc)
        } else {
            if (full_type == "r_squared" || full_type == "anova") {
                if (length(unique(fhat_ful)) == 1) {
                    arg_lst_redu$Y <- Y_cc
                } else {
                    arg_lst_redu$family <- stats::gaussian()
                    arg_lst_redu$Y <- fhat_ful
                }
            } else {
                arg_lst_redu$Y <- Y_cc
            }
            arg_lst_redu$X <- X_minus_s
            arg_lst_redu$SL.library <- SL.library
            arg_lst_redu$obsWeights <- weights_cc
            reduced <- do.call(SuperLearner::SuperLearner, arg_lst_redu, quote = TRUE)

            # get the fitted values
            fhat_red <- SuperLearner::predict.SuperLearner(reduced,
                                                           onlySL = TRUE)$pred
        }
    } else { # otherwise they are fitted values
        # check to make sure that the fitted values, folds are what we expect
        check_fitted_values(Y = Y, f1 = f1, f2 = f2,
                            sample_splitting_folds = sample_splitting_folds,
                            cv = FALSE)
        sample_splitting_folds_cc <- sample_splitting_folds[C == 1]

        # set up the fitted value objects
        fhat_ful <- switch((length(f1) == nrow(Y)) + 1, f1, subset(f1, C == 1))
        fhat_red <- switch((length(f2) == nrow(Y)) + 1, f2, subset(f2, C == 1))

        full <- reduced <- NA
    }
    # calculate the estimators, EIFs
    arg_lst <- list(...)
    if (!is.null(names(arg_lst)) && any(grepl("cvControl", names(arg_lst)))) {
        arg_lst$cvControl$stratifyCV <- FALSE
    }
    if (full_type == "anova") {
        # no sample-splitting, since no hypothesis testing
        est_lst <- measure_anova(
            full = fhat_ful, reduced = fhat_red,
            y = Y_cc, full_y = Y_cc,
            C = C, Z = Z_in,
            ipc_weights = ipc_weights,
            ipc_fit_type = "SL", na.rm = na.rm,
            SL.library = SL.library, arg_lst
        )
        est <- est_lst$point_est
        naive <- est_lst$naive
        eif <- est_lst$eif
        predictiveness_full <- NA
        predictiveness_redu <- NA
        eif_full <- rep(NA, length(Y))
        eif_redu <- rep(NA, length(Y))
        se_full <- NA
        se_redu <- NA
        if (bootstrap) {
            se <- bootstrap_se(Y = Y_cc, f1 = fhat_ful, f2 = fhat_red,
                               type = full_type, b = b)$se
        } else {
            se <- vimp_se(eif = eif, na.rm = na.rm)
        }
    } else {
        # only the point estimates need to be estimated on separate data splits
        # if no sample splitting, estimate on the whole data
        ss_folds_full <- switch((sample_splitting) + 1,
                                rep(1, length(sample_splitting_folds_cc)),
                                sample_splitting_folds_cc)
        ss_folds_redu <- switch((sample_splitting) + 1,
                                rep(2, length(sample_splitting_folds_cc)),
                                sample_splitting_folds_cc)
        predictiveness_full <- do.call(
            est_predictiveness,
            args = c(list(fitted_values = fhat_ful[ss_folds_full == 1],
                          y = Y_cc[ss_folds_full == 1, , drop = FALSE],
                          full_y = Y_cc,
                          type = full_type, C = C[sample_splitting_folds == 1],
                          Z = Z_in[sample_splitting_folds == 1, , drop = FALSE],
                          ipc_weights = ipc_weights[sample_splitting_folds == 1],
                          ipc_fit_type = "SL", scale = scale,
                          ipc_est_type = ipc_est_type, na.rm = na.rm,
                          SL.library = SL.library),
                     arg_lst)
        )$point_est
        eif_full <- do.call(
            est_predictiveness,
            args = c(list(fitted_values = fhat_ful,
                          y = Y_cc,
                          full_y = Y_cc,
                          type = full_type, C = C,
                          Z = Z_in,
                          ipc_weights = ipc_weights,
                          ipc_fit_type = "SL", scale = scale,
                          ipc_est_type = ipc_est_type, na.rm = na.rm,
                          SL.library = SL.library),
                     arg_lst)
        )$eif
        predictiveness_redu <- do.call(
            est_predictiveness,
            args = c(list(fitted_values = fhat_red[ss_folds_redu == 2],
                          y = Y_cc[ss_folds_redu == 2, , drop = FALSE],
                          full_y = Y_cc,
                          type = full_type, C = C[sample_splitting_folds == 2],
                          Z = Z_in[sample_splitting_folds == 2, , drop = FALSE],
                          ipc_weights = ipc_weights[sample_splitting_folds == 2],
                          ipc_fit_type = "SL", scale = scale,
                          ipc_est_type = ipc_est_type, na.rm = na.rm,
                          SL.library = SL.library),
                     arg_lst)
        )$point_est
        eif_redu <- do.call(
            est_predictiveness,
            args = c(list(fitted_values = fhat_red,
                          y = Y_cc,
                          full_y = Y_cc,
                          type = full_type, C = C,
                          Z = Z_in,
                          ipc_weights = ipc_weights,
                          ipc_fit_type = "SL", scale = scale,
                          ipc_est_type = ipc_est_type, na.rm = na.rm,
                          SL.library = SL.library),
                     arg_lst)
        )$eif
        se_full <- vimp_se(eif = eif_full, na.rm = na.rm)
        se_redu <- vimp_se(eif = eif_redu, na.rm = na.rm)
        est <- predictiveness_full - predictiveness_redu
        naive <- NA
        if (bootstrap & !sample_splitting) {
            ses <- bootstrap_se(Y = Y_cc, f1 = fhat_ful, f2 = fhat_red, type = full_type,
                                b = b)
            se <- ses$se
            se_full <- ses$se_full
            se_redu <- ses$se_reduced
        } else if (bootstrap) {
            warning(paste0("Bootstrap-based standard error estimates are currently",
                           " only available if sample_splitting = FALSE. Returning",
                           " standard error estimates based on the efficient",
                           " influence function instead."))
            se <- sqrt(se_full ^ 2 * length(eif_full) / sum(ss_folds_full == 1)
                       + se_redu ^ 2 * length(eif_redu) / sum(ss_folds_redu == 2))
        } else {
            se <- sqrt(se_full ^ 2 * length(eif_full) / sum(ss_folds_full == 1)
                       + se_redu ^ 2 * length(eif_redu) / sum(ss_folds_redu == 2))
        }
    }
    if (!bootstrap) {
        var_full <- se_full ^ 2 * length(eif_full)
        var_redu <- se_redu ^ 2 * length(eif_redu)
    } else {
        var_full <- se_full ^ 2
        var_redu <- se_redu ^ 2
    }

    # if est < 0, set to zero and print warning
    if (est < 0 && !is.na(est) & scale_est) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    } else if (is.na(est)) {
        warning("Original estimate NA; consider using a different library of learners.")
    }

    # compute the confidence intervals
    ci <- vimp_ci(est, se, scale = scale, level = 1 - alpha)
    predictiveness_ci_full <- vimp_ci(
        predictiveness_full, se = se_full, scale = scale, level = 1 - alpha
    )
    predictiveness_ci_redu <- vimp_ci(
        predictiveness_redu, se = se_redu, scale = scale, level = 1 - alpha
    )

    # perform a hypothesis test against the null of zero importance
    if (full_type == "anova" || full_type == "regression") {
        hyp_test <- list(test = NA, p_value = NA, test_statistics = NA)
    } else {
        hyp_test <- vimp_hypothesis_test(
            predictiveness_full = predictiveness_full,
            predictiveness_reduced = predictiveness_redu,
            se_full = sqrt(var_full / sum(ss_folds_full == 1)),
            se_reduced = sqrt(var_redu / sum(ss_folds_redu == 2)),
            delta = delta, alpha = alpha
        )
    }
    # create the output and return it (as a tibble)
    chr_indx <- paste(as.character(indx), collapse = ",")
    mat <- tibble::tibble(
        s = chr_indx, est = est, se = se, cil = ci[1], ciu = ci[2],
        test = hyp_test$test, p_value = hyp_test$p_value
    )
    output <- list(s = chr_indx,
                 SL.library = SL.library,
                 full_fit = fhat_ful, red_fit = fhat_red,
                 est = est,
                 naive = naive,
                 eif = eif_full - eif_redu,
                 eif_full = eif_full,
                 eif_reduced = eif_redu,
                 se = se, ci = ci,
                 predictiveness_full = predictiveness_full,
                 predictiveness_reduced = predictiveness_redu,
                 predictiveness_ci_full = predictiveness_ci_full,
                 predictiveness_ci_reduced = predictiveness_ci_redu,
                 test = hyp_test$test,
                 p_value = hyp_test$p_value,
                 test_statistic = hyp_test$test_statistic,
                 full_mod = full,
                 red_mod = reduced,
                 alpha = alpha,
                 delta = delta,
                 y = Y,
                 sample_splitting_folds = sample_splitting_folds,
                 ipc_weights = ipc_weights,
                 scale = scale,
                 mat = mat)

    # make it also a vim and vim_type object
    tmp.cls <- class(output)
    class(output) <- c("vim", full_type, tmp.cls)
    return(output)
}
