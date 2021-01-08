#' Nonparametric Variable Importance Estimates and Inference
#'
#' Compute estimates of and confidence intervals for nonparametric variable importance based on the population-level contrast between the oracle predictiveness using the feature(s) of interest versus not.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the fitted values from a flexible estimation technique regressing Y on X.
#' @param f2 the fitted values from a flexible estimation technique regressing Y on X withholding the columns in \code{indx}.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param type the type of importance to compute; defaults to \code{r_squared}, but other supported options are \code{auc}, \code{accuracy}, \code{deviance}, and \code{anova}.
#' @param run_regression if outcome Y and covariates X are passed to \code{vimp_accuracy}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or logit ("logit") scale?
#' @param na.rm should we remove NAs in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param folds the folds used for \code{f1} and \code{f2}; assumed to be 1 for the observations used in \code{f1} and 2 for the observations used in \code{f2}. If there is only a single fold passed in, then hypothesis testing is not done.
#' @param stratified if run_regression = TRUE, then should the generated folds be stratified based on the outcome (helps to ensure class balance across cross-validation folds)
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param Z either (i) NULL (the default, in which case the argument \code{C} above must be all ones), or (ii) a character vector specifying the variable(s) among Y and X that are thought to play a role in the coarsening mechanism.
#' @param ipc_weights weights for the computed influence curve (i.e., inverse probability weights for coarsened-at-random settings). Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of classes \code{vim} and the type of risk-based measure. See Details for more information.
#'
#' @details We define the population variable importance measure (VIM) for the group
#' of features (or single feature) \eqn{s} with respect to the predictiveness measure
#' \eqn{V} by \deqn{\psi_{0,s} := V(f_0, P_0) - V(f_{0,s}, P_0),} where \eqn{f_0} is
#' the population predictiveness maximizing function, \eqn{f_{0,s}} is the population
#' predictiveness maximizing function that is only allowed to access the features with
#' index not in \eqn{s}, and \eqn{P_0} is the true data-generating distribution. VIM estimates are
#' obtained by obtaining estimators \eqn{f_n} and \eqn{f_{n,s}} of \eqn{f_0} and \eqn{f_{0,s}},
#' respectively; obtaining an estimator \eqn{P_n} of \eqn{P_0}; and finally, setting \eqn{\psi_{n,s} := V(f_n, P_n) - V(f_{n,s}, P_n)}.
#'
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list including:
#' \itemize{
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{type}{ - the type of risk-based variable importance measured}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance (only used if \code{type = "anova"})}
#'  \item{eif}{ - the estimated influence curve}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{test}{ - a decision to either reject (TRUE) or not reject (FALSE) the null hypothesis, based on a conservative test}
#'  \item{p_value}{ - a p-value based on the same test as \code{test}}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{folds}{ - the folds used for hypothesis testing}
#'  \item{y}{ - the outcome}
#'  \item{ipc_weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
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
#' # generate Y ~ Normal (smooth, 1)
#' y <- matrix(rbinom(n, size = 1, prob = smooth))
#'
#' # set up a library for SuperLearner; note simple library for speed
#' library("SuperLearner")
#' learners <- c("SL.glm", "SL.mean")
#'
#' # using Y and X; use class-balanced folds
#' folds_1 <- sample(rep(seq_len(2), length = sum(y == 1)))
#' folds_0 <- sample(rep(seq_len(2), length = sum(y == 0)))
#' folds <- vector("numeric", length(y))
#' folds[y == 1] <- folds_1
#' folds[y == 0] <- folds_0
#' est <- vim(y, x, indx = 2, type = "r_squared",
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, cvControl = list(V = 2),
#'            folds = folds)
#'
#' # using pre-computed fitted values
#' full <- SuperLearner::SuperLearner(Y = y[folds == 1], X = x[folds == 1, ],
#' SL.library = learners, cvControl = list(V = 2))
#' full.fit <- SuperLearner::predict.SuperLearner(full)$pred
#' reduced <- SuperLearner::SuperLearner(Y = y[folds == 2], 
#' X = x[folds == 2, -2, drop = FALSE],
#' SL.library = learners, cvControl = list(V = 2))
#' red.fit <- SuperLearner::predict.SuperLearner(reduced)$pred
#'
#' est <- vim(Y = y, f1 = full.fit, f2 = red.fit,
#'             indx = 2, run_regression = FALSE, alpha = 0.05, folds = folds,
#'             type = "accuracy")
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
vim <- function(Y = NULL, X = NULL, f1 = NULL, f2 = NULL, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE, folds = NULL, stratified = FALSE, C = rep(1, length(Y)), Z = NULL, ipc_weights = rep(1, length(Y)), ...) {
    # check to see if f1 and f2 are missing
    # if the data is missing, stop and throw an error
    check_inputs(Y, X, f1, f2, indx)

    # check to see if Y is a matrix or data.frame; if not, make it one (just for ease of reading)
    if(is.null(dim(Y))) Y <- as.matrix(Y)

    # set up internal data -- based on complete cases only
    Y_cc <- subset(Y, C == 1, drop = FALSE)
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

    # if run_regression = TRUE, then fit SuperLearner
    if (run_regression) {
        X_cc <- subset(X, C == 1, drop = FALSE)
        if (is.null(folds)) {
            folds <- .make_folds(Y, V = 2, stratified = stratified)
        }
        folds_cc <- folds[C == 1]

        # set up the reduced X
        X_minus_s <- X_cc[, -indx, drop = FALSE]

        # fit the Super Learner given the specified library
        full <- SuperLearner::SuperLearner(Y = Y_cc[(folds_cc == 1), , drop = FALSE], X = X_cc[(folds_cc == 1), , drop = FALSE], SL.library = SL.library, obsWeights = weights_cc[(folds_cc == 1)], ...)

        # get the fitted values
        fhat_ful <- SuperLearner::predict.SuperLearner(full)$pred

        # fit the super learner on the reduced covariates:
        # if the reduced set of covariates is empty, return the mean
        # otherwise, if "r_squared" or "anova", regress the fitted values on the remaining covariates
        if (ncol(X_minus_s) == 0) {
            reduced <- NA
            fhat_red <- mean(Y_cc[(folds_cc == 2), , drop = FALSE])
        } else {
            arg_lst <- list(...)
            if (full_type == "r_squared" || full_type == "anova") {
                if (length(unique(fhat_ful)) == 1) {
                    arg_lst$Y <- Y_cc[(folds_cc == 2), , drop = FALSE]
                } else {
                    arg_lst$family <- stats::gaussian()
                    arg_lst$Y <- SuperLearner::predict.SuperLearner(full, newdata = X_cc[(folds_cc == 2), , drop = FALSE], onlySL = TRUE)$pred
                }
                arg_lst$X <- X_minus_s[(folds_cc == 2), , drop = FALSE]
                arg_lst$SL.library <- SL.library
                arg_lst$obsWeights <- weights_cc[(folds_cc == 2)]
            }
            reduced <- do.call(SuperLearner::SuperLearner, arg_lst)

            # get the fitted values
            fhat_red <- SuperLearner::predict.SuperLearner(reduced, onlySL = TRUE)$pred
        }
    } else { # otherwise they are fitted values
        # check to make sure that the fitted values, folds are what we expect
        check_fitted_values(Y, f1, f2, folds, cv = FALSE)
        folds_cc <- folds[C == 1]

        # set up the fitted value objects
        fhat_ful <- switch((length(f1) == nrow(Y)) + 1, f1, subset(f1, C == 1))
        fhat_red <- switch((length(f2) == nrow(Y)) + 1, f2, subset(f2, C == 1))

        full <- reduced <- NA
    }
    # calculate the estimators, EIFs
    if (full_type == "anova" || full_type == "regression") {
        est_lst <- measure_anova(full = fhat_ful, reduced = fhat_red, y = Y_cc[folds_cc == 1, , drop = FALSE], C = C[folds == 1], Z = Z_in, ipc_weights = ipc_weights[folds == 1], ipc_fit_type = "SL", na.rm = na.rm, SL.library = SL.library, ...)
        est <- est_lst$point_est
        naive <- est_lst$naive
        eif <- est_lst$eif
        predictiveness_full <- NA
        predictiveness_redu <- NA
        eif_full <- rep(NA, length(Y))
        eif_redu <- rep(NA, length(Y))
        se_full <- NA
        se_redu <- NA
    } else {
        est_lst_full <- est_predictiveness(fitted_values = fhat_ful, y = Y_cc[folds_cc == 1, , drop = FALSE], type = full_type, C = C[folds == 1], Z = Z_in[folds == 1, , drop = FALSE], ipc_weights = ipc_weights[folds == 1], ipc_fit_type = "SL", scale = scale, na.rm = na.rm, SL.library = SL.library, ...)
        est_lst_redu <- est_predictiveness(fitted_values = fhat_red, y = Y_cc[folds_cc == 2, , drop = FALSE], type = full_type, C = C[folds == 2], Z = Z_in[folds == 2, , drop = FALSE], ipc_weights = ipc_weights[folds == 2], ipc_fit_type = "SL", scale = scale, na.rm = na.rm, SL.library = SL.library, ...)
        predictiveness_full <- est_lst_full$point_est
        predictiveness_redu <- est_lst_redu$point_est
        eif_full <- est_lst_full$eif
        se_full <- vimp_se(predictiveness_full, eif_full, na.rm = na.rm)
        eif_redu <- est_lst_redu$eif
        se_redu <- vimp_se(predictiveness_redu, eif_redu, na.rm = na.rm)
        est <- est_lst_full$point_est - est_lst_redu$point_est
        naive <- NA
        len_full <- length(eif_full)
        len_redu <- length(eif_redu)
        if (len_full != len_redu) {
            max_length <- max(len_full, len_redu)
            tmp_eif_full <- c(eif_full, rep(0, max_length - len_full))
            tmp_eif_redu <- c(eif_redu, rep(0, max_length - len_redu))
        } else {
            tmp_eif_full <- eif_full
            tmp_eif_redu <- eif_redu
        }
        eif <- tmp_eif_full - tmp_eif_redu
    }
    # compute the standard error
    se <- vimp_se(est, eif, na.rm = na.rm)

    # if est < 0, set to zero and print warning
    if (est < 0 && !is.na(est)) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    } else if (is.na(est)) {
        warning("Original estimate NA; consider using a different library of learners.")
    }

    # compute the confidence intervals
    ci <- vimp_ci(est, se, scale = scale, level = 1 - alpha)
    predictiveness_ci_full <- vimp_ci(predictiveness_full, se = se_full, scale = scale, level = 1 - alpha)
    predictiveness_ci_redu <- vimp_ci(predictiveness_redu, se = se_redu, scale = scale, level = 1 - alpha)

    # perform a hypothesis test against the null of zero importance
    if (full_type == "anova" || full_type == "regression") {
        hyp_test <- list(test = NA, p_value = NA, test_statistics = NA)
    } else {
        hyp_test <- vimp_hypothesis_test(predictiveness_full = predictiveness_full, predictiveness_reduced = predictiveness_redu, se_full = se_full, se_reduced = se_redu, delta = delta, alpha = alpha)
    }
    # create the output and return it (as a tibble)
    chr_indx <- paste(as.character(indx), collapse = ",")
    mat <- tibble::tibble(s = chr_indx, est = est, se = se[1], cil = ci[1], ciu = ci[2], test = hyp_test$test, p_value = hyp_test$p_value)
    output <- list(s = chr_indx,
                 SL.library = SL.library,
                 full_fit = fhat_ful, red_fit = fhat_red,
                 est = est,
                 naive = naive,
                 eif = eif,
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
                 folds = folds,
                 ipc_weights = ipc_weights,
                 scale = scale,
                 mat = mat)

    # make it also an vim and vim_type object
    tmp.cls <- class(output)
    class(output) <- c("vim", full_type, tmp.cls)
    return(output)
}
