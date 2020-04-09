#' Nonparametric Variable Importance Estimates
#'
#' Compute estimates of and confidence intervals for nonparametric risk-based variable importance.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the fitted values from a flexible estimation technique regressing Y on X.
#' @param f2 the fitted values from a flexible estimation technique regressing Y on X withholding the columns in \code{indx}.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type the type of importance to compute; defaults to \code{r_squared}, but other supported options are \code{auc}, \code{accuracy}, and \code{anova}.
#' @param run_regression if outcome Y and covariates X are passed to \code{vimp_accuracy}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or logit ("logit") scale?
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param folds the folds used for \code{f1} and \code{f2}; assumed to be 1 for the observations used in \code{f1} and 2 for the observations used in \code{f2}. If there is only a single fold passed in, then hypothesis testing is not done.
#' @param stratified if run_regression = TRUE, then should the generated folds be stratified based on the outcome (helps to ensure class balance across cross-validation folds)
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of classes \code{vim} and the type of risk-based measure. See Details for more information.
#'
#' @details In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{vim}}
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{type}{ - the type of risk-based variable importance measured}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance}
#'  \item{update}{ - the influence curve-based update}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{test}{ - a decision to either reject (TRUE) or not reject (FALSE) the null hypothesis, based on a conservative test}
#'  \item{pval}{ - a conservative p-value based on the same conservative test as \code{test}}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{folds}{ - the folds used for hypothesis testing}
#'  \item{y}{ - the outcome}
#'  \item{weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
#' }
#'
#' @examples
#' library(SuperLearner)
#' library(ranger)
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
#' learners <- "SL.ranger"
#'
#' ## using Y and X; use class-balanced folds
#' folds_1 <- sample(rep(seq_len(2), length = sum(y == 1)))
#' folds_0 <- sample(rep(seq_len(2), length = sum(y == 0)))
#' folds <- vector("numeric", length(y))
#' folds[y == 1] <- folds_1
#' folds[y == 0] <- folds_0
#' est <- vim(y, x, indx = 2, type = "r_squared",
#'            alpha = 0.05, run_regression = TRUE,
#'            SL.library = learners, cvControl = list(V = 5),
#'            folds = folds)
#'
#' ## using pre-computed fitted values
#' full <- SuperLearner(Y = y[folds == 1], X = x[folds == 1, ],
#' SL.library = learners, cvControl = list(V = 5))
#' full.fit <- predict(full)$pred
#' reduced <- SuperLearner(Y = y[folds == 2], X = x[folds == 2, -2, drop = FALSE],
#' SL.library = learners, cvControl = list(V = 5))
#' red.fit <- predict(reduced)$pred
#'
#' est <- vim(Y = y, f1 = full.fit, f2 = red.fit,
#'             indx = 2, run_regression = FALSE, alpha = 0.05, folds = folds,
#'             type = "accuracy")
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export


vim <- function(Y, X, f1 = NULL, f2 = NULL, indx = 1, weights = rep(1, length(Y)), type = "r_squared", run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE, folds = NULL, stratified = FALSE, ...) {
    ## check to see if f1 and f2 are missing
    ## if the data is missing, stop and throw an error
    if (missing(f1) & missing(Y)) stop("You must enter either Y or fitted values for the full regression.")
    if (missing(f2) & missing(Y)) stop("You must enter either Y or fitted values for the reduced regression.")
    ## if indx is outside the range of X, stop and throw an error
    if (!missing(X)) {
        if (any(indx > dim(X)[2])) stop("One of the feature indices in 'indx' is larger than the total number of features in X. Please specify a new index subgroup in 'indx'.")
    }
    
    ## check to see if Y is a matrix or data.frame; if not, make it one (just for ease of reading)
    if(is.null(dim(Y))) Y <- as.matrix(Y)

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")

    ## if run_regression = TRUE, then fit SuperLearner
    if (run_regression) {
        ## create folds for cross-fitting
        .make_folds <- function(y, V, stratified = FALSE) {
            folds <- vector("numeric", length(y))
            if (stratified) {
                folds_1 <- rep(seq_len(V), length = sum(y == 1))
                folds_0 <- rep(seq_len(V), length = sum(y == 0))
                folds_1 <- sample(folds_1)
                folds_0 <- sample(folds_0)
                folds[y == 1] <- folds_1
                folds[y == 0] <- folds_0
            } else {
                folds <- rep(seq_len(V), length = length(y))
                folds <- sample(folds)
            }
            return(folds)
        }
        if (is.null(folds)) {
            folds <- .make_folds(Y, V = 2, stratified = stratified)
        }
        ## if formula is entered, need a library for Super Learner
        if (is.null(SL.library)) stop("You must enter a library of learners for the Super Learner.")

        ## set up the reduced X
        X_minus_s <- X[, -indx, drop = FALSE]

        ## fit the Super Learner given the specified library
        full <- SuperLearner::SuperLearner(Y = Y[folds == 1, , drop = FALSE], X = X[folds == 1, ], SL.library = SL.library, ...)

        ## get the fitted values
        fhat_ful <- SuperLearner::predict.SuperLearner(full)$pred

        ## fit the super learner on the reduced covariates:
        ## if the reduced set of covariates is empty, return the mean
        ## otherwise, if "r_squared" or "anova", regress the fitted values on the remaining covariates
        if (ncol(X_minus_s) == 0) {
            reduced <- NA
            fhat_red <- mean(Y[folds == 2, , drop = FALSE])
        } else {
            arg_lst <- list(...)
            if (full_type == "r_squared" | full_type == "anova") {
                if (length(unique(fhat_ful)) == 1) {
                    arg_lst$Y <- Y[folds == 2, , drop = FALSE]
                } else {
                    arg_lst$family <- stats::gaussian()
                    arg_lst$Y <- fhat_ful
                }
                arg_lst$X <- X_minus_s[folds == 2, , drop = FALSE]
                arg_lst$SL.library <- SL.library
            }
            reduced <- do.call(SuperLearner::SuperLearner, arg_lst)
            
            ## get the fitted values
            fhat_red <- SuperLearner::predict.SuperLearner(reduced)$pred
        }
    } else { ## otherwise they are fitted values

        ## check to make sure they are the same length as y
        if (is.null(Y)) stop("Y must be entered.")
        if (length(f1) != length(Y[folds == 1])) stop("Fitted values from the full regression must be the same length as the number of observations in the first fold.")
        if (length(f2) != length(Y[folds == 2])) stop("Fitted values from the reduced regression must be the same length as the number of observations in the second fold.")
        ## if full_type is "anova", don't do a hypothesis test
        if (full_type == "anova" ) warning("Hypothesis testing is not available for type = 'anova'. If you want an R-squared-based hypothesis test, please enter type = 'r_squared'.")

        ## set up the fitted value objects
        fhat_ful <- f1
        fhat_red <- f2

        full <- reduced <- NA
    }

    ## calculate the estimators
    ests <- vimp_point_est(fhat_ful, fhat_red, Y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)

    ## if type = "anova", then use corrected; else use plug-in
    if (full_type == "anova" | full_type == "regression") {
        est <- ests[1]
        naive <- ests[2]
        predictiveness_full <- NA
        predictiveness_redu <- NA
    } else {
        est <- ests[2]
        naive <- NA
        predictiveness_full <- predictiveness_point_est(fitted_values = fhat_ful, y = Y[folds == 1, , drop = FALSE], weights = weights[folds == 1], type = full_type, na.rm = na.rm)
        predictiveness_redu <- predictiveness_point_est(fitted_values = fhat_red, y = Y[folds == 2, , drop = FALSE], weights = weights[folds == 2], type = full_type, na.rm = na.rm)
    }
    ## compute the update
    update <- vimp_update(fhat_ful, fhat_red, Y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)

    ## compute the standard error
    se <- vimp_se(est, update, scale = scale, na.rm = na.rm)

    ## if est < 0, set to zero and print warning
    if (est < 0) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    }

    ## compute the confidence interval
    ci <- vimp_ci(est, se, scale = scale, level = 1 - alpha)
    predictiveness_ci_full <- vimp_ci(predictiveness_full, se = vimp_se(predictiveness_full, predictiveness_update(fhat_ful, Y[folds == 1, , drop = FALSE], type = full_type, na.rm = na.rm), scale = scale), scale = scale, level = 1 - alpha)
    predictiveness_ci_redu <- vimp_ci(predictiveness_redu, se = vimp_se(predictiveness_redu, predictiveness_update(fhat_red, Y[folds == 2, , drop = FALSE], type = full_type, na.rm = na.rm), scale = scale), scale = scale, level = 1 - alpha)

    ## perform a hypothesis test against the null of zero importance
    if (full_type != "anova") {
        hyp_test <- vimp_hypothesis_test(fhat_ful, fhat_red, Y, folds, delta = delta, weights = weights, type = full_type, alpha = alpha, scale = scale, na.rm = na.rm)
    } else {
        hyp_test <- list(test = NA, p_value = NA, predictiveness_full = NA, predictiveness_reduced = NA, predictiveness_ci_full = rep(NA, 2), predictiveness_ci_reduced = rep(NA, 2), se_full = NA, se_redu = NA, test_statistic = NA)
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
                 y = Y,
                 folds = folds,
                 weights = weights,
                 scale = scale,
                 mat = mat)

    ## make it also an vim and vim_type object
    tmp.cls <- class(output)
    class(output) <- c("vim", full_type, tmp.cls)
    return(output)
}
