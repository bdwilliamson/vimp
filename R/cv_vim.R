#' Nonparametric Variable Importance Estimates using Cross-validation
#'
#' Compute estimates and confidence intervals for the
#' nonparametric variable importance parameter of interest, using cross-validation.
#' This essentially involves splitting the data into V train/test splits; train the learners on the training data, evaluate importance on the test data; and average over these splits.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the predicted values on validation data from a flexible estimation technique regressing Y on X in the training data; a list of length V, where each object is a set of predictions on the validation data.
#' @param f2 the predicted values on validation data from a flexible estimation technique regressing the fitted values in \code{f1} on X withholding the columns in \code{indx}; a list of length V, where each object is a set of predictions on the validation data.
#' @param indx the indices of the covariate(s) to calculate variable importance for; defaults to 1.
#' @param V the number of folds for cross-validation, defaults to 10.
#' @param folds the folds to use, if f1 and f2 are supplied. A list of length two; the first element provides the outer folds (for hypothesis testing), while the second element is a list providing the inner folds (for cross-validation).
#' @param stratified if run_regression = TRUE, then should the generated folds be stratified based on the outcome (helps to ensure class balance across cross-validation folds)
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type the type of parameter (e.g., ANOVA-based is \code{"anova"}).
#' @param run_regression if outcome Y and covariates X are passed to \code{cv_vim}, and \code{run_regression} is \code{TRUE}, then Super Learner will be used; otherwise, variable importance will be computed using the inputted fitted values.
#' @param SL.library a character vector of learners to pass to \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or logit ("logit") scale?
#' @param na.rm should we remove NA's in the outcome and fitted values in computation? (defaults to \code{FALSE})
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function, and the validity
#' of the confidence intervals.
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{cv_vim}}
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data (a list, for train and test data)}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data (a list, for train and test data)}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance}
#'  \item{naives}{ - the naive estimator on each fold}
#'  \item{updates}{ - the influence curve-based update for each fold}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{folds}{ - the folds used for hypothesis testing and cross-validation}
#'  \item{y}{ - the outcome}
#'  \item{weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
#' }
#'
#' @examples
#' \donttest{
#' ## don't test because this can take a long time to run
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
#' ## using Super Learner (with a small number of folds, for illustration only)
#' ## -----------------------------------------
#' set.seed(4747)
#' est <- cv_vim(Y = y, X = x, indx = 2, V = 2,
#' type = "r_squared", run_regression = TRUE,
#' SL.library = learners, cvControl = list(V = 5), alpha = 0.05)
#'
#' ## ------------------------------------------
#' ## doing things by hand, and plugging them in (with a small number of folds, for illustration only)
#' ## ------------------------------------------
#' ## set up the folds
#' indx <- 2
#' V <- 2
#' set.seed(4747)
#' folds <- rep(seq_len(V), length = n)
#' folds <- sample(folds)
#' ## get the fitted values by fitting the super learner on each pair
#' fhat_ful <- list()
#' fhat_red <- list()
#' for (v in 1:V) {
#'     ## fit super learner
#'     fit <- SuperLearner::SuperLearner(Y = y[folds != v, , drop = FALSE],
#'      X = x[folds != v, , drop = FALSE], SL.library = learners, cvControl = list(V = 5))
#'     fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
#'     ## get predictions on the validation fold
#'     fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit,
#'      newdata = x[folds == v, , drop = FALSE])$pred
#'     ## fit the super learner on the reduced covariates
#'     red <- SuperLearner::SuperLearner(Y = fitted_v,
#'      X = x[folds != v, -indx, drop = FALSE], SL.library = learners, cvControl = list(V = 5))
#'     ## get predictions on the validation fold
#'     fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red,
#'      newdata = x[folds == v, -indx, drop = FALSE])$pred
#' }
#' est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
#' V = V, folds = folds, type = "r_squared", run_regression = FALSE, alpha = 0.05)
#' }
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
cv_vim <- function(Y, X, f1, f2, indx = 1, V = length(unique(folds)), folds = NULL, stratified = FALSE, weights = rep(1, length(Y)), type = "r_squared", run_regression = TRUE, SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE, ...) {
    ## check to see if f1 and f2 are missing
    ## if the data is missing, stop and throw an error
    if (missing(f1) & missing(Y)) stop("You must enter either Y or fitted values for the full regression.")
    if (missing(f2) & missing(X)) stop("You must enter either X or fitted values for the reduced regression.")
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


    ## if we need to run the regression, fit Super Learner with the given library
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
        ## set up the cross-validation
        outer_folds <- .make_folds(Y, V = 2, stratified = stratified)
        inner_folds_1 <- .make_folds(Y[outer_folds == 1, , drop = FALSE], V = V, stratified = stratified)
        inner_folds_2 <- .make_folds(Y[outer_folds == 2, , drop = FALSE], V = V, stratified = stratified)
        ## fit the super learner on each full/reduced pair
        fhat_ful <- list()
        fhat_red <- list()
        for (v in 1:V) {
            ## fit super learner
            fit <- SuperLearner::SuperLearner(Y = Y[outer_folds == 1, , drop = FALSE][inner_folds_1 != v, , drop = FALSE], X = X[outer_folds == 1, , drop = FALSE][inner_folds_1 != v, , drop = FALSE], SL.library = SL.library, ...)
            fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
            ## get predictions on the validation fold
            fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, newdata = X[outer_folds == 1, , drop = FALSE][inner_folds_1 == v, , drop = FALSE])$pred
            ## fit the super learner on the reduced covariates:
            ## if the reduced set of covariates is empty, return the mean
            ## otherwise, if type is r_squared or anova, always use gaussian; if first regression was mean, use Y instead
            if (ncol(X[outer_folds == 2, , drop = FALSE][, -indx, drop = FALSE]) == 0) {
                fhat_red[[v]] <- mean(Y[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE])
            } else {
                arg_lst <- list(...)
                if (length(unique(fitted_v)) == 1) {
                    arg_lst$Y <- Y[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE]
                } else if (type == "r_squared" | type == "anova") {
                    arg_lst$family <- stats::gaussian()
                    fit_2 <- SuperLearner::SuperLearner(Y = Y[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE], X = X[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE], SL.library = SL.library, ...)
                    arg_lst$Y <- SuperLearner::predict.SuperLearner(fit_2)$pred
                } else {
                    arg_lst$Y <- Y[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE]
                    # get the family
                    if (is.character(arg_lst$family))
                        arg_lst$family <- get(arg_lst$family, mode = "function", envir = parent.frame())
                }
                arg_lst$X <- X[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, -indx, drop = FALSE]
                arg_lst$SL.library <- SL.library
                red <- do.call(SuperLearner::SuperLearner, arg_lst)
                ## get predictions on the validation fold
                fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red, newdata = X[outer_folds == 2, , drop = FALSE][inner_folds_2 == v, -indx, drop = FALSE])$pred
            }
        }
        full <- reduced <- NA
        folds <- list(outer_folds = outer_folds, inner_folds = list(inner_folds_1, inner_folds_2))

    } else { ## otherwise they are fitted values

        ## check to make sure they are the same length as y
        if (is.null(Y)) stop("Y must be entered.")
        if (is.null(f1)) stop("You must specify a list of predicted values from a regression of Y on X.")
        if (is.null(f2)) stop("You must specify a list of predicted values from a regression of the fitted values from the Y on X regression on the reduced set of covariates.")
        if (is.null(folds)) stop("You must specify a list of folds.")
        if (length(f1) != V) stop("The number of folds from the full regression must be the same length as the number of folds.")
        if (length(f2) != V) stop("The number of folds from the reduced regression must be the same length as the number of folds.")
        if (full_type == "anova") warning("Hypothesis testing is not available for ANOVA-based variable importance.")
        ## set up the fitted value objects (both are lists!)
        fhat_ful <- f1
        fhat_red <- f2
        ## set up the folds objects
        outer_folds <- folds[[1]]
        inner_folds_1 <- folds[[2]][[1]]
        inner_folds_2 <- folds[[2]][[2]]

        full <- reduced <- NA

    }
    ## get point estimate based on cv
    ests_lst <- cv_vimp_point_est(fhat_ful, fhat_red, Y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)
        ests <- ests_lst$point_est
    all_ests <- ests_lst$all_ests

    ## if type = "anova", then use corrected; else use plug-in
    if (full_type == "anova" | full_type == "regression") {
        est <- ests[1]
        naive <- ests[2]
        predictiveness_full <- NA
        predictiveness_redu <- NA
    } else { # compute all point ests, ics for hypothesis test
        est <- ests[2]
        naive <- NA
        predictiveness_full <- cv_predictiveness_point_est(fitted_values = fhat_ful, y = Y[outer_folds == 1, , drop = FALSE], folds = inner_folds_1, weights = weights[outer_folds == 1], type = full_type, na.rm = na.rm)$point_est
        predictiveness_redu <- cv_predictiveness_point_est(fitted_values = fhat_red, y = Y[outer_folds == 2, , drop = FALSE], folds = inner_folds_2, weights = weights[outer_folds == 2], type = full_type, na.rm = na.rm)$point_est
    }

    ## compute the update
    update_lst <- cv_vimp_update(fhat_ful, fhat_red, Y, folds = folds, weights = weights, type = full_type, na.rm = na.rm)
    update <- update_lst$ic
    updates <- update_lst$all_ics

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
