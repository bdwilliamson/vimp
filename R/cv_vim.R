#' Nonparametric Intrinsic Variable Importance Estimates and Inference using Cross-fitting
#'
#' Compute estimates and confidence intervals using cross-fitting for 
#' nonparametric intrinsic variable importance based on the 
#' population-level contrast between the oracle predictiveness using the 
#' feature(s) of interest versus not.
#'
#' @param Y the outcome.
#' @param X the covariates.
#' @param f1 the predicted values on validation data from a flexible estimation 
#'   technique regressing Y on X in the training data; a list of length V, where
#'   each object is a set of predictions on the validation data.
#' @param f2 the predicted values on validation data from a flexible estimation 
#'   technique regressing the fitted values in \code{f1} on X withholding the 
#'   columns in \code{indx}; a list of length V, where each object is a set of 
#'   predictions on the validation data.
#' @param indx the indices of the covariate(s) to calculate variable importance 
#'   for; defaults to 1.
#' @param V the number of folds for cross-fitting, defaults to 10.
#' @param folds the folds to use, if f1 and f2 are supplied. A list of length 
#'   two; the first element provides the outer folds (for hypothesis testing), 
#'   while the second element is a list providing the inner folds 
#'   (for cross-fitting).
#' @param stratified if run_regression = TRUE, then should the generated folds 
#'   be stratified based on the outcome (helps to ensure class balance across 
#'   cross-fitting folds)
#' @param type the type of parameter (e.g., ANOVA-based is \code{"anova"}).
#' @param run_regression if outcome Y and covariates X are passed to 
#'   \code{cv_vim}, and \code{run_regression} is \code{TRUE}, then Super Learner 
#'   will be used; otherwise, variable importance will be computed using the 
#'   inputted fitted values.
#' @param SL.library a character vector of learners to pass to 
#'   \code{SuperLearner}, if \code{f1} and \code{f2} are Y and X, respectively. 
#'   Defaults to \code{SL.glmnet}, \code{SL.xgboost}, and \code{SL.mean}.
#' @param alpha the level to compute the confidence interval at. Defaults to 
#'   0.05, corresponding to a 95\% confidence interval.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if 
#'   importance < \eqn{\delta}); defaults to 0.
#' @param scale should CIs be computed on original ("identity") or 
#'   logit ("logit") scale?
#' @param na.rm should we remove NA's in the outcome and fitted values in 
#'   computation? (defaults to \code{FALSE})
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
#' @param ... other arguments to the estimation tool, see "See also".
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details We define the population variable importance measure (VIM) for the group
#' of features (or single feature) \eqn{s} with respect to the predictiveness measure
#' \eqn{V} by \deqn{\psi_{0,s} := V(f_0, P_0) - V(f_{0,s}, P_0),} where \eqn{f_0} is
#' the population predictiveness maximizing function, \eqn{f_{0,s}} is the population
#' predictiveness maximizing function that is only allowed to access the features with
#' index not in \eqn{s}, and \eqn{P_0} is the true data-generating distribution. Cross-fitted
#' VIM estimates are obtained by first splitting the data into \eqn{K} folds; then using each
#' fold in turn as a hold-out set, constructing estimators \eqn{f_{n,k}} and \eqn{f_{n,k,s}} of
#' \eqn{f_0} and \eqn{f_{0,s}}, respectively on the training data and estimator \eqn{P_{n,k}} of
#' \eqn{P_0} using the test data; and finally, computing \deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K \{V(f_{n,k},P_{n,k}) - V(f_{n,k,s}, P_{n,k})\}}
#' See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind the \code{cv_vim} function, and the validity
#' of the confidence intervals.
#'
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list including:
#' \itemize{
#'  \item{s}{ - the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - the library of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - the fitted values of the chosen method fit to the full data (a list, for train and test data)}
#'  \item{red_fit}{ - the fitted values of the chosen method fit to the reduced data (a list, for train and test data)}
#'  \item{est}{ - the estimated variable importance}
#'  \item{naive}{ - the naive estimator of variable importance}
#'  \item{naives}{ - the naive estimator on each fold}
#'  \item{eif}{- the estimated influence function}
#'  \item{all_eifs}{ - the estimated influence curve for each fold}
#'  \item{se}{ - the standard error for the estimated variable importance}
#'  \item{ci}{ - the \eqn{(1-\alpha) \times 100}\% confidence interval for the variable importance estimate}
#'  \item{test}{ - a decision to either reject (TRUE) or not reject (FALSE) the null hypothesis, based on a conservative test}
#'  \item{p_value}{ - a p-value based on the same test as \code{test}}
#'  \item{full_mod}{ - the object returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - the object returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#'  \item{folds}{ - the folds used for hypothesis testing and cross-fitting}
#'  \item{y}{ - the outcome}
#'  \item{ipc_weights}{ - the weights}
#'  \item{mat}{- a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
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
#' # using Super Learner (with a small number of folds, for illustration only)
#' # -----------------------------------------
#' set.seed(4747)
#' est <- cv_vim(Y = y, X = x, indx = 2, V = 2,
#' type = "r_squared", run_regression = TRUE,
#' SL.library = learners, cvControl = list(V = 2), alpha = 0.05)
#'
#' # ------------------------------------------
#' # doing things by hand, and plugging them in 
#' # (with a small number of folds, for illustration only)
#' # ------------------------------------------
#' # set up the folds
#' indx <- 2
#' V <- 2
#' set.seed(4747)
#' outer_folds <- sample(rep(seq_len(2), length = n))
#' inner_folds_1 <- sample(rep(seq_len(V), length = sum(outer_folds == 1)))
#' inner_folds_2 <- sample(rep(seq_len(V), length = sum(outer_folds == 2)))
#' y_1 <- y[outer_folds == 1, , drop = FALSE]
#' x_1 <- x[outer_folds == 1, , drop = FALSE]
#' y_2 <- y[outer_folds == 2, , drop = FALSE]
#' x_2 <- x[outer_folds == 2, , drop = FALSE]
#' # get the fitted values by fitting the super learner on each pair
#' fhat_ful <- list()
#' fhat_red <- list()
#' for (v in 1:V) {
#'     # fit super learner
#'     fit <- SuperLearner::SuperLearner(Y = y_1[inner_folds_1 != v, , drop = FALSE],
#'      X = x_1[inner_folds_1 != v, , drop = FALSE],
#'      SL.library = learners, cvControl = list(V = V))
#'     fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
#'     # get predictions on the validation fold
#'     fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit,
#'      newdata = x_1[inner_folds_1 == v, , drop = FALSE])$pred
#'     # fit the super learner on the reduced covariates
#'     red <- SuperLearner::SuperLearner(Y = y_2[inner_folds_2 != v, , drop = FALSE],
#'      X = x_2[inner_folds_2 != v, -indx, drop = FALSE],
#'      SL.library = learners, cvControl = list(V = V))
#'     # get predictions on the validation fold
#'     fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red,
#'      newdata = x_2[inner_folds_2 == v, -indx, drop = FALSE])$pred
#' }
#' est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
#' V = V, folds = list(outer_folds = outer_folds,
#' inner_folds = list(inner_folds_1, inner_folds_2)),
#' type = "r_squared", run_regression = FALSE, alpha = 0.05)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the \code{SuperLearner} function and package.
#' @export
cv_vim <- function(Y = NULL, X = NULL, f1 = NULL, f2 = NULL, indx = 1, 
                   V = length(unique(folds)), folds = NULL, 
                   stratified = FALSE, type = "r_squared", 
                   run_regression = TRUE, 
                   SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"), 
                   alpha = 0.05, delta = 0, scale = "identity", 
                   na.rm = FALSE, C = rep(1, length(Y)), Z = NULL, 
                   ipc_weights = rep(1, length(Y)), 
                   ipc_est_type = "aipw", ...) {
    # check to see if f1 and f2 are missing
    # if the data is missing, stop and throw an error
    check_inputs(Y, X, f1, f2, indx)

    # check to see if Y is a matrix or data.frame; if not, make it one 
    # (just for ease of reading)
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

    # if we need to run the regression, fit Super Learner with the given library
    if (run_regression) {
        X_cc <- subset(X, C == 1, drop = FALSE)
        # set up the cross-fitting
        outer_folds <- .make_folds(Y, V = 2, C = C, stratified = stratified)
        inner_folds_1 <- .make_folds(
            Y[outer_folds == 1, , drop = FALSE], V = V, 
            C = C[outer_folds == 1], stratified = stratified
        )
        inner_folds_2 <- .make_folds(
            Y[outer_folds == 2, , drop = FALSE], V = V, 
            C = C[outer_folds == 2], stratified = stratified
        )
        outer_folds_cc <- outer_folds[C == 1]
        inner_folds_1_cc <- inner_folds_1[C[outer_folds == 1] == 1]
        inner_folds_2_cc <- inner_folds_2[C[outer_folds == 2] == 1]

        # fit the super learner on each full/reduced pair
        fhat_ful <- list()
        fhat_red <- list()
        for (v in 1:V) {
            # fit super learner
            fit <- SuperLearner::SuperLearner(
                Y = Y_cc[(outer_folds_cc == 1), , 
                         drop = FALSE][inner_folds_1_cc != v, , drop = FALSE], 
                X = X_cc[(outer_folds_cc == 1), , 
                         drop = FALSE][inner_folds_1_cc != v, , drop = FALSE], 
                SL.library = SL.library, 
                obsWeights = weights_cc[(outer_folds_cc == 1)][inner_folds_1_cc != v], 
                ...
            )
            fitted_v <- SuperLearner::predict.SuperLearner(
                fit, onlySL = TRUE
            )$pred
            # get predictions on the validation fold
            fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(
                fit, 
                newdata = X_cc[(outer_folds_cc == 1), , 
                               drop = FALSE][inner_folds_1_cc == v, , drop = FALSE], 
                onlySL = TRUE
            )$pred
            # fit the super learner on the reduced covariates:
            # if the reduced set of covariates is empty, return the mean
            # otherwise, if type is r_squared or anova, always use gaussian; if first regression was mean, use Y instead
            if (ncol(X_cc[(outer_folds_cc == 2), , drop = FALSE][, -indx, drop = FALSE]) == 0) {
                fhat_red[[v]] <- mean(
                    Y_cc[(outer_folds_cc == 2), , 
                         drop = FALSE][inner_folds_2_cc != v, , drop = FALSE]
                )
            } else {
                arg_lst <- list(...)
                if (length(unique(fitted_v)) == 1) {
                    arg_lst$Y <- Y_cc[(outer_folds_cc == 2), , 
                                      drop = FALSE][inner_folds_2_cc != v, , 
                                                    drop = FALSE]
                } else if (type == "r_squared" || type == "anova") {
                    arg_lst$family <- stats::gaussian()
                    if (any(grepl("cvControl", names(arg_lst)))) {
                        arg_lst$cvControl$stratifyCV <- FALSE
                    }
                    fit_2 <- SuperLearner::SuperLearner(
                        Y = Y_cc[(outer_folds_cc == 2), , 
                                 drop = FALSE][inner_folds_2_cc != v, , drop = FALSE], 
                        X = X_cc[(outer_folds_cc == 2), , 
                                 drop = FALSE][inner_folds_2_cc != v, , drop = FALSE], 
                        SL.library = SL.library, ...
                    )
                    arg_lst$Y <- SuperLearner::predict.SuperLearner(
                        fit_2, onlySL = TRUE
                    )$pred
                } else {
                    arg_lst$Y <- Y_cc[(outer_folds_cc == 2), , 
                                      drop = FALSE][inner_folds_2_cc != v, , 
                                                    drop = FALSE]
                    # get the family
                    if (is.character(arg_lst$family))
                        arg_lst$family <- get(arg_lst$family, mode = "function", 
                                              envir = parent.frame())
                }
                arg_lst$X <- X_cc[(outer_folds_cc == 2), , 
                                  drop = FALSE][inner_folds_2_cc != v, -indx, 
                                                drop = FALSE]
                arg_lst$SL.library <- SL.library
                arg_lst$obsWeights <- weights_cc[(outer_folds_cc == 2)][inner_folds_2_cc != v]
                red <- do.call(SuperLearner::SuperLearner, arg_lst)
                # get predictions on the validation fold
                fhat_red[[v]] <- SuperLearner::predict.SuperLearner(
                    red, 
                    newdata = X_cc[(outer_folds_cc == 2), , 
                                   drop = FALSE][inner_folds_2_cc == v, -indx, 
                                                 drop = FALSE], onlySL = TRUE
                )$pred
            }
        }
        full <- reduced <- NA
        folds <- list(
            outer_folds = outer_folds, 
            inner_folds = list(inner_folds_1, inner_folds_2)
        )

    } else { # otherwise they are fitted values
        # check to make sure that the fitted values, folds are what we expect
        check_fitted_values(Y, f1, f2, folds, V = V, cv = TRUE)
        # set up the fitted value objects (both are lists!)
        fhat_ful <- f1
        fhat_red <- f2
        # set up the folds objects
        outer_folds <- folds[[1]]
        outer_folds_cc <- outer_folds[C == 1]
        inner_folds_1 <- folds[[2]][[1]]
        inner_folds_2 <- folds[[2]][[2]]
        inner_folds_1_cc <- inner_folds_1[C[outer_folds == 1] == 1]
        inner_folds_2_cc <- inner_folds_2[C[outer_folds == 2] == 1]

        full <- reduced <- NA
    }
    arg_lst <- list(...)
    if (!is.null(names(arg_lst)) && any(grepl("cvControl", names(arg_lst)))) {
        arg_lst$cvControl$stratifyCV <- FALSE
    }
    # calculate the estimators, EIFs
    if (full_type == "anova" || full_type == "regression") {
        est_lst <- sapply(
            1:V, 
            function(v) 
                do.call(
                    measure_anova,
                    args = c(
                        list(full = fhat_ful[[v]], 
                             reduced = fhat_red[[v]], 
                             y = Y_cc[outer_folds_cc == 1, , 
                                  drop = FALSE][inner_folds_1_cc == v], 
                             C = C[outer_folds == 1][inner_folds_1 == v], 
                             Z = Z_in[outer_folds == 1, , 
                                      drop = FALSE][inner_folds_1 == v, , 
                                                    drop = FALSE], 
                             ipc_weights = ipc_weights[outer_folds == 1][inner_folds_1 == v], 
                             ipc_fit_type = "SL", na.rm = na.rm, 
                             ipc_est_type = ipc_est_type, scale = scale,
                             SL.library = SL.library),
                        arg_lst
                    )
                ), 
            simplify = FALSE
        )
        point_ests <- unlist(lapply(est_lst, function(x) x$point_est))
        naives <- unlist(lapply(est_lst, function(x) x$naive))
        est <- mean(point_ests)
        naive <- mean(naives)
        all_eifs <- lapply(est_lst, function(x) x$eif)
        eif <- vector("numeric", nrow(Y))
        for (v in 1:V) {
            eif[inner_folds_1 == v] <- all_eifs[[v]]
        }
        predictiveness_full <- rep(NA, V)
        predictiveness_redu <- rep(NA, V)
        eif_full <- rep(list(rep(NA, sum(outer_folds == 1))), V)
        eif_redu <- rep(list(rep(NA, sum(outer_folds == 2))), V)
        se_full <- rep(NA, V)
        se_redu <- rep(NA, V)
    } else {
        est_lst_full <- do.call(
            est_predictiveness_cv,
            args = c(
                list(fitted_values = fhat_ful, 
                     y = Y_cc[outer_folds_cc == 1, , drop = FALSE], 
                     full_y = Y_cc,
                     folds = inner_folds_1_cc, type = full_type, 
                     C = C[outer_folds == 1], 
                     Z = Z_in[outer_folds == 1, , drop = FALSE], 
                     folds_Z = inner_folds_1, 
                     ipc_weights = ipc_weights[outer_folds == 1], 
                     ipc_fit_type = "SL", scale = scale, 
                     ipc_est_type = ipc_est_type, na.rm = na.rm, 
                     SL.library = SL.library),
                arg_lst
            )
        )
        est_lst_redu <- do.call(
            est_predictiveness_cv,
            args = c(
                list(fitted_values = fhat_red, 
                     y = Y_cc[outer_folds_cc == 2, , drop = FALSE], 
                     full_y = Y_cc,
                     folds = inner_folds_2_cc, type = full_type, 
                     C = C[outer_folds == 2], 
                     Z = Z_in[outer_folds == 2, , drop = FALSE], 
                     folds_Z = inner_folds_2, 
                     ipc_weights = ipc_weights[outer_folds == 2], 
                     ipc_fit_type = "SL", scale = scale, 
                     ipc_est_type = ipc_est_type, na.rm = na.rm, 
                     SL.library = SL.library),
                arg_lst
            )
        )
        predictiveness_full <- est_lst_full$point_est
        predictiveness_redu <- est_lst_redu$point_est
        all_eifs_full <- est_lst_full$all_eifs
        eif_full <- est_lst_full$eif
        all_eifs_redu <- est_lst_redu$all_eifs
        eif_redu <- est_lst_redu$eif
        se_full <- vimp_se(list(est = predictiveness_full, 
                                eif = eif_full,
                                all_eifs = all_eifs_full), na.rm = na.rm)
        se_redu <- vimp_se(list(est = predictiveness_redu, 
                                eif = eif_redu,
                                all_eifs = all_eifs_redu), na.rm = na.rm)
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
        all_eifs <- sapply(1:V, function(v) {
            all_eifs_full[[v]] - all_eifs_redu[[v]]
        }, simplify = FALSE)
    }
    # compute the standard error
    se <- vimp_se(list(est = est, eif = eif, all_eifs = all_eifs), 
                  na.rm = na.rm)

    # if est < 0, set to zero and print warning
    if (est < 0 && !is.na(est)) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    } else if (is.na(est)) {
        warning("Original estimate NA; consider using a different library of learners.")
    }

    # calculate the confidence interval
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
            se_full = se_full, se_reduced = se_redu, 
            delta = delta, alpha = alpha
        )
    }

    # create the output and return it (as a tibble)
    chr_indx <- paste(as.character(indx), collapse = ",")
    mat <- tibble::tibble(
        s = chr_indx, est = est, se = se[1], cil = ci[1], ciu = ci[2], 
        test = hyp_test$test, p_value = hyp_test$p_value
    )
    output <- list(s = chr_indx,
                 SL.library = SL.library,
                 full_fit = fhat_ful, red_fit = fhat_red,
                 est = est,
                 naive = naive,
                 eif = eif,
                 se = se, ci = ci,
                 all_eifs = all_eifs,
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
                 folds = folds,
                 y = Y,
                 ipc_weights = ipc_weights,
                 scale = scale,
                 mat = mat)

    # make it also an vim object
    tmp.cls <- class(output)
    class(output) <- c("vim", type, tmp.cls)
    return(output)
}
