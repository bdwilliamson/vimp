#' Nonparametric Intrinsic Variable Importance Estimates and Inference using Cross-fitting
#'
#' Compute estimates and confidence intervals using cross-fitting for
#' nonparametric intrinsic variable importance based on the
#' population-level contrast between the oracle predictiveness using the
#' feature(s) of interest versus not.
#'
#' @inheritParams vim
#' @param cross_fitted_f1 the predicted values on validation data from a
#'   flexible estimation technique regressing Y on X in the training data. Provided as
#'   either (a) a vector, where each element is
#'   the predicted value when that observation is part of the validation fold;
#'   or (b) a list of length V, where each element in the list is a set of predictions on the
#'   corresponding validation data fold.
#'   If sample-splitting is requested, then these must be estimated specially; see Details. However,
#'   the resulting vector should be the same length as \code{Y}; if using a list, then the summed
#'   length of each element across the list should be the same length as \code{Y} (i.e.,
#'   each observation is included in the predictions).
#' @param cross_fitted_f2 the predicted values on validation data from a
#'   flexible estimation technique regressing either (a) the fitted values in
#'   \code{cross_fitted_f1}, or (b) Y, on X withholding the columns in \code{indx}.
#'   Provided as either (a) a vector, where each element is
#'   the predicted value when that observation is part of the validation fold;
#'   or (b) a list of length V, where each element in the list is a set of predictions on the
#'   corresponding validation data fold.
#'   If sample-splitting is requested, then these must be estimated specially; see Details. However,
#'   the resulting vector should be the same length as \code{Y}; if using a list, then the summed
#'   length of each element across the list should be the same length as \code{Y} (i.e.,
#'   each observation is included in the predictions).
#' @param f1 the fitted values from a flexible estimation technique
#'   regressing Y on X. If sample-splitting is requested, then these must be
#'   estimated specially; see Details. If \code{cross_fitted_se = TRUE},
#'   then this argument is not used.
#' @param f2 the fitted values from a flexible estimation technique
#'   regressing either (a) \code{f1} or (b) Y on X withholding the columns in
#'   \code{indx}. If sample-splitting is requested, then these must be
#'   estimated specially; see Details. If \code{cross_fitted_se = TRUE},
#'   then this argument is not used.
#' @param V the number of folds for cross-fitting, defaults to 5. If
#'   \code{sample_splitting = TRUE}, then a special type of \code{V}-fold cross-fitting
#'   is done. See Details for a more detailed explanation.
#' @param cross_fitting_folds the folds for cross-fitting. Only used if
#'   \code{run_regression = FALSE}.
#' @param cross_fitted_se should we use cross-fitting to estimate the standard
#'   errors (\code{TRUE}, the default) or not (\code{FALSE})?
#'
#' @return An object of class \code{vim}. See Details for more information.
#'
#' @details We define the population variable importance measure (VIM) for the
#' group of features (or single feature) \eqn{s} with respect to the
#' predictiveness measure \eqn{V} by
#' \deqn{\psi_{0,s} := V(f_0, P_0) - V(f_{0,s}, P_0),} where \eqn{f_0} is
#' the population predictiveness maximizing function, \eqn{f_{0,s}} is the
#' population predictiveness maximizing function that is only allowed to access
#' the features with index not in \eqn{s}, and \eqn{P_0} is the true
#' data-generating distribution.
#'
#' Cross-fitted VIM estimates are computed differently if sample-splitting
#' is requested versus if it is not. We recommend using sample-splitting
#' in most cases, since only in this case will inferences be valid if
#' the variable(s) of interest have truly zero population importance.
#' The purpose of cross-fitting is to estimate \eqn{f_0} and \eqn{f_{0,s}}
#' on independent data from estimating \eqn{P_0}; this can result in improved
#' performance, especially when using flexible learning algorithms. The purpose
#' of sample-splitting is to estimate \eqn{f_0} and \eqn{f_{0,s}} on independent
#' data; this allows valid inference under the null hypothesis of zero importance.
#'
#' Without sample-splitting, cross-fitted VIM estimates are obtained by first
#' splitting the data into \eqn{K} folds; then using each fold in turn as a
#' hold-out set, constructing estimators \eqn{f_{n,k}} and \eqn{f_{n,k,s}} of
#' \eqn{f_0} and \eqn{f_{0,s}}, respectively on the training data and estimator
#' \eqn{P_{n,k}} of \eqn{P_0} using the test data; and finally, computing
#' \deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K \{V(f_{n,k},P_{n,k}) - V(f_{n,k,s}, P_{n,k})\}.}
#'
#' With sample-splitting, cross-fitted VIM estimates are obtained by first
#' splitting the data into \eqn{2K} folds. These folds are further divided
#' into 2 groups of folds. Then, for each fold \eqn{k} in the first group,
#' estimator \eqn{f_{n,k}} of \eqn{f_0} is constructed using all data besides
#' the kth fold in the group (i.e., \eqn{(2K - 1)/(2K)} of the data) and
#' estimator \eqn{P_{n,k}} of \eqn{P_0} is constructed using the held-out data
#' (i.e., \eqn{1/2K} of the data); then, computing
#' \deqn{v_{n,k} = V(f_{n,k},P_{n,k}).}
#' Similarly, for each fold \eqn{k} in the second group,
#' estimator \eqn{f_{n,k,s}} of \eqn{f_{0,s}} is constructed using all data
#' besides the kth fold in the group (i.e., \eqn{(2K - 1)/(2K)} of the data)
#' and estimator \eqn{P_{n,k}} of \eqn{P_0} is constructed using the held-out
#' data (i.e., \eqn{1/2K} of the data); then, computing
#' \deqn{v_{n,k,s} = V(f_{n,k,s},P_{n,k}).}
#' Finally,
#' \deqn{\psi_{n,s} := K^{(-1)}\sum_{k=1}^K \{v_{n,k} - v_{n,k,s}\}.}
#'
#' See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind the \code{cv_vim} function, and the
#' validity of the confidence intervals.
#'
#' In the interest of transparency, we return most of the calculations
#' within the \code{vim} object. This results in a list including:
#' \describe{
#'  \item{s}{the column(s) to calculate variable importance for}
#'  \item{SL.library}{the library of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{the fitted values of the chosen method fit to the full data (a list, for train and test data)}
#'  \item{red_fit}{the fitted values of the chosen method fit to the reduced data (a list, for train and test data)}
#'  \item{est}{the estimated variable importance}
#'  \item{naive}{the naive estimator of variable importance}
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
#'  \item{sample_splitting_folds}{the folds used for hypothesis testing}
#'  \item{cross_fitting_folds}{the folds used for cross-fitting}
#'  \item{y}{the outcome}
#'  \item{ipc_weights}{the weights}
#'  \item{cluster_id}{the cluster IDs}
#'  \item{mat}{a tibble with the estimate, SE, CI, hypothesis testing decision, and p-value}
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
#' Y <- matrix(y)
#' set.seed(4747)
#' # Note that the CV.SuperLearner should be run with an outer layer
#' # of 2*V folds (for V-fold cross-fitted importance)
#' full_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
#' Y = Y, X = x, SL.library = learners, cvControl = list(V = 2 * V),
#' innerCvControl = list(list(V = V))
#' ))
#' full_cv_preds <- full_cv_fit$SL.predict
#' # use the same cross-fitting folds for reduced
#' reduced_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
#'     Y = Y, X = x[, -indx, drop = FALSE], SL.library = learners,
#'     cvControl = SuperLearner::SuperLearner.CV.control(
#'         V = 2 * V, validRows = full_cv_fit$folds
#'     ),
#'     innerCvControl = list(list(V = V))
#' ))
#' reduced_cv_preds <- reduced_cv_fit$SL.predict
#' # for hypothesis testing
#' cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
#' set.seed(1234)
#' sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
#' set.seed(5678)
#' est <- cv_vim(Y = y, cross_fitted_f1 = full_cv_preds,
#' cross_fitted_f2 = reduced_cv_preds, indx = 2, delta = 0, V = V, type = "r_squared",
#' cross_fitting_folds = cross_fitting_folds,
#' sample_splitting_folds = sample_splitting_folds,
#' run_regression = FALSE, alpha = 0.05, na.rm = TRUE)
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the
#'   \code{SuperLearner} function and package.
#' @export
cv_vim <- function(Y = NULL, X = NULL, cross_fitted_f1 = NULL,
                   cross_fitted_f2 = NULL, f1 = NULL, f2 = NULL, indx = 1,
                   V = ifelse(is.null(cross_fitting_folds), 5, length(unique(cross_fitting_folds))),
                   sample_splitting = TRUE, final_point_estimate = "split",
                   sample_splitting_folds = NULL, cross_fitting_folds = NULL,
                   stratified = FALSE, type = "r_squared",
                   run_regression = TRUE,
                   SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                   alpha = 0.05, delta = 0, scale = "identity",
                   na.rm = FALSE, C = rep(1, length(Y)), Z = NULL,
                   ipc_scale = "identity", ipc_weights = rep(1, length(Y)),
                   ipc_est_type = "aipw", scale_est = TRUE,
                   nuisance_estimators_full = NULL,
                   nuisance_estimators_reduced = NULL, exposure_name = NULL,
                   cross_fitted_se = TRUE, bootstrap = FALSE, b = 1000,
                   boot_interval_type = "perc", clustered = FALSE,
                   cluster_id = rep(NA, length(Y)), ...) {
    # check to see if f1 and f2 are missing
    # if the data is missing, stop and throw an error
    check_inputs(Y, X, cross_fitted_f1, cross_fitted_f2, indx)

    if (bootstrap & clustered & sum(is.na(cluster_id)) > 0){
      stop(paste0("If using clustered bootstrap, cluster IDs must be provided",
                  " for all observations."))
    }

    if (sample_splitting) {
        ss_V <- V * 2
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
    X_cc <- X[C == 1, , drop = FALSE]
    if (is.null(exposure_name)) {
      A_cc <- rep(1, length(Y_cc))
    } else {
      A_cc <- X_cc[, exposure_name]
    }
    X_cc <- X_cc[, !(names(X_cc) %in% exposure_name), drop = FALSE]
    weights_cc <- cc_lst$weights
    Z_in <- cc_lst$Z

    # get the correct measure function; if not one of the supported ones, say so
    full_type <- get_full_type(type)

    # get sample-splitting folds (if null and/or run_regression = TRUE)
    if (is.null(sample_splitting_folds) | run_regression) {
        if (is.null(cross_fitting_folds) & !run_regression) {
            stop(paste0("You must specify the folds used for cross-fitting if ",
                        "run_regression = FALSE."))
        }
        if (run_regression) {
            # set up the cross-fitting folds
            cross_fitting_folds <- make_folds(
                Y, V = ss_V, C = C, stratified = stratified
            )
        }
        if (sample_splitting) {
            # create sample splitting folds; equal number in each
            sample_splitting_folds <- make_folds(
                unique(cross_fitting_folds), V = 2
            )
        } else {
            sample_splitting_folds <- rep(1, ss_V)
        }
    }
    cross_fitting_folds_cc <- cross_fitting_folds[C == 1]

    # if we need to run the regression, fit Super Learner with the given library
    if (run_regression) {
        full_feature_vec <- 1:ncol(X_cc)
        full_sl_lst <- run_sl(Y = Y_cc, X = X_cc, V = ss_V,
                              SL.library = SL.library, s = full_feature_vec,
                              sample_splitting = sample_splitting,
                              cv_folds = cross_fitting_folds_cc,
                              ss_folds = sample_splitting_folds, split = 1,
                              verbose = FALSE, weights = weights_cc,
                              cross_fitted_se = cross_fitted_se,
                              vector = TRUE, ...)
        red_split <- switch((sample_splitting) + 1, 1, 2)
        red_Y <- Y_cc
        if (full_type == "r_squared" || full_type == "anova") {
            if (sample_splitting) {
                non_ss_folds <- rep(2, nrow(Y_cc))
                full_sl_lst_2 <- run_sl(Y = Y_cc, X = X_cc, V = 1,
                                        SL.library = SL.library, s = full_feature_vec,
                                        sample_splitting = FALSE,
                                        ss_folds = non_ss_folds, split = 2,
                                        verbose = FALSE, weights = weights_cc,
                                        cross_fitted_se = FALSE,
                                        vector = TRUE, ...)
                red_Y <- matrix(full_sl_lst_2$preds, ncol = 1)
            } else {
                full_sl_lst_2 <- run_sl(Y = Y_cc, X = X_cc, V = 1,
                                        SL.library = SL.library, s = full_feature_vec,
                                        sample_splitting = FALSE,
                                        ss_folds = rep(2, nrow(Y_cc)), split = 2,
                                        cv_folds = cross_fitting_folds_cc,
                                        verbose = FALSE, weights = weights_cc,
                                        cross_fitted_se = FALSE,
                                        vector = TRUE, ...)
                red_Y <- matrix(full_sl_lst_2$preds, ncol = 1)
            }
            if (length(unique(red_Y)) == 1) {
                red_Y <- Y_cc
            }
        }
        redu_sl_lst <- run_sl(Y = red_Y, X = X_cc, V = ss_V,
                              SL.library = SL.library, s = full_feature_vec[-indx],
                              sample_splitting = sample_splitting,
                              cv_folds = cross_fitting_folds_cc,
                              ss_folds = sample_splitting_folds, split = red_split,
                              verbose = FALSE, weights = weights_cc,
                              cross_fitted_se = cross_fitted_se,
                              vector = TRUE, ...)
        full <- full_sl_lst$fit
        reduced <- redu_sl_lst$fit
        full_preds <- full_sl_lst$preds
        redu_preds <- redu_sl_lst$preds
        non_cf_full_preds <- full_sl_lst$preds_non_cf_se
        non_cf_redu_preds <- redu_sl_lst$preds_non_cf_se
    } else { # otherwise they are fitted values
        # check to make sure that the fitted values, folds are what we expect
        check_fitted_values(Y = Y, cross_fitted_f1 = cross_fitted_f1,
                            cross_fitted_f2 = cross_fitted_f2, f1 = f1, f2 = f2,
                            sample_splitting_folds = sample_splitting_folds,
                            cross_fitting_folds = cross_fitting_folds,
                            cross_fitted_se = cross_fitted_se, V = V,
                            ss_V = ss_V, cv = TRUE)
        # if cross_fitted_f1 and/or cross_fitted_f2 aren't vectors, make them vectors
        if (!is.numeric(cross_fitted_f1)) {
            cross_fitted_f1 <- extract_sampled_split_predictions(
                preds = cross_fitted_f1, sample_splitting = sample_splitting,
                sample_splitting_folds = switch((sample_splitting) + 1,
                                                rep(1, V), sample_splitting_folds),
                cross_fitting_folds = cross_fitting_folds,
                full = TRUE, vector = TRUE
            )
        }
        if (!is.numeric(cross_fitted_f2)) {
            cross_fitted_f2 <- extract_sampled_split_predictions(
                preds = cross_fitted_f2, sample_splitting = sample_splitting,
                sample_splitting_folds = switch((sample_splitting) + 1,
                                                rep(2, V), sample_splitting_folds),
                cross_fitting_folds = cross_fitting_folds,
                full = FALSE, vector = TRUE
            )
        }
        # set up the fitted value objects (both are vectors!)
        full_preds <- cross_fitted_f1
        redu_preds <- cross_fitted_f2
        # non-cross-fitted fits (only used if cross_fitted_se = FALSE)
        non_cf_full_preds <- f1
        non_cf_redu_preds <- f2

        full <- reduced <- NA
        cross_fitting_folds_cc <- cross_fitting_folds[C == 1]
    }
    arg_lst <- list(...)
    # set method and family to compatible with continuous values, for EIF estimation
    arg_lst <- process_arg_lst(arg_lst)
    eifs_lst <- NA
    # calculate the estimators, EIFs
    if (full_type == "anova") {
        # no sample-splitting, since no hypothesis testing
        est_lst <- lapply(as.list(seq_len(V)),
            function(v)
                do.call(
                    measure_anova,
                    args = c(
                        list(full = full_preds[[v]],
                             reduced = redu_preds[[v]],
                             y = Y_cc[cross_fitting_folds_cc == v],
                             full_y = Y_cc,
                             C = C[cross_fitting_folds == v],
                             Z = Z_in[cross_fitting_folds == v, ,
                                      drop = FALSE],
                             folds_Z = cross_fitting_folds,
                             ipc_weights = ipc_weights[cross_fitting_folds == v],
                             ipc_fit_type = "SL", na.rm = na.rm,
                             ipc_est_type = ipc_est_type, scale = ipc_scale,
                             SL.library = SL.library),
                        arg_lst
                    )
                )
        )
        point_ests <- unlist(lapply(est_lst, function(x) x$point_est))
        naives <- unlist(lapply(est_lst, function(x) x$naive))
        est <- mean(point_ests)
        naive <- mean(naives)
        if (cross_fitted_se) {
            eifs_full <- NA
            eifs_redu <- NA
            eifs_lst <- unlist(lapply(est_lst, function(x) x$eif))
            se <- sqrt(mean(unlist(lapply(eifs_lst, function(x) t(x) %*% x / length(x)))) / nrow(Y_cc))
        } else {
            eif <- measure_anova(
                full = non_cf_full_preds, reduced = non_cf_redu_preds,
                y = Y_cc, full_y = Y_cc,
                C = C, Z = Z_in,
                ipc_weights = ipc_weights,
                ipc_fit_type = "SL", na.rm = na.rm,
                SL.library = SL.library, arg_lst
            )$eif
            se <- sqrt(mean(eif^2) / nrow(Y_cc))
        }
        predictiveness_full <- NA
        predictiveness_redu <- NA
        eif_full <- rep(NA, sum(cross_fitting_folds_cc == 1))
        eif_redu <- rep(NA, sum(cross_fitting_folds_cc == 2))
        se_full <- NA
        se_redu <- NA
        if (bootstrap) {
            boot_results <- bootstrap_se(Y = Y_cc, f1 = non_cf_full_preds,
                                         f2 = non_cf_redu_preds, type = full_type,
                                         b = b, clustered = clustered,
                                         cluster_id = cluster_id)
            se <- boot_results$se
        }
    } else {
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
        predictiveness_full_object <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[full_test_cc], full_y = Y_cc,
               a = A_cc[full_test_cc], fitted_values = full_preds[full_test_cc],
               cross_fitting_folds = cf_folds_full_cc, C = C[full_test],
               Z = Z_in[full_test, , drop = FALSE],
               folds_Z = cf_folds_full, ipc_weights = ipc_weights[full_test],
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library, nuisance_estimators = lapply(
                 nuisance_estimators_full, function(l) {
                   l[full_test_cc]
                 }
               )), arg_lst
        ))
        predictiveness_reduced_object <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[redu_test_cc], full_y = Y_cc,
               a = A_cc[redu_test_cc], fitted_values = redu_preds[redu_test_cc],
               cross_fitting_folds = cf_folds_redu_cc, C = C[redu_test],
               Z = Z_in[redu_test, , drop = FALSE],
               folds_Z = cf_folds_redu, ipc_weights = ipc_weights[redu_test],
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library, nuisance_estimators = lapply(
                 nuisance_estimators_reduced, function(l) {
                   l[redu_test_cc]
                 }
               )), arg_lst
        ))
        predictiveness_full_lst <- estimate(predictiveness_full_object)
        predictiveness_redu_lst <- estimate(predictiveness_reduced_object)
        eifs_full <- predictiveness_full_lst$all_eifs
        eifs_redu <- predictiveness_redu_lst$all_eifs
        # use non-cross-fitted SE if requested
        if (!cross_fitted_se) {
            eif_full_lst <- do.call(
                est_predictiveness,
                args = c(list(fitted_values = non_cf_full_preds[full_test_cc],
                              y = Y_cc[full_test_cc], full_y = Y_cc, folds = cf_folds_full_cc,
                              type = full_type, C = C, Z = Z_in, folds_Z = cf_folds_full,
                              ipc_weights = ipc_weights,
                              ipc_fit_type = "SL", scale = ipc_scale,
                              ipc_est_type = ipc_est_type, na.rm = na.rm,
                              SL.library = SL.library),
                         arg_lst), quote = TRUE
            )
            eif_full <- eif_full_lst$eif
            eif_redu_lst <- do.call(
                est_predictiveness,
                args = c(list(fitted_values = non_cf_redu_preds[full_test_cc],
                              y = Y_cc[redu_test_cc], full_y = Y_cc, folds = cf_folds_redu_cc,
                              type = full_type, C = C, Z = Z_in, folds_Z = cf_folds_redu,
                              ipc_weights = ipc_weights,
                              ipc_fit_type = "SL", scale = ipc_scale,
                              ipc_est_type = ipc_est_type, na.rm = na.rm,
                              SL.library = SL.library),
                         arg_lst), quote = TRUE
            )
            eif_redu <- eif_redu_lst$eif
            var_full <- mean(eif_full ^ 2)
            var_redu <- mean(eif_redu ^ 2)
        } else {
            eif_full <- predictiveness_full_lst$eif
            eif_redu <- predictiveness_redu_lst$eif
            var_full <- mean(unlist(lapply(as.list(seq_len(V)), function(k) {
                mean(eifs_full[[k]] ^ 2)
            })))
            var_redu <- mean(unlist(lapply(as.list(seq_len(V)), function(k) {
                mean(eifs_redu[[k]] ^ 2)
            })))
        }
        predictiveness_full <- predictiveness_full_lst$point_est
        predictiveness_redu <- predictiveness_redu_lst$point_est
        est <- predictiveness_full - predictiveness_redu
        naive <- NA
        se_full <- sqrt(var_full / sum(full_test_cc))
        se_redu <- sqrt(var_redu / sum(redu_test_cc))
        if (bootstrap & !sample_splitting & !cross_fitted_se) {
            boot_results <- bootstrap_se(Y = Y_cc, f1 = non_cf_full_preds,
                                         f2 = non_cf_redu_preds, type = full_type,
                                         b = b, boot_interval_type = boot_interval_type,
                                         alpha = alpha, clustered = clustered,
                                         cluster_id = cluster_id)
            se <- boot_results$se
            se_full <- boot_results$se_full
            se_redu <- boot_results$se_reduced
        } else {
            if (bootstrap) {
                warning(paste0("Bootstrap-based standard error estimates are currently",
                               " only available if sample_splitting = FALSE. Returning",
                               " standard error estimates based on the efficient",
                               " influence function instead."))
            }
            if (!cross_fitted_se) {
                se <- vimp_se(eif_full = eif_full, eif_reduced = eif_redu,
                              cross_fit = FALSE, sample_split = sample_splitting,
                              na.rm = na.rm)
            } else {
                se <- vimp_se(eif_full = eifs_full, eif_reduced = eifs_redu,
                              cross_fit = TRUE, sample_split = sample_splitting,
                              na.rm = na.rm)
            }
        }
    }
    est_for_inference <- est
    predictiveness_full_for_inference <- predictiveness_full
    predictiveness_reduced_for_inference <- predictiveness_redu
    # if sample-splitting was requested and final_point_estimate isn't "split", estimate
    # the required quantities
    if (sample_splitting & (final_point_estimate != "split")) {
      k_fold_lst_for_est <- list(
        full = cross_fitting_folds, reduced = cross_fitting_folds
      )
      cf_folds_for_est <- k_fold_lst_for_est$full
      cf_folds_for_est_cc <- k_fold_lst_for_est$full[C == 1]
      if (final_point_estimate == "full") {
        est_pred_full <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc, full_y = Y_cc,
               a = A_cc, fitted_values = full_preds,
               cross_fitting_folds = cf_folds_for_est_cc, C = C,
               Z = Z_in, folds_Z = cf_folds_for_est, ipc_weights = ipc_weights,
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library,
               nuisance_estimators = nuisance_estimators_full), arg_lst
        ))
        est_pred_reduced <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc, full_y = Y_cc,
               a = A_cc, fitted_values = redu_preds,
               cross_fitting_folds = cf_folds_for_est_cc, C = C,
               Z = Z_in, folds_Z = cf_folds_for_est, ipc_weights = ipc_weights,
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library,
               nuisance_estimators = nuisance_estimators_reduced), arg_lst
        ))
        est_pred_full_lst <- estimate(est_pred_full)
        est_pred_reduced_lst <- estimate(est_pred_reduced)
        predictiveness_full <- est_pred_full_lst$point_est
        predictiveness_redu <- est_pred_reduced_lst$point_est
        est <- predictiveness_full - predictiveness_redu
      } else {
        # swap the roles of the folds
        full_test_for_est <- (k_fold_lst$sample_splitting_folds == 2)
        redu_test_for_est <- (k_fold_lst$sample_splitting_folds == 1)
        cf_folds_full_for_est <- k_fold_lst$reduced
        cf_folds_redu_for_est <- k_fold_lst$full
        cf_folds_full_cc_for_est <- cf_folds_full_for_est[C[full_test_for_est] == 1]
        cf_folds_redu_cc_for_est <- cf_folds_redu_for_est[C[redu_test_for_est] == 1]
        full_test_cc_for_est <- full_test_for_est[C == 1]
        redu_test_cc_for_est <- redu_test_for_est[C == 1]
        est_pred_full <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[full_test_cc_for_est], full_y = Y_cc,
               a = A_cc[full_test_cc_for_est], fitted_values = full_preds[full_test_cc_for_est],
               cross_fitting_folds = cf_folds_full_cc_for_est, C = C[full_test_for_est],
               Z = Z_in[full_test_for_est, , drop = FALSE],
               folds_Z = cf_folds_full_for_est, ipc_weights = ipc_weights[full_test_for_est],
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library, nuisance_estimators = lapply(
                 nuisance_estimators_full, function(l) {
                   l[full_test_cc_for_est]
                 }
               )), arg_lst
        ))
        est_pred_reduced <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[redu_test_cc_for_est], full_y = Y_cc,
               a = A_cc[redu_test_cc_for_est], fitted_values = redu_preds[redu_test_cc_for_est],
               cross_fitting_folds = cf_folds_redu_cc_for_est, C = C[redu_test_for_est],
               Z = Z_in[redu_test_for_est, , drop = FALSE],
               folds_Z = cf_folds_redu_for_est, ipc_weights = ipc_weights[redu_test_for_est],
               ipc_fit_type = "SL", scale = ipc_scale, ipc_est_type = ipc_est_type,
               na.rm = na.rm, SL.library = SL.library, nuisance_estimators = lapply(
                 nuisance_estimators_reduced, function(l) {
                   l[redu_test_cc_for_est]
                 }
               )), arg_lst
        ))
        est_pred_full_lst <- estimate(est_pred_full)
        est_pred_reduced_lst <- estimate(est_pred_reduced)
        # compute the point estimates of predictiveness and variable importance
        predictiveness_full <- mean(c(predictiveness_full, est_pred_full_lst$point_est))
        predictiveness_redu <- mean(c(predictiveness_redu, est_pred_reduced_lst$point_est))
        est <- predictiveness_full - predictiveness_redu
      }
    }
    # if est < 0, set to zero and print warning
    if (est < 0 && !is.na(est) & scale_est) {
        est <- 0
        warning("Original estimate < 0; returning zero.")
    } else if (is.na(est)) {
        warning("Original estimate NA; consider using a different library of learners.")
    }

    # calculate the confidence interval
    ci <- vimp_ci(est_for_inference, se, scale = scale, level = 1 - alpha)
    if (bootstrap) {
        ci <- boot_results$ci
    }
    predictiveness_ci_full <- vimp_ci(
        predictiveness_full_for_inference, se = se_full, scale = scale, level = 1 - alpha
    )
    predictiveness_ci_redu <- vimp_ci(
        predictiveness_reduced_for_inference, se = se_redu, scale = scale, level = 1 - alpha
    )

    # perform a hypothesis test against the null of zero importance
    if (full_type == "anova" || full_type == "regression") {
        hyp_test <- list(test = NA, p_value = NA, test_statistics = NA)
    } else {
        hyp_test <- vimp_hypothesis_test(
            predictiveness_full = predictiveness_full_for_inference,
            predictiveness_reduced = predictiveness_reduced_for_inference,
            se = se, delta = delta, alpha = alpha
        )
    }

    # create the output and return it (as a tibble)
    chr_indx <- paste(as.character(indx), collapse = ",")
    mat <- tibble::tibble(
        s = chr_indx, est = est, se = se[1], cil = ci[1], ciu = ci[2],
        test = hyp_test$test, p_value = hyp_test$p_value
    )
    if (full_type == "anova") {
        if (cross_fitted_se) {
            final_eif <- eifs_lst
        } else {
            final_eif <- eif
        }
    } else {
        if (length(eif_full) != length(eif_redu)) {
            max_len <- max(c(length(eif_full), length(eif_redu)))
            eif_full <- c(eif_full, rep(NA, max_len - length(eif_full)))
            eif_redu <- c(eif_redu, rep(NA, max_len - length(eif_redu)))
        }
        final_eif <- eif_full - eif_redu
    }
    output <- list(s = chr_indx,
                   SL.library = SL.library,
                   full_fit = full_preds, red_fit = redu_preds,
                   est = est,
                   naive = naive,
                   eif = final_eif,
                   eif_full = eif_full, eif_redu = eif_redu,
                   all_eifs_full = eifs_full, all_eifs_redu = eifs_redu,
                   se = se, ci = ci,
                   est_for_inference = est_for_inference,
                   predictiveness_full = predictiveness_full,
                   predictiveness_reduced = predictiveness_redu,
                   predictiveness_full_for_inference = predictiveness_full_for_inference,
                   predictiveness_reduced_for_inference = predictiveness_reduced_for_inference,
                   predictiveness_ci_full = predictiveness_ci_full,
                   predictiveness_ci_reduced = predictiveness_ci_redu,
                   se_full = se_full, se_reduced = se_redu,
                   test = hyp_test$test,
                   p_value = hyp_test$p_value,
                   test_statistic = hyp_test$test_statistic,
                   full_mod = full,
                   red_mod = reduced,
                   alpha = alpha,
                   delta = delta,
                   sample_splitting_folds = sample_splitting_folds,
                   cross_fitting_folds = cross_fitting_folds,
                   y = Y,
                   ipc_weights = ipc_weights,
                   ipc_scale = ipc_scale,
                   scale = scale,
                   cluster_id = cluster_id,
                   mat = mat)

    # make it also a vim and vim_type object
    tmp.cls <- class(output)
    class(output) <- c("vim", full_type, tmp.cls)
    return(output)
}
