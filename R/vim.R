#' Nonparametric Intrinsic Variable Importance Estimates and Inference
#'
#' Compute estimates of and confidence intervals for nonparametric intrinsic
#' variable importance based on the population-level contrast between the oracle
#' predictiveness using the feature(s) of interest versus not.
#'
#' @param Y the outcome.
#' @param X the covariates. If \code{type = "average_value"}, then the exposure
#'   variable should be part of \code{X}, with its name provided in \code{exposure_name}.
#' @param f1 the fitted values from a flexible estimation technique
#'   regressing Y on X. A vector of the same length as \code{Y}; if sample-splitting
#'   is desired, then the value of \code{f1} at each position should be the result
#'   of predicting from a model trained without that observation.
#' @param f2 the fitted values from a flexible estimation technique
#'   regressing either (a) \code{f1} or (b) Y on X withholding the columns in
#'   \code{indx}. A vector of the same length as \code{Y}; if sample-splitting
#'   is desired, then the value of \code{f2} at each position should be the result
#'   of predicting from a model trained without that observation.
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
#'   another scale? (options are "log" and "logit")
#' @param na.rm should we remove NAs in the outcome and fitted values
#'   in computation? (defaults to \code{FALSE})
#' @param sample_splitting should we use sample-splitting to estimate the full and
#'   reduced predictiveness? Defaults to \code{TRUE}, since inferences made using
#'   \code{sample_splitting = FALSE} will be invalid for variables with truly zero
#'   importance.
#' @param sample_splitting_folds the folds used for sample-splitting;
#'   these identify the observations that should be used to evaluate
#'   predictiveness based on the full and reduced sets of covariates, respectively.
#'   Only used if \code{run_regression = FALSE}.
#' @param final_point_estimate if sample splitting is used, should the final point estimates
#'   be based on only the sample-split folds used for inference (\code{"split"}, the default),
#'   or should they instead be based on the full dataset (\code{"full"}) or the average
#'   across the point estimates from each sample split (\code{"average"})? All three
#'   options result in valid point estimates -- sample-splitting is only required for valid inference.
#' @param stratified if run_regression = TRUE, then should the generated
#'   folds be stratified based on the outcome (helps to ensure class balance
#'   across cross-validation folds)
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either (i) NULL (the default, in which case the argument
#'   \code{C} above must be all ones), or (ii) a character vector
#'   specifying the variable(s) among Y and X that are thought to play a
#'   role in the coarsening mechanism. To specify the outcome, use \code{"Y"}; to
#'   specify covariates, use a character number corresponding to the desired
#'   position in X (e.g., \code{"1"}).
#' @param ipc_scale what scale should the inverse probability weight correction be applied on (if any)?
#'   Defaults to "identity". (other options are "log" and "logit")
#' @param ipc_weights weights for the computed influence curve (i.e.,
#'   inverse probability weights for coarsened-at-random settings).
#'   Assumed to be already inverted (i.e., ipc_weights = 1 / [estimated
#'   probability weights]).
#' @param ipc_est_type the type of procedure used for coarsened-at-random
#'   settings; options are "ipw" (for inverse probability weighting) or
#'   "aipw" (for augmented inverse probability weighting).
#'   Only used if \code{C} is not all equal to 1.
#' @param scale_est should the point estimate be scaled to be greater than or equal to 0?
#'   Defaults to \code{TRUE}.
#' @param nuisance_estimators_full (only used if \code{type = "average_value"})
#'   a list of nuisance function estimators on the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#'   Specifically: an estimator of the optimal treatment rule; an estimator of the
#'   propensity score under the estimated optimal treatment rule; and an estimator
#'   of the outcome regression when treatment is assigned according to the estimated optimal rule.
#' @param nuisance_estimators_reduced (only used if \code{type = "average_value"})
#'   a list of nuisance function estimators on the
#'   observed data (may be within a specified fold, for cross-fitted estimates).
#'   Specifically: an estimator of the optimal treatment rule; an estimator of the
#'   propensity score under the estimated optimal treatment rule; and an estimator
#'   of the outcome regression when treatment is assigned according to the estimated optimal rule.
#' @param exposure_name (only used if \code{type = "average_value"}) the name of
#'   the exposure of interest; binary, with 1 indicating presence of the exposure and
#'   0 indicating absence of the exposure.
#' @param bootstrap should bootstrap-based standard error estimates be computed?
#'   Defaults to \code{FALSE} (and currently may only be used if
#'   \code{sample_splitting = FALSE}).
#' @param b the number of bootstrap replicates (only used if \code{bootstrap = TRUE}
#'   and \code{sample_splitting = FALSE}); defaults to 1000.
#' @param boot_interval_type the type of bootstrap interval (one of \code{"norm"},
#'   \code{"basic"}, \code{"stud"}, \code{"perc"}, or \code{"bca"}, as in
#'   \code{\link{boot}{boot.ci}}) if requested. Defaults to \code{"perc"}.
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
#' full_fit <- SuperLearner::CV.SuperLearner(Y = y, X = x,
#'                                           SL.library = learners,
#'                                           cvControl = list(V = 2),
#'                                           innerCvControl = list(list(V = V)))
#' full_fitted <- SuperLearner::predict.SuperLearner(full_fit)$pred
#' # fit the data with only X1
#' reduced_fit <- SuperLearner::CV.SuperLearner(Y = full_fitted,
#'                                              X = x[, -2, drop = FALSE],
#'                                              SL.library = learners,
#'                                              cvControl = list(V = 2, validRows = full_fit$folds),
#'                                              innerCvControl = list(list(V = V)))
#' reduced_fitted <- SuperLearner::predict.SuperLearner(reduced_fit)$pred
#'
#' est_2 <- vim(Y = y, f1 = full_fitted, f2 = reduced_fitted,
#'             indx = 2, run_regression = FALSE, alpha = 0.05,
#'             stratified = TRUE, type = "accuracy",
#'             sample_splitting_folds = get_cv_sl_folds(full_fit$folds))
#'
#' @seealso \code{\link[SuperLearner]{SuperLearner}} for specific usage of the
#'   \code{SuperLearner} function and package.
#' @export
vim <- function(Y = NULL, X = NULL, f1 = NULL, f2 = NULL, indx = 1,
                type = "r_squared", run_regression = TRUE,
                SL.library = c("SL.glmnet", "SL.xgboost", "SL.mean"),
                alpha = 0.05, delta = 0, scale = "identity", na.rm = FALSE,
                sample_splitting = TRUE, sample_splitting_folds = NULL,
                final_point_estimate = "split", stratified = FALSE,
                C = rep(1, length(Y)), Z = NULL, ipc_scale = "identity",
                ipc_weights = rep(1, length(Y)),
                ipc_est_type = "aipw", scale_est = TRUE, nuisance_estimators_full = NULL,
                nuisance_estimators_reduced = NULL, exposure_name = NULL,
                bootstrap = FALSE, b = 1000, boot_interval_type = "perc", ...) {
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

    # set up folds for sample-splitting; if sample_splitting is FALSE, these
    # aren't actually folds
    if (is.null(sample_splitting_folds) | run_regression) {
        if (sample_splitting) {
            sample_splitting_folds <- make_folds(
                Y, V = 2, C = C, stratified = stratified
            )
        } else {
            sample_splitting_folds <- rep(1, length(Y))
        }
    }
    sample_splitting_folds_cc <- sample_splitting_folds[C == 1]

    # if run_regression = TRUE, then fit SuperLearner
    if (run_regression) {
        full_feature_vec <- 1:ncol(X_cc)
        full_sl_lst <- run_sl(Y = Y_cc, X = X_cc, V = ifelse(sample_splitting, 2, 1),
                              SL.library = SL.library,
                              s = full_feature_vec, sample_splitting = sample_splitting,
                              cv_folds = sample_splitting_folds_cc,
                              ss_folds = sample_splitting_folds_cc, split = 1, verbose = FALSE,
                              weights = weights_cc, cross_fitted_se = FALSE,
                              vector = TRUE, ...)
        red_split <- switch((sample_splitting) + 1, 1, 2)
        red_Y <- Y_cc
        if (full_type == "r_squared" || full_type == "anova") {
            if (sample_splitting) {
                full_sl_lst_2 <- run_sl(Y = Y_cc, X = X_cc, V = ifelse(sample_splitting, 2, 1),
                                        SL.library = SL.library,
                                        s = full_feature_vec, sample_splitting = sample_splitting,
                                        cv_folds = sample_splitting_folds_cc,
                                        ss_folds = sample_splitting_folds_cc, split = 2, verbose = FALSE,
                                        weights = weights_cc, cross_fitted_se = FALSE,
                                        vector = TRUE, ...)
                red_Y <- matrix(full_sl_lst_2$preds)
            } else {
                red_Y <- matrix(full_sl_lst$preds, ncol = 1)
            }
            if (length(unique(red_Y)) == 1) {
                red_Y <- Y_cc
            }
        }
        redu_sl_lst <- run_sl(Y = red_Y, X = X_cc, V = ifelse(sample_splitting, 2, 1),
                              SL.library = SL.library,
                              s = full_feature_vec[-indx], sample_splitting = sample_splitting,
                              cv_folds = sample_splitting_folds_cc,
                              ss_folds = sample_splitting_folds_cc, split = red_split, verbose = FALSE,
                              weights = weights_cc, cross_fitted_se = FALSE,
                              vector = TRUE, ...)
        full <- full_sl_lst$fit
        reduced <- redu_sl_lst$fit
        full_preds <- full_sl_lst$preds
        redu_preds <- redu_sl_lst$preds
        # if variable importance based on the average value under the optimal rule is requested,
        # create a list with the necessary nuisance function estimators
        if (grepl("average_value", full_type)) {
            nuisance_estimators_full <- estimate_nuisances(fit = full, X = X_cc,
                                                           exposure_name = exposure_name,
                                                           V = ifelse(sample_splitting, 2, 1),
                                                           SL.library = SL.library,
                                                           sample_splitting = sample_splitting,
                                                           sample_splitting_folds = sample_splitting_folds_cc,
                                                           verbose = FALSE, weights = weights_cc,
                                                           cross_fitted_se = FALSE, split = 1, ...)
            nuisance_estimators_reduced <- estimate_nuisances(fit = reduced, X = X_cc %>% dplyr::select(-!!exposure_name),
                                                              exposure_name = exposure_name,
                                                              V = ifelse(sample_splitting, 2, 1),
                                                              SL.library = SL.library,
                                                              sample_splitting = sample_splitting,
                                                              sample_splitting_folds = sample_splitting_folds_cc,
                                                              verbose = FALSE, weights = weights_cc,
                                                              cross_fitted_se = FALSE, split = red_split, ...)
        } else {
            nuisance_estimators_full <- NULL
            nuisance_estimators_reduced <- NULL
        }
    } else { # otherwise they are fitted values
        # check to make sure that the fitted values, folds are what we expect
        check_fitted_values(Y = Y, f1 = f1, f2 = f2,
                            sample_splitting_folds = sample_splitting_folds,
                            cv = FALSE)
        sample_splitting_folds_cc <- sample_splitting_folds[C == 1]
        sample_splitting_folds_1 <- sample_splitting_folds_cc == 1
        sample_splitting_folds_2 <- switch(
            (sample_splitting) + 1,
            sample_splitting_folds_cc == 1, sample_splitting_folds_cc == 2
        )

        # set up the fitted value objects
        full_preds <- switch((length(f1) == nrow(Y)) + 1, f1, subset(f1, C == 1))
        redu_preds <- switch((length(f2) == nrow(Y)) + 1, f2, subset(f2, C == 1))

        full <- reduced <- NA
    }
    # calculate the estimators, EIFs
    arg_lst <- list(...)
    # set method and family to compatible with continuous values, for EIF estimation
    arg_lst <- process_arg_lst(arg_lst)
    if (full_type == "anova") {
        # no sample-splitting, since no hypothesis testing
        est_lst <- measure_anova(
            full = full_preds, reduced = redu_preds,
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
            boot_results <- bootstrap_se(Y = Y_cc, f1 = full_preds, f2 = redu_preds,
                                         type = full_type, b = b,
                                         boot_interval_type = boot_interval_type,
                                         alpha = alpha)
            se <- boot_results$se
        } else {
            se <- sqrt(mean(eif ^ 2) / length(eif))
        }
    } else {
        # if no sample splitting, estimate on the whole data
        ss_folds_full <- switch((sample_splitting) + 1,
                                rep(1, length(sample_splitting_folds_cc)),
                                sample_splitting_folds_cc)
        ss_folds_redu <- switch((sample_splitting) + 1,
                                rep(2, length(sample_splitting_folds_cc)),
                                sample_splitting_folds_cc)
        predictiveness_full_object <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[ss_folds_full == 1, , drop = FALSE],
               a = A_cc[ss_folds_full == 1], fitted_values = full_preds[ss_folds_full == 1],
               full_y = Y_cc, nuisance_estimators = lapply(nuisance_estimators_full, function(l) {
                 l[ss_folds_full == 1]
               }), C = C[sample_splitting_folds == 1],
               Z = Z_in[sample_splitting_folds == 1, , drop = FALSE],
               ipc_weights = ipc_weights[sample_splitting_folds == 1],
               ipc_fit_type = "SL", scale = ipc_scale,
               ipc_est_type = ipc_est_type, na.rm = na.rm,
               SL.library = SL.library), arg_lst
        ))
        predictiveness_reduced_object <- do.call(predictiveness_measure, c(
         list(type = full_type, y = Y_cc[ss_folds_redu == 2, , drop = FALSE],
              a = A_cc[ss_folds_redu == 2], fitted_values = redu_preds[ss_folds_redu == 2],
              full_y = Y_cc, nuisance_estimators = lapply(nuisance_estimators_reduced, function(l) {
                l[ss_folds_redu == 2]
              }), C = C[sample_splitting_folds == 2],
              Z = Z_in[sample_splitting_folds == 2, , drop = FALSE],
              ipc_weights = ipc_weights[sample_splitting_folds == 2],
              ipc_fit_type = "SL", scale = ipc_scale,
              ipc_est_type = ipc_est_type, na.rm = na.rm,
              SL.library = SL.library), arg_lst
        ))
        predictiveness_full_lst <- estimate(predictiveness_full_object)
        predictiveness_redu_lst <- estimate(predictiveness_reduced_object)
        # compute the point estimates of predictiveness and variable importance
        predictiveness_full <- predictiveness_full_lst$point_est
        predictiveness_redu <- predictiveness_redu_lst$point_est
        est <- predictiveness_full - predictiveness_redu
        naive <- NA
        # compute estimates of standard error
        eif_full <- predictiveness_full_lst$eif
        eif_redu <- predictiveness_redu_lst$eif
        se_full <- sqrt(mean(eif_full ^ 2) / length(eif_full))
        se_redu <- sqrt(mean(eif_redu ^ 2) / length(eif_redu))
        if (bootstrap & !sample_splitting) {
            boot_results <- bootstrap_se(Y = Y_cc, f1 = full_preds, f2 = redu_preds,
                                         type = full_type, b = b,
                                         boot_interval_type = boot_interval_type,
                                         alpha = alpha)
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
            se <- vimp_se(eif_full = eif_full, eif_reduced = eif_redu,
                          cross_fit = FALSE, sample_split = sample_splitting,
                          na.rm = na.rm)
        }
    }
    est_for_inference <- est
    predictiveness_full_for_inference <- predictiveness_full
    predictiveness_reduced_for_inference <- predictiveness_redu
    # if sample-splitting was requested and final_point_estimate isn't "split", estimate
    # the required quantities
    if (sample_splitting & (final_point_estimate != "split")) {
      if (final_point_estimate == "full") {
        est_pred_full <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc,
               a = A_cc, fitted_values = full_preds,
               full_y = Y_cc, nuisance_estimators = nuisance_estimators_full, C = C,
               Z = Z_in,
               ipc_weights = ipc_weights,
               ipc_fit_type = "SL", scale = ipc_scale,
               ipc_est_type = ipc_est_type, na.rm = na.rm,
               SL.library = SL.library), arg_lst
        ))
        est_pred_reduced <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc,
               a = A_cc, fitted_values = redu_preds,
               full_y = Y_cc, nuisance_estimators = nuisance_estimators_reduced, C = C,
               Z = Z_in,
               ipc_weights = ipc_weights,
               ipc_fit_type = "SL", scale = scale,
               ipc_est_type = ipc_est_type, na.rm = na.rm,
               SL.library = SL.library), arg_lst
        ))
        est_pred_full_lst <- estimate(est_pred_full)
        est_pred_reduced_lst <- estimate(est_pred_reduced)
        # compute the point estimates of predictiveness and variable importance
        predictiveness_full <- est_pred_full_lst$point_est
        predictiveness_redu <- est_pred_reduced_lst$point_est
        est <- predictiveness_full - predictiveness_redu
      } else {
        est_pred_full <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[ss_folds_full == 2, , drop = FALSE],
               a = A_cc[ss_folds_full == 2], fitted_values = full_preds[ss_folds_full == 2],
               full_y = Y_cc, nuisance_estimators = lapply(nuisance_estimators_full, function(l) {
                 l[ss_folds_full == 2]
               }), C = C[sample_splitting_folds == 2],
               Z = Z_in[sample_splitting_folds == 2, , drop = FALSE],
               ipc_weights = ipc_weights[sample_splitting_folds == 2],
               ipc_fit_type = "SL", scale = ipc_scale,
               ipc_est_type = ipc_est_type, na.rm = na.rm,
               SL.library = SL.library), arg_lst
        ))
        est_pred_reduced <- do.call(predictiveness_measure, c(
          list(type = full_type, y = Y_cc[ss_folds_redu == 1, , drop = FALSE],
               a = A_cc[ss_folds_redu == 1], fitted_values = redu_preds[ss_folds_redu == 1],
               full_y = Y_cc, nuisance_estimators = lapply(nuisance_estimators_reduced, function(l) {
                 l[ss_folds_redu == 1]
               }), C = C[sample_splitting_folds == 1],
               Z = Z_in[sample_splitting_folds == 1, , drop = FALSE],
               ipc_weights = ipc_weights[sample_splitting_folds == 1],
               ipc_fit_type = "SL", scale = ipc_scale,
               ipc_est_type = ipc_est_type, na.rm = na.rm,
               SL.library = SL.library), arg_lst
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

    # compute the confidence intervals
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
    if (full_type == "anova" || full_type == "regression" || !sample_splitting) {
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
        s = chr_indx, est = est, se = se, cil = ci[1], ciu = ci[2],
        test = hyp_test$test, p_value = hyp_test$p_value
    )
    if (full_type == "anova") {
        final_eif <- eif
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
                 eif_full = eif_full,
                 eif_reduced = eif_redu,
                 se = se, ci = ci,
                 est_for_inference = est_for_inference,
                 predictiveness_full = predictiveness_full,
                 predictiveness_reduced = predictiveness_redu,
                 predictiveness_full_for_inference = predictiveness_full_for_inference,
                 predictiveness_reduced_for_inference = predictiveness_reduced_for_inference,
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
                 ipc_scale = ipc_scale,
                 scale = scale,
                 mat = mat)

    # make it also a vim and vim_type object
    tmp.cls <- class(output)
    class(output) <- c("vim", full_type, tmp.cls)
    return(output)
}
