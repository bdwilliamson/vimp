#' Perform a hypothesis test against the null hypothesis of zero importance
#'
#' Perform a hypothesis test against the null hypothesis of zero importance by:
#' (i) for a user-specified level \eqn{\alpha}, compute a \eqn{(1 - \alpha)\times 100}\% confidence interval around the predictiveness for both the full and reduced regression functions (these must be estimated on independent splits of the data);
#' (ii) if the intervals do not overlap, reject the null hypothesis.
#'
#' @param full either (i) fitted values from a regression of the outcome on the full set of covariates from a first independent split of the data (if \code{cv = FALSE}) or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the predicted values from the full regression on the reduced set of covariates; either (i) a single set of predictions (if \code{cv = FALSE}) fit on an independent split of the data from \code{full} or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param y the outcome.
#' @param folds the folds used for splitting; assumed to be 1 for the full regression and 2 for the reduced regression (if V = 2).
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which parameter are you estimating (defaults to \code{r_squared}, for difference in R-squared-based variable importance)?
#' @param alpha the desired type I error rate (defaults to 0.05).
#' @param cv was V-fold cross-validation used to estimate the predictiveness (\code{TRUE}) or was the sample split in two (\code{FALSE}); defaults to \code{FALSE}.
#' @param scale scale to compute CI on ("identity" for identity scale, "logit" for logit scale and back-transform)
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return \code{TRUE} if the null hypothesis is rejected (i.e., if the confidence intervals do not overlap); otherwise, \code{FALSE}.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_hypothesis_test <- function(full, reduced, y, folds, weights = rep(1, length(y)), type = "r_squared", alpha = 0.05, cv = FALSE, scale = "logit", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    ## calculate the necessary pieces for the influence curve
    if (full_type == "regression" | full_type == "anova") {
        hyp_test <- NA
    } else {
        if (!cv) {
            fold_nums <- unique(folds)[order(unique(folds))]
            ## point estimates of the risk
            predictiveness_full <- predictiveness_point_est(fitted_values = full, y = y[folds == fold_nums[1]], type = type, na.rm = na.rm)
            predictiveness_redu <- predictiveness_point_est(fitted_values = reduced, y = y[folds == fold_nums[2]], type = type, na.rm = na.rm)

            ## influence curve estimates for the risk, and corresponding ses
            ic_full <- predictiveness_update(fitted_values = full, y = y[folds == fold_nums[1]], weights = weights[folds == fold_nums[1]], type = type, na.rm = na.rm)
            ic_redu <- predictiveness_update(fitted_values = reduced, y = y[folds == fold_nums[2]], weights = weights[folds == fold_nums[2]], type = type, na.rm = na.rm)
            se_full <- predictiveness_se(predictiveness_full, ic_full, na.rm = na.rm)
            se_redu <- predictiveness_se(predictiveness_redu, ic_redu, na.rm = na.rm)
            ## CIs for both predictivenesses
            predictiveness_ci_full <- predictiveness_ci(est = predictiveness_full, se = predictiveness_se(predictiveness_full, ic_full, na.rm = na.rm), level = 1 - alpha)
            predictiveness_ci_redu <- predictiveness_ci(est = predictiveness_redu, se = predictiveness_se(predictiveness_redu, ic_redu, na.rm = na.rm), level = 1 - alpha)

            ## hypothesis test (check that lower bound of full is bigger than upper bound of reduced)
            ## (since measures are R^2 [bigger = better], auc [bigger = better], accuracy [bigger = better])
            ## to get a p-value, apply the CIs to a range of levels; p-value is the largest at which we would still reject
            levels <- seq(0.001, 1 - 0.001, 0.001)
            predictiveness_cis_full <- t(apply(matrix(1 - levels), 1, function(x) predictiveness_ci(est = predictiveness_full, se = se_full, level = x)))
            predictiveness_cis_redu <- t(apply(matrix(1 - levels), 1, function(x) predictiveness_ci(est = predictiveness_redu, se = se_redu, level = x)))
            hyp_tests <- predictiveness_cis_full[, 1] > predictiveness_cis_redu[, 2]
            if (all(hyp_tests, na.rm = na.rm)) {
                p_value <- 0.001
            } else if (!any(hyp_tests, na.rm = na.rm)) {
                p_value <- 1
            } else {
                p_value <- min(levels[hyp_tests], na.rm = na.rm)
            }
            hyp_test <- p_value < alpha

        } else { ## V-fold CV; compute a p-value comparing predictiveness on each fold v = 1,...,V with reduced ones, average
                 ## testing decision is based on average p-value
            V <- length(unique(folds))
            p_values <- vector("list", length = V)

            ## all point estimates
            predictiveness_full_lst <- cv_predictiveness_point_est(full, y, folds = folds, type = full_type, na.rm = na.rm)
            predictiveness_redu_lst <- cv_predictiveness_point_est(reduced, y, folds = folds, type = full_type, na.rm = na.rm)
            predictiveness_full <- predictiveness_full_lst$point_est
            predictiveness_redu <- predictiveness_redu_lst$point_est
            predictiveness_fulls <- predictiveness_full_lst$all_ests
            predictiveness_redus <- predictiveness_redu_lst$all_ests
            ## all ics
            ic_full_lst <- cv_predictiveness_update(full, y, folds = folds, type = full_type, na.rm = na.rm)
            ic_redu_lst <- cv_predictiveness_update(reduced, y, folds = folds, type = full_type, na.rm = na.rm)
            ic_full <- ic_full_lst$ic
            ic_redu <- ic_redu_lst$ic
            ics_full <- ic_full_lst$all_ics
            ics_redu <- ic_redu_lst$all_ics
            ## compute CI, hypothesis test for each fold
            for (v in 1:V) {
                se_full <- predictiveness_se(predictiveness_fulls[v], ics_full[, v], na.rm = na.rm)
                if (is.na(se_full)) stop("Estimated standard error is NA. Consider re-running with na.rm = TRUE.")
                ses_redu <- sapply(1:length(seq_len(V)[-v]), function(x) predictiveness_se(predictiveness_redus[x], ics_redu[, x], na.rm = na.rm))
                ## compute a p-value
                levels <- seq(0.001, 1 - 0.001, 0.001)
                ## get the lower limit of the full predictiveness CI; vector of length(levels)
                predictiveness_cis_ll_full <- apply(matrix(1 - levels), 1, function(x) predictiveness_ci(est = predictiveness_full, se = se_full, level = x)[, 1])
                ## get the upper limit of the reduced predictiveness CI for each fold; dimension length(levels) by V - 1
                predictiveness_cis_ul_redu <- t(apply(matrix(1 - levels), 1, function(x) sapply(1:(V - 1), function(v) predictiveness_ci(est = predictiveness_redus[v], se = ses_redu[v],
                                                                                                                level = x)[, 2])))
                ## compare lower limit of full predictiveness CI to all upper limits of reduced predictiveness cis
                hyp_tests <- t(apply(matrix(1:dim(predictiveness_cis_ul_redu)[1]), 1, function(x) predictiveness_cis_ll_full[x] > predictiveness_cis_ul_redu[x, ]))
                ## compute a p-value for each fold
                check_fold_hyp_tests <- function(fold_hyp_tests) {
                    if (all(fold_hyp_tests, na.rm = na.rm)) {
                        fold_p_value <- 0.001
                    } else if (!any(fold_hyp_tests, na.rm = na.rm)) {
                        fold_p_value <- 1
                    } else {
                    fold_p_value <- min(levels[fold_hyp_tests], na.rm = na.rm)
                    }
                    return(fold_p_value)
                }
                ## save off the p-value
                p_values[[v]] <- mean(apply(hyp_tests, 2, check_fold_hyp_tests))
            }
            ## average the p-values, make a testing decision
            p_value <- mean(unlist(p_values))
            hyp_test <- p_value < alpha
            ## compute cis
            predictiveness_ci_full <- predictiveness_ci(predictiveness_full, predictiveness_se(predictiveness_full, ic_full, na.rm = na.rm), level = 1 - alpha)
            predictiveness_ci_redu <- predictiveness_ci(predictiveness_redu, predictiveness_se(predictiveness_redu, ic_redu, na.rm = na.rm), level = 1 - alpha)
        }
    }
    return(list(test = hyp_test, p_value = p_value, predictiveness_full = predictiveness_full, predictiveness_reduced = predictiveness_redu,
              predictiveness_ci_full = predictiveness_ci_full, predictiveness_ci_reduced = predictiveness_ci_redu))
}
