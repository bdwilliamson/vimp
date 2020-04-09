#' Perform a hypothesis test against the null hypothesis of \eqn{\delta} importance
#'
#' Perform a hypothesis test against the null hypothesis of zero importance by:
#' (i) for a user-specified level \eqn{\alpha}, compute a \eqn{(1 - \alpha)\times 100}\% confidence interval around the predictiveness for both the full and reduced regression functions (these must be estimated on independent splits of the data);
#' (ii) if the intervals do not overlap, reject the null hypothesis.
#'
#' @param full either (i) fitted values from a regression of the outcome on the full set of covariates from a first independent split of the data (if \code{cv = FALSE}) or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the predicted values from the full regression on the reduced set of covariates; either (i) a single set of predictions (if \code{cv = FALSE}) fit on an independent split of the data from \code{full} or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param y the outcome.
#' @param folds the folds used for splitting. If \code{cv = FALSE}, assumed to be a vector with 1 for the full regression and 2 for the reduced regression (if V = 2). If \code{cv = TRUE}, assumed to be a list with first element the outer folds (for hypothesis testing) and second element a list with the inner cross-validation folds.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
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
#' @importFrom stats pnorm
#'
#' @export
vimp_hypothesis_test <- function(full, reduced, y, folds, delta = 0, weights = rep(1, length(y)), type = "r_squared", alpha = 0.05, cv = FALSE, scale = "identity", na.rm = FALSE) {

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

        } else {
            ## all point estimates
            predictiveness_full_lst <- cv_predictiveness_point_est(full, y[folds[[1]] == 1, , drop = FALSE], folds = folds[[2]][[1]], type = full_type, na.rm = na.rm)
            predictiveness_redu_lst <- cv_predictiveness_point_est(reduced, y[folds[[1]] == 2, , drop = FALSE], folds = folds[[2]][[2]], type = full_type, na.rm = na.rm)
            predictiveness_fulls <- predictiveness_full_lst$all_ests
            predictiveness_redus <- predictiveness_redu_lst$all_ests
            ## all ics
            ic_full_lst <- cv_predictiveness_update(full, y[folds[[1]] == 1, , drop = FALSE], folds = folds[[2]][[1]], type = full_type, na.rm = na.rm)
            ic_redu_lst <- cv_predictiveness_update(reduced, y[folds[[1]] == 2, , drop = FALSE], folds = folds[[2]][[2]], type = full_type, na.rm = na.rm)
            ics_full <- ic_full_lst$all_ics
            ics_redu <- ic_redu_lst$all_ics
            ## compute point estimate, SE based on first group of folds
            predictiveness_full <- mean(predictiveness_fulls)
            ic_full <- rowMeans(ics_full)
            se_full <- predictiveness_se(predictiveness_full, ic_full, na.rm = na.rm)
            ## compute point estimate, SE based on second group of folds
            predictiveness_redu <- mean(predictiveness_redus)
            ic_redu <- rowMeans(ics_redu)
            se_redu <- predictiveness_se(predictiveness_redu, ic_redu, na.rm = na.rm)
        }
        ## hypothesis test (check that lower bound of full is bigger than upper bound of reduced)
        test_statistic <- (predictiveness_full - predictiveness_redu - delta)/(sqrt(se_full^2 + se_redu^2))
        p_value <- 1 - pnorm(test_statistic)
        hyp_test <- p_value < alpha
        ## CIs for both predictivenesses
        predictiveness_ci_full <- predictiveness_ci(est = predictiveness_full, se = predictiveness_se(predictiveness_full, ic_full, na.rm = na.rm), level = 1 - alpha)
        predictiveness_ci_redu <- predictiveness_ci(est = predictiveness_redu, se = predictiveness_se(predictiveness_redu, ic_redu, na.rm = na.rm), level = 1 - alpha)
    }
    return(list(test = hyp_test, p_value = p_value, predictiveness_full = predictiveness_full, predictiveness_reduced = predictiveness_redu,
              predictiveness_ci_full = predictiveness_ci_full, predictiveness_ci_reduced = predictiveness_ci_redu,
              se_full = se_full, se_redu = se_redu, test_statistic = test_statistic))
}
