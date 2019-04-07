#' Perform a hypothesis test against the null hypothesis of zero importance
#'
#' Perform a hypothesis test against the null hypothesis of zero importance by: 
#' (i) for a user-specified level \eqn{\alpha}, compute a \eqn{(1 - \alpha)\times 100}\% confidence interval around the risk for both the full and reduced regression functions (these must be estimated on independent splits of the data);
#' (ii) if the intervals do not overlap, reject the null hypothesis.
#'
#' @param full either (i) fitted values from a regression of the outcome on the full set of covariates from a first independent split of the data (if \code{cv = FALSE}) or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the predicted values from the full regression on the reduced set of covariates; either (i) a single set of predictions (if \code{cv = FALSE}) fit on an independent split of the data from \code{full} or (ii) a list of predicted values from a cross-validated procedure (if \code{cv = TRUE}).
#' @param y the outcome.
#' @param folds the folds used for splitting; assumed to be 1 for the full regression and 2 for the reduced regression (if V = 2).
#' @param V the number of folds used, defaults to 2.
#' @param type which parameter are you estimating (defaults to \code{r_squared}, for difference in R-squared-based variable importance)?
#' @param level the desired type I error rate (defaults to 0.05).
#' @param cv was V-fold cross-validation used to estimate the risks (\code{TRUE}) or was the sample split in two (\code{FALSE}); defaults to \code{FALSE}.
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return \code{TRUE} if the null hypothesis is rejected (i.e., if the confidence intervals do not overlap); otherwise, \code{FALSE}.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_hypothesis_test <- function(full, reduced, y, folds, type = "r_squared", level = 0.05, cv = FALSE, na.rm = FALSE) {
  
  ## calculate the necessary pieces for the influence curve
  if (type == "regression" | type == "anova") {
    hyp_test <- NA
  } else {
    if (!cv) {
        fold_nums <- unique(folds)
        ## point estimates of the risk
        risk_full <- risk_estimator(fitted_values = full, y = y[folds == fold_nums[1]], type = type, na.rm = na.rm)
        risk_reduced <- risk_estimator(fitted_values = reduced, y = y[folds == fold_nums[2]], type = type, na.rm = na.rm)

        ## influence curve estimates for the risk
        ic_full <- risk_update(fitted_values = full, y = y[folds == fold_nums[1]], type = type, na.rm = na.rm)
        ic_reduced <- risk_update(fitted_values = reduced, y = y[folds == fold_nums[2]], type = type, na.rm = na.rm)

        ## CIs for both risks
        risk_ci_full <- vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = 1 - level)
        risk_ci_reduced <- vimp_ci(est = risk_reduced, se = vimp_se(ic_reduced, na.rm = na.rm), level = 1 - level)
        ## hypothesis test (check that lower bound of full is bigger than upper bound of reduced)
        ## (since measures are R^2 [bigger = better], auc [bigger = better], accuracy [bigger = better])
        hyp_test <- risk_ci_full[1] > risk_ci_reduced[2]

        ## to get a p-value, apply the CIs to a range of levels; p-value is the largest at which we would still reject
        levels <- seq(0.0001, 1 - 0.0001, 0.0001)
        risk_cis_full <- t(apply(matrix(1 - levels), 1, function(x) vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = x)))
        risk_cis_redu <- t(apply(matrix(1 - levels), 1, function(x) vimp_ci(est = risk_reduced, se = vimp_se(ic_reduced, na.rm = na.rm), level = x)))
        hyp_tests <- risk_cis_full[, 1] > risk_cis_redu[, 2]
        p_value <- ifelse(all(hyp_tests), 0.0001, min(levels[hyp_tests]))        
        
    } else { ## V-fold CV, reject iff ALL pairwise comparisons have no overlapping CI
        V <- length(unique(folds))
        hyp_tests <- vector("logical", V)
        p_values <- vector("numeric", V)
        risks_full <- vector("numeric", V)
        risks_reduced <- vector("numeric", V)
        for (v in 1:V) {
            other_folds <- (1:V)[-v]
            ## compute the full risk on fold v
            risk_full <- risk_estimator(fitted_values = full[[v]], y = y[folds == v], type = type, na.rm = na.rm)
            ic_full <- risk_update(fitted_values = full[[v]], y = y[folds == v], type = type, na.rm = na.rm)
            risk_ci_full <- vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = (1 - level/V))
            ## compute the reduced risk on all other folds
            risks_red <- mapply(function(x, z, y, folds, type, na.rm) risk_estimator(fitted_values = x, y = y[folds == z], type = type, na.rm = na.rm), 
                                reduced[-v], as.list(other_folds), MoreArgs = list(y = y, folds = folds, type = type, na.rm = na.rm), SIMPLIFY = FALSE)
            ics_red <- mapply(function(x, z, y, folds, type, na.rm) risk_update(fitted_values = x, y = y[folds == z], type = type, na.rm = na.rm), 
                                reduced[-v], as.list(other_folds), MoreArgs = list(y = y, folds = folds, type = type, na.rm = na.rm), SIMPLIFY = FALSE)
            risk_cis_red <- mapply(function(est, se, level) vimp_ci(est = est, se = se, level = level),
                                   est = risks_red, se = lapply(ics_red, vimp_se, na.rm = na.rm),
                                   MoreArgs = list(level = (1 - level/V)), SIMPLIFY = "matrix")
            ## compute the value of the hypothesis test (reject or not reject)
            hyp_tests[v] <- all(risk_ci_full[1] > risk_cis_red[2, ])
            ## save off the risks
            risks_full[v] <- risk_full
            risks_reduced[v] <- mean(unlist(risks_red))
            ## get the p-value
            levels <- seq(0.0001, 1 - 0.0001, 0.0001)
            risk_cis_full <- t(apply(matrix(1 - levels/V), 1, function(x) vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = x)))
            risks_cis_red <- t(apply(matrix(1 - levels/V), 1, function(x) mapply(function(est, se, level) vimp_ci(est = est, se = se, level = level),
                                                                                 est = risks_red, se = lapply(ics_red, vimp_se, na.rm = na.rm),
                                                                                 MoreArgs = list(level = x), SIMPLIFY = "matrix")))
            many_hyp_tests <- apply((risk_cis_full[, 1] > risks_cis_red[, c(FALSE, TRUE)]), 1, all)
            p_values[v] <- ifelse(all(many_hyp_tests), 0.0001, min(levels[many_hyp_tests]))        
        }
        hyp_test <- all(hyp_tests)
        p_value <- max(p_values)
        risk_full <- mean(risks_full)
        risk_reduced <- mean(risks_reduced)
    }
  } 
  return(list(test = hyp_test, p_value = p_value, risk_full = risk_full, risk_reduced = risk_reduced))
}
