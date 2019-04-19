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
#' @param alpha the desired type I error rate (defaults to 0.05).
#' @param cv was V-fold cross-validation used to estimate the risks (\code{TRUE}) or was the sample split in two (\code{FALSE}); defaults to \code{FALSE}.
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return \code{TRUE} if the null hypothesis is rejected (i.e., if the confidence intervals do not overlap); otherwise, \code{FALSE}.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_hypothesis_test <- function(full, reduced, y, folds, type = "r_squared", alpha = 0.05, cv = FALSE, na.rm = FALSE) {
  
  ## calculate the necessary pieces for the influence curve
  if (type == "regression" | type == "anova") {
    hyp_test <- NA
  } else {
    if (!cv) {
        fold_nums <- unique(folds)
        ## point estimates of the risk
        risk_full <- risk_estimator(fitted_values = full, y = y[folds == fold_nums[1]], type = type, na.rm = na.rm)
        risk_reduced <- risk_estimator(fitted_values = reduced, y = y[folds == fold_nums[2]], type = type, na.rm = na.rm)

        ## influence curve estimates for the risk, and corresponding ses
        ic_full <- risk_update(fitted_values = full, y = y[folds == fold_nums[1]], type = type, na.rm = na.rm)
        ic_reduced <- risk_update(fitted_values = reduced, y = y[folds == fold_nums[2]], type = type, na.rm = na.rm)
        se_full <- vimp_se(ic_full, na.rm = na.rm)
        se_reduced <- vimp_se(ic_full, na.rm = na.rm)
        ## CIs for both risks
        risk_ci_full <- vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = 1 - alpha)
        risk_ci_reduced <- vimp_ci(est = risk_reduced, se = vimp_se(ic_reduced, na.rm = na.rm), level = 1 - alpha)
        
        ## hypothesis test (check that lower bound of full is bigger than upper bound of reduced)
        ## (since measures are R^2 [bigger = better], auc [bigger = better], accuracy [bigger = better])
        hyp_test <- risk_ci_full[1] > risk_ci_reduced[2]
        
        ## to get a p-value, apply the CIs to a range of levels; p-value is the largest at which we would still reject
        levels <- seq(0.0001, 1 - 0.0001, 0.0001)
        risk_cis_full <- t(apply(matrix(1 - levels), 1, function(x) vimp_ci(est = risk_full, se = risk_se_full, level = x)))
        risk_cis_redu <- t(apply(matrix(1 - levels), 1, function(x) vimp_ci(est = risk_reduced, se = risk_se_reduced, level = x)))
        hyp_tests <- risk_cis_full[, 1] > risk_cis_redu[, 2]
        if (all(hyp_tests)) {
          p_value <- 0.0001
        } else if (!any(hyp_tests)) {
          p_value <- 1
        } else {
          p_value <- min(levels[hyp_tests])
        } 
        
    } else { ## V-fold CV; compute a p-value comparing risk on each fold v = 1,...,V with reduced ones, average
             ## testing decision is based on average p-value
        V <- length(unique(folds))
        p_values <- vector("list", length = V)
        risks_full <- vector("numeric", length = V)
        risks_reduced <- vector("numeric", length = V)
        risk_vars_full <- vector("numeric", length = V)
        risk_vars_reduced <- vector("numeric", length = V)
        for (v in 1:V) {
          ## compute full risk on fold v, reduced risks on folds -v; also variances
          risk_full <- risk_estimator(fitted_values = full[[v]], y = y[folds == v], type = type, na.rm = na.rm)
          risk_var_full <- mean(risk_update(fitted_values = full[[v]], y = y[folds == v], type = type, 
                                            na.rm = na.rm)^2)
          risk_se_full <- sqrt(risk_var_full/length(y[folds == v]))
          risks_reduced <- sapply(seq_len(V)[-v], function(x) risk_estimator(fitted_values = reduced[[x]],
                                                                             y = y[folds == x], type = type,
                                                                             na.rm = na.rm))
          risk_vars_reduced <- sapply(seq_len(V)[-v], function(x) mean(risk_update(fitted_values = reduced[[x]],
                                                                                 y = y[folds == x], type = type,
                                                                                 na.rm = na.rm)^2), simplify = FALSE)
          risk_ses_reduced <- sapply(1:length(seq_len(V)[-v]), function(x) sqrt(risk_vars_reduced[[x]]/length(y[folds == x])))

          ## compute a p-value
          levels <- seq(0.0001, 1 - 0.0001, 0.0001)
          ## get the lower limit of the full risk CI; vector of length(levels)
          risk_cis_ll_full <- apply(matrix(1 - levels), 1, function(x) vimp_ci(est = risk_full, se = risk_se_full, level = x)[, 1])
          ## get the upper limit of the reduced risk CI for each fold; dimension length(levels) by V - 1 
          risk_cis_ul_redu <- t(apply(matrix(1 - levels), 1, function(x) sapply(1:(V - 1), function(v) vimp_ci(est = risks_reduced[v], se = risk_ses_reduced[v],
                                                                                                            level = x)[, 2])))
          ## compare lower limit of full risk CI to all upper limits of reduced risk cis
          hyp_tests <- t(apply(matrix(1:dim(risk_cis_ul_redu)[1]), 1, function(x) risk_cis_ll_full[x] > risk_cis_ul_redu[x, ]))
          ## compute a p-value for each fold
          check_fold_hyp_tests <- function(fold_hyp_tests) {
            if (all(fold_hyp_tests)) {
              fold_p_value <- 0.0001
            } else if (!any(fold_hyp_tests)) {
              fold_p_value <- 1
            } else {
              fold_p_value <- min(levels[fold_hyp_tests])
            }
            return(fold_p_value)
          }
          ## save off the p-value
          p_values[[v]] <- mean(apply(hyp_tests, 2, check_fold_hyp_tests))
          ## save off the risks and the variances
          risks_full[v] <- risk_full
          risk_vars_full[v] <- risk_var_full
          risks_reduced[v] <- risk_estimator(fitted_values = reduced[[v]], y = y[folds == v], type = type, na.rm = na.rm)
          risk_vars_reduced[v] <- mean(risk_update(fitted_values = reduced[[v]], y = y[folds == v], type = type, 
                                                   na.rm = na.rm)^2)
        }
        ## average the p-values, make a testing decision
        p_value <- mean(unlist(p_values))
        hyp_test <- p_value < alpha
        ## average the full and reduced risks, get a CI
        risk_full <- mean(risks_full)
        risk_reduced <- mean(risks_reduced)
        risk_ci_full <- vimp_ci(est = risk_full, se = sqrt(mean(risk_vars_full)/length(y)), level = 1 - alpha)
        risk_ci_reduced <- vimp_ci(est = risk_reduced, se = sqrt(mean(risk_vars_reduced)/length(y)), level = 1 - alpha)
        # ## sample the folds to compute full and reduced risk
        # folds_full <- sample(seq_len(V), ceiling(V/2))
        # folds_reduced <- seq_len(V)[-folds_full]
        # 
        # ## get full risk, vars
        # risks_full <- sapply(folds_full, function(x) risk_estimator(fitted_values = full[[x]],
        #                                                             y = y[folds == x],
        #                                                             type = type, na.rm = na.rm))
        # risk_vars_full <- sapply(folds_full, function(x) mean(risk_update(full[[x]], 
        #                                                                   y[folds == x], 
        #                                                                   type = type, na.rm = na.rm)^2))
        # risk_full_n <- sum(sapply(folds_full, function(x) length(y[folds == x])))
        # risk_full <- mean(risks_full)
        # risk_se_full <- sqrt(mean(risk_vars_full/risk_full_n))
        # risk_ci_full <- vimp_ci(est = risk_full, se = risk_se_full, level = 1 - alpha)
        # 
        # ## get full risk, vars
        # risks_reduced <- sapply(folds_reduced, function(x) risk_estimator(fitted_values = reduced[[x]],
        #                                                             y = y[folds == x],
        #                                                             type = type, na.rm = na.rm))
        # risk_vars_reduced <- sapply(folds_reduced, function(x) mean(risk_update(reduced[[x]], 
        #                                                                   y[folds == x], 
        #                                                                   type = type, na.rm = na.rm)^2))
        # risk_reduced_n <- sum(sapply(folds_reduced, function(x) length(y[folds == x])))
        # risk_reduced <- mean(risks_reduced)
        # risk_se_reduced <- sqrt(mean(risk_vars_reduced/risk_reduced_n))
        # risk_ci_reduced <- vimp_ci(est = risk_reduced, se = risk_se_reduced, level = 1 - alpha)
    }
  }
  return(list(test = hyp_test, p_value = p_value, risk_full = risk_full, risk_reduced = risk_reduced,
              risk_ci_full = risk_ci_full, risk_ci_reduced = risk_ci_reduced))
}
