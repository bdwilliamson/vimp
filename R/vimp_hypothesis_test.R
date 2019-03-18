#' Perform a hypothesis test against the null hypothesis of zero importance
#'
#' Perform a hypothesis test against the null hypothesis of zero importance by: 
#' (i) for a user-specified level $\alpha$, compute a $(1 - \alpha)\times 100$\% confidence interval around the risk for both the full and reduced regression functions;
#' (ii) if the intervals do not overlap, reject the null hypothesis.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param level the desired type I error rate (defaults to 0.05).
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return \code{TRUE} if the null hypothesis is rejected (i.e., if the confidence intervals do not overlap); otherwise, \code{FALSE}.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_hypothesis_test <- function(full, reduced, y, type = "r_squared", level = 0.05, na.rm = FALSE) {
  
  ## calculate the necessary pieces for the influence curve
  if (type == "regression" | type == "anova") {
    hyp_test <- NA
  } else {
    ## point estimates of the risk
    risk_full <- risk_estimator(full, y, type, na.rm)
    risk_reduced <- risk_estimator(reduced, y, type, na.rm)

    ## influence curve estimates for the risk
    ic_full <- risk_update(full, y, type, level, na.rm)
    ic_reduced <- risk_update(reduced, y, type, level, na.rm)

    ## CIs for both risks
    risk_ci_full <- vimp_ci(est = risk_full, se = vimp_se(ic_full, na.rm = na.rm), level = level)
    risk_ci_reduced <- vimp_ci(est = risk_reduced, se = vimp_se(ic_reduced, na.rm = na.rm), level = level)
    ## hypothesis test (check that lower bound of full is bigger than upper bound of reduced)
    ## (since measures are R^2 [bigger = better], auc [bigger = better], accuracy [bigger = better])
    hyp_test <- risk_ci_full[1] > risk_ci_reduced[2]
  } 
  
  return(hyp_test)
}
