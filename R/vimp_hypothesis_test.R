#' Perform a hypothesis test against the null hypothesis of \eqn{\delta} importance
#'
#' Perform a hypothesis test against the null hypothesis of zero importance by:
#' (i) for a user-specified level \eqn{\alpha}, compute a \eqn{(1 - \alpha)\times 100}\% confidence interval around the predictiveness for both the full and reduced regression functions (these must be estimated on independent splits of the data);
#' (ii) if the intervals do not overlap, reject the null hypothesis.
#'
#' @param predictiveness_full the estimated predictiveness of the regression including the covariate(s) of interest.
#' @param predictiveness_reduced the estimated predictiveness of the regression excluding the covariate(s) of interest.
#' @param se_full the estimated standard error for the predictiveness of the regression including the covariate(s) of interest.
#' @param se_reduced the estimated standard error for the predictiveness of the regression excluding the covariate(s) of interest.
#' @param delta the value of the \eqn{\delta}-null (i.e., testing if importance < \eqn{\delta}); defaults to 0.
#' @param alpha the desired type I error rate (defaults to 0.05).
#'
#' @return a list, with: the hypothesis testing decision (\code{TRUE} if the null hypothesis is rejected, \code{FALSE} otherwise); the p-value from the hypothesis test; and the test statistic from the hypothesis test.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @importFrom stats pnorm
#'
#' @export
vimp_hypothesis_test <- function(predictiveness_full, predictiveness_reduced, se_full, se_reduced, delta = 0, alpha = 0.05) {
    # hypothesis test, based on t-statistic (requires independent splits to estimate full, reduced predictiveness)
    test_statistic <- (predictiveness_full - predictiveness_reduced - delta)/(sqrt(se_full^2 + se_reduced^2))
    p_value <- 1 - pnorm(test_statistic)
    hyp_test <- p_value < alpha
    return(list(test = hyp_test, p_value = p_value, test_statistic = test_statistic))
}
