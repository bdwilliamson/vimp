#' Compute bootstrap-based standard error estimates for variable importance
#' 
#' @inheritParams vim
#' @param b the number of bootstrap replicates (defaults to 1000)
#' 
#' @return a bootstrap-based standard error estimate
#' 
#' @export
bootstrap_se <- function(Y = NULL, f1 = NULL, f2 = NULL, 
                          type = "r_squared", b = 1000) {
  vim_boot_stat <- function(data, indices, type) {
    y <- data$y[indices]
    f1 <- data$f1[indices]
    f2 <- data$f2[indices]
    predictiveness_full <- est_predictiveness(
      fitted_values = f1, y = y, full_y = y, type = type
    )$point_est
    predictiveness_reduced <- est_predictiveness(
      fitted_values = f2, y = y, full_y = y, type = type
    )$point_est
    est_vim <- predictiveness_full - predictiveness_reduced
    c(vim = est_vim, pred_full = predictiveness_full, pred_redu = predictiveness_reduced)
  }
  bootstrapped_ests <- boot::boot(data = data.frame(y = as.numeric(Y), f1 = f1, f2 = f2),
                                  statistic = vim_boot_stat, R = b, 
                                  sim = "ordinary", stype = "i", 
                                  type = type)
  vars <- apply(bootstrapped_ests$t, 2, function(x) mean( (x - mean( x ) ) ^ 2) )
  list(se = sqrt(vars[1]), se_full = sqrt(vars[2]), se_reduced = sqrt(vars[3]))
}