#' Compute bootstrap-based standard error estimates for variable importance
#'
#' @inheritParams vim
#'
#' @return a bootstrap-based standard error estimate
#'
#' @importFrom boot boot boot.ci
#' @export
bootstrap_se <- function(Y = NULL, f1 = NULL, f2 = NULL,
                         cluster_id = NULL, clustered = FALSE,
                         type = "r_squared", b = 1000,
                         boot_interval_type = "perc", alpha = 0.05) {
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

  if (!clustered){
    bootstrapped_ests <- boot::boot(data = data.frame(y = as.numeric(Y), f1 = f1, f2 = f2),
                                    statistic = vim_boot_stat, R = b,
                                    sim = "ordinary", stype = "i",
                                    type = type)
  } else{
    single_boot_rep <- function(data){
      n_clust <- length(unique(data$cluster_id))
      sampled_clusts <- sample(unique(data$cluster_id),
                               size = n_clust,
                               replace = TRUE)
      sampled_rows <- which(data$cluster_id %in% sampled_clusts)
      return(vim_boot_stat(data = data,
                           indices = sampled_rows,
                           type = type))
    }
    .dat <- data.frame(y = as.numeric(Y),
                      f1 = f1,
                      f2 = f2,
                      cluster_id = cluster_id)
    b_boot_ests <- t(replicate(b, single_boot_rep(data = .dat)))
    overall_est <- vim_boot_stat(data = .dat, indices = 1:nrow(.dat), type = type)
    # mimic the structure of "boot" object so that the boot.ci
    # function can be used for CIs
    bootstrapped_ests <- list(t0 = overall_est, t = b_boot_ests, R = b)
  }

  vars <- apply(bootstrapped_ests$t, 2, function(x) mean( (x - mean( x ) ) ^ 2) )
  ci_init <- boot::boot.ci(bootstrapped_ests, type = boot_interval_type,
                           conf = 1 - alpha)[[4]]
  num_ci_cols <- length(ci_init)
  ci <- ci_init[, c(num_ci_cols - 1, num_ci_cols)]
  list(se = sqrt(vars[1]), se_full = sqrt(vars[2]), se_reduced = sqrt(vars[3]), ci = ci)
}
