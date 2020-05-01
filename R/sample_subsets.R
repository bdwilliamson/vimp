#' Create necessary objects for SPVIMs
#'
#' Creates the Z and W matrices and a list of sampled subsets, S, for SPVIM estimation.
#'
#' @param p the number of covariates
#' @param gamma the fraction of the sample size to sample (e.g., \code{gamma = 1} means sample \code{n} subsets)
#' @param n the sample size
#'
#' @return a list, with elements Z (the matrix encoding presence/absence of each feature in the uniquely sampled subsets), S (the list of unique sampled subsets), W (the matrix of weights), and z_counts (the number of times each subset was sampled)
#'
#' @examples
#' p <- 10
#' gamma <- 1
#' n <- 100
#' set.seed(100)
#' subset_lst <- sample_subsets(p, gamma, n)
#' @importFrom stats aggregate
#' @export
sample_subsets <- function(p, gamma, n) {
  max_subset <- 1:p
  sampling_weights <- c(1, apply(matrix(1:(p-1)), 1, function(s) choose(p - 2, s - 1) ^ (-1)), 1)
  subset_sizes <- sort(sample(0:p, size = gamma * n, replace = TRUE, prob = sampling_weights / sum(sampling_weights)))
  S_all <- apply(matrix(subset_sizes), 1, function(s) sort(sample(1:p, s, replace = FALSE)))
  Z_all <- do.call(rbind, lapply(as.list(S_all), function(s) {
    z <- rep(0, p)
    z[match(s, max_subset)] <- 1
    return(z)
  } ))
  Z_df <- data.frame(Z_all)
  Z_with_counts <- aggregate(list(n = rep(1, nrow(Z_df))), Z_df, length)
  Z <- as.matrix(Z_with_counts[, -ncol(Z_with_counts)])
  z_counts <- Z_with_counts$n
  Z_aug <- cbind(1, Z)
  S <- lapply(lapply(apply(Z, 1, list), unlist), function(z) max_subset[as.logical(z)])
  W <- diag(z_counts / sum(z_counts))
  return(list(Z = Z_aug, S = S, W = W, z_counts = z_counts))
}
