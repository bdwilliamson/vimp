#' Influence function estimates for SPVIMs
#'
#' Compute the influence functions for the contribution from sampling observations and subsets.
#'
#' @param Z the matrix of presence/absence of each feature (columns) in each sampled subset (rows)
#' @param z_counts the number of times each unique subset was sampled
#' @param W the matrix of weights
#' @param v the estimated predictiveness measures
#' @param psi the estimated SPVIM values
#' @param G the constraint matrix
#' @param c_n the constraint values
#' @param ics a list of influence function values for each predictiveness measure
#' @param measure the type of measure (e.g., "r_squared" or "auc")
#'
#' @return a named list of length 2; \code{contrib_v} is the contribution from estimating V, while \code{contrib_s} is the contribution from sampling subsets.
#'
#' @details The processes for sampling observations and sampling subsets are independent. Thus, we can compute the influence function separately for each sampling process. For further details, see the paper by Williamson and Feng (2020).
#'
#' @export
spvim_ics <- function(Z, z_counts, W, v, psi, G, c_n, ics, measure) {
  # compute contribution from estimating V
  Z_W <- t(Z) %*% W
  A_m <- Z_W %*% Z
  A_m_inv <- MASS::ginv(A_m)
  prefix <- A_m_inv %*% Z_W
  ic_mat <- t(as.matrix(data.frame(ics)))
  phi_01 <- prefix %*% ic_mat 

  # compute contribution from sampling S
  qr_decomp <- qr(t(G))
  U_2 <- qr.Q(qr_decomp, complete = TRUE)[, 3:ncol(Z), drop = FALSE]
  V <- t(U_2) %*% (t(Z) %*% W %*% Z) %*% U_2
  phi_02_shared_mat <- (-1) * U_2 %*% MASS::ginv(V)
  phi_02_uniq_vectors <- matrix(NA, nrow = nrow(Z), ncol = ncol(U_2))
  for (z in 1:nrow(Z)) {
    phi_02_uniq_vectors[z, ] <- as.vector(Z[z, , drop = FALSE] %*% psi - v[z]) * as.vector(t(U_2) %*% t(Z[z, , drop = FALSE]))
  }
  phi_02_uniq <- phi_02_shared_mat %*% t(phi_02_uniq_vectors)
  phi_02_uniq_lst <- split(phi_02_uniq, rep(1:ncol(phi_02_uniq), each = nrow(phi_02_uniq)))
  phi_02_rep_lst <- sapply(1:length(phi_02_uniq_lst), function(s) replicate(z_counts[s], phi_02_uniq_lst[[s]]))
  phi_02 <- do.call(cbind, phi_02_rep_lst)
  return(list(contrib_v = phi_01, contrib_s = phi_02))
}
