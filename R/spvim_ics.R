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
#' @param ics a matrix of influence function values for each predictiveness measure
#' @param measure the type of measure (e.g., "r_squared" or "auc")
#' 
#' @return a named list of length 2; \code{contrib_v} is the contribution from estimating V, while \code{contrib_s} is the contribution from sampling subsets.
#' 
#' @details 
#' 
#' @export
spvim_ics <- function(Z, z_counts, W, v, psi, G, c_n, ics, measure) {
  # compute contribution from estimating V
  Z_W <- t(Z) %*% W
  A_m <- Z_W %*% Z
  A_m_inv <- solve(A_m) 
  phi_01 <- A_m_inv %*% Z_W %*% ics
  
  # compute contribution from sampling S
  qr_decomp <- qr(t(G))
  U_2 <- qr.Q(qr_decomp, complete = TRUE)[, 3:ncol(Z)]
  V <- t(U_2) %*% (t(Z) %*% W %*% Z) %*% U_2
  phi_02_shared_mat <- (-1) * U_2 %*% solve(V)
  phi_02_uniq_vectors <- "fill this in"
  phi_02_uniq <- phi_02_shared_mat %*% phi_02_uniq_vectors
  phi_02 <- rep(phi_02_uniq, each = z_counts)
  return(list(contrib_v = phi_01, contrib_s = phi_02))
}