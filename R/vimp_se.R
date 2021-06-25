#' Estimate variable importance standard errors
#'
#' Compute standard error estimates for estimates of variable importance.
#'
#' @param eif_full the estimated efficient influence function (EIF) based on
#'   the full set of covariates.
#' @param eif_reduced the estimated EIF based on the reduced set of covariates.
#' @param cross_fit logical; was cross-fitting used to compute the EIFs?
#'   (defaults to \code{TRUE})
#' @param sample_split logical; was sample-splitting used? (defaults to \code{TRUE})
#' @param na.rm logical; should NA's be removed in computation?
#'   (defaults to \code{FALSE}).
#'
#' @return The standard error for the estimated variable importance for the
#'   given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#'   details on the mathematics behind this function and the definition of the
#'   parameter of interest.
#'
#' @export
vimp_se <- function(eif_full, eif_reduced, cross_fit = TRUE, sample_split = TRUE, 
                    na.rm = FALSE) {
    if (!cross_fit & !sample_split) {
        se <- sqrt( mean( (eif_full - eif_reduced) ^ 2 ) / length(eif_full) )
    } else if (cross_fit & !sample_split) {
        fold_vars <- unlist(lapply(
            as.list(seq_len(length(eif_full))), function(k) {
                mean( (eif_full[[k]] - eif_reduced[[k]]) ^ 2)
            }
        ))
        n <- sum(unlist(lapply(as.list(seq_len(length(eif_full))), function(k) {
            length(eif_full[[k]])
        })))
        se <- sqrt(mean(fold_vars) / n)
    } else if (!cross_fit & sample_split) {
        se <- sqrt( mean( (eif_full) ^ 2 ) / length(eif_full) + 
                        mean( (eif_reduced) ^ 2 ) / length(eif_reduced) )
    } else {
        n_1 <- sum(unlist(lapply(as.list(seq_len(length(eif_full))), function(k) {
            length(eif_full[[k]])
        })))
        n_2 <- sum(unlist(lapply(as.list(seq_len(length(eif_reduced))), function(k) {
            length(eif_reduced[[k]])
        })))
        full_indices <- as.list(seq_len(length(eif_full)))
        redu_indices <- as.list(seq_len(length(eif_reduced)))
        full_vars <- unlist(lapply(full_indices, function(k) {
            mean( (eif_full[[k]]) ^ 2 )
        }))
        redu_vars <- unlist(lapply(redu_indices, function(k) {
            mean( (eif_reduced[[k]]) ^ 2 )
        }))
        se <- sqrt( mean(full_vars) / n_1 + mean(redu_vars) / n_2 )
    }
    return(se)
}
