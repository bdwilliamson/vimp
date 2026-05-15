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
#' @param use_square logical; should we estimate the variance using the square of the EIF (\code{TRUE}) or using the cross product (\code{FALSE}, the default)
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
                    na.rm = FALSE, use_square = FALSE) {
    if (!cross_fit & !sample_split) {
      if (isTRUE(use_square)) {
        se <- sqrt( mean( (eif_full - eif_reduced) ^ 2 , na.rm = na.rm) / length(eif_full) )
      } else {
        var <- ifelse(na.rm, crossprod(na.omit(matrix(eif_full - eif_reduced))), crossprod(matrix(eif_full - eif_reduced)))
        se <- as.numeric(sqrt(var)) / length(eif_full)
      }
    } else if (cross_fit & !sample_split) {
      if (isTRUE(use_square)) {
        fold_vars <- unlist(lapply(
          as.list(seq_len(length(eif_full))), function(k) {
            mean( (eif_full[[k]] - eif_reduced[[k]]) ^ 2)
          }
        ))
      } else {
        fold_vars <- unlist(lapply(
          as.list(seq_len(length(eif_full))), function(k) {
            if (isTRUE(na.rm)) {
              as.numeric(crossprod(na.omit(matrix(eif_full[[k]] - eif_reduced[[k]])))) / length(eif_full[[k]])
            } else {
              as.numeric(crossprod(matrix(eif_full[[k]] - eif_reduced[[k]]))) / length(eif_full[[k]])
            }
          }
        ))
      }
      n <- sum(unlist(lapply(as.list(seq_len(length(eif_full))), function(k) {
          length(eif_full[[k]])
      })))
      se <- sqrt(mean(fold_vars) / n)
    } else if (!cross_fit & sample_split) {
      if (isTRUE(use_square)) {
        var_1 <- mean( (eif_full) ^ 2 )
        var_2 <- mean( (eif_reduced) ^ 2 )
      } else {
        if (isTRUE(na.rm)) {
          var_1 <- as.numeric(crossprod(na.omit(matrix(eif_full)))) / length(eif_full)
          var_2 <- as.numeric(crossprod(na.omit(matrix(eif_reduced)))) / length(eif_reduced)
        } else {
          var_1 <- as.numeric(crossprod(matrix(eif_full))) / length(eif_full)
          var_2 <- as.numeric(crossprod(matrix(eif_reduced))) / length(eif_reduced)
        }
      }
        se <- sqrt( as.numeric(var_1) / length(eif_full) + as.numeric(var_2) / length(eif_reduced) )
    } else {
        n_1 <- sum(unlist(lapply(as.list(seq_len(length(eif_full))), function(k) {
            length(eif_full[[k]])
        })))
        n_2 <- sum(unlist(lapply(as.list(seq_len(length(eif_reduced))), function(k) {
            length(eif_reduced[[k]])
        })))
        full_indices <- as.list(seq_len(length(eif_full)))
        redu_indices <- as.list(seq_len(length(eif_reduced)))
        if (isTRUE(use_square)) {
          full_vars <- unlist(lapply(full_indices, function(k) {
            mean( (eif_full[[k]]) ^ 2 , na.rm = na.rm)
          }))
          redu_vars <- unlist(lapply(redu_indices, function(k) {
            mean( (eif_reduced[[k]]) ^ 2 , na.rm = na.rm)
          }))
        } else {
          full_vars <- unlist(lapply(full_indices, function(k) {
            if (isTRUE(na.rm)) {
              as.numeric(crossprod(na.omit(matrix(eif_full[[k]])))) / length(eif_full[[k]])
            } else {
              as.numeric(crossprod(matrix(eif_full[[k]]))) / length(eif_full[[k]])
            }
          }))
          redu_vars <- unlist(lapply(redu_indices, function(k) {
            if (isTRUE(na.rm)) {
              as.numeric(crossprod(na.omit(matrix(eif_reduced[[k]])))) / length(eif_reduced[[k]])
            } else {
              as.numeric(crossprod(matrix(eif_reduced[[k]]))) / length(eif_reduced[[k]])
            }
          }))
        }

        se <- sqrt( mean(full_vars) / n_1 + mean(redu_vars) / n_2 )
    }
    return(se)
}
