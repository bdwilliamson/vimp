#' Standard error estimate for SPVIM values
#'
#' Compute standard error estimates based on the estimated influence function 
#'   for a SPVIM value of interest.
#'
#' @param ics the influence function estimates based on the contributions 
#'   from sampling observations and sampling subsets: a list of length two 
#'   resulting from a call to \code{spvim_ics}.
#' @param idx the index of interest
#' @param gamma the proportion of the sample size used when sampling subsets
#' @param na_rm remove \code{NA}s?
#'
#' @return The standard error estimate for the desired SPVIM value
#'
#' @details Since the processes for sampling observations and subsets are 
#'   independent, the variance for a given SPVIM estimator is simply the sum of
#'  the vairances based on sampling observations and on sampling subsets.
#'
#' @seealso \code{\link[vimp]{spvim_ics}} for how the influence functions are estimated.
#' @importFrom stats var
#' @export
spvim_se <- function(ics, idx = 1, gamma = 1, na_rm = FALSE) {
  var_v <- mean(sapply(1:length(ics$contrib_v), function(i) {
    var(ics$contrib_v[[i]][idx, ], na.rm = na_rm)
  }))
  var_s <- var(ics$contrib_s[idx, ], na.rm = na_rm)

  se <- sqrt(var_v / ncol(ics$contrib_v[[1]]) + var_s / ncol(ics$contrib_s) * (1 / gamma))
  return(list(se=se, var_v_contrib = var_v / nrow(ics$contrib_v[[1]]), 
              var_s_contrib = var_s / ncol(ics$contrib_s) * (1 / gamma)))
}
