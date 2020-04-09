#' Standard error estimate for SPVIM values
#'
#' Compute standard error estimates based on the estimated influence function for a SPVIM value of interest.
#'
#' @param ics the influence function estimates based on the contributions from sampling observations and sampling subsets: a list of length two resulting from a call to \code{spvim_ics}.
#' @param idx the index of interest
#' @param gamma the proportion of the sample size used when sampling subsets
#' @param na_rm remove \code{NA}s?
#'
#' @return The standard error estimate for the desired SPVIM value
#'
#' @details Since the processes for sampling observations and subsets are independent, the variance for a given SPVIM estimator is simply the sum of the vairances based on sampling observations and on sampling subsets.
#'
#' @examples
#' \donttest{
#' ## don't test because this can take some time to run
#' library(SuperLearner)
#' library(gam)
#' n <- 100
#' p <- 2
#' ## generate the data
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- as.matrix(smooth + stats::rnorm(n, 0, 1))
#'
#' ## set up a library for SuperLearner
#' learners <- c("SL.mean", "SL.gam")
#'
#' ## -----------------------------------------
#' ## using Super Learner
#' ## -----------------------------------------
#' set.seed(4747)
#' est <- sp_vim(Y = y, X = x, V = 5,
#' type = "r_squared",
#' SL.library = learners, alpha = 0.05)
#' }
#'
#' @seealso \code{\link[vimp]{spvim_ics}} for how the influence functions are estimated.
#' @importFrom stats var
#' @export
spvim_se <- function(ics, idx = 1, gamma = 1, na_rm = FALSE) {
  var_v <- var(ics$contrib_v[idx, ], na.rm = na_rm)
  var_s <- var(ics$contrib_s[idx, ], na.rm = na_rm)

  se <- sqrt(var_v / ncol(ics$contrib_v) + var_s / ncol(ics$contrib_s) * (1 / gamma))
  return(se)
}
