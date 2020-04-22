#' Merge multiple \code{vim} objects into one
#'
#' Take the output from multiple different calls to \code{vimp_regression} and
#' merge into a single \code{vim} object; mostly used for plotting results.
#'
#' @param ... an arbitrary number of \code{vim} objects, separated by commas.
#'
#' @return an object of class \code{vim} containing all of the output
#' from the individual \code{vim} objects. This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{merge_vim()}}
#'  \item{s}{ - a list of the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - a list of the libraries of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - a list of the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{ - a list of the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{- a vector with the corrected estimates}
#'  \item{naive}{- a vector with the naive estimates}
#'  \item{update}{- a list with the influence curve-based updates}
#'  \item{se}{- a vector with the standard errors}
#'  \item{ci}{- a matrix with the CIs}
#'  \item{mat}{ - a tibble with the estimated variable importance, the standard errors, and the \eqn{(1-\alpha) \times 100}\% confidence intervals}
#'  \item{full_mod}{ - a list of the objects returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - a list of the objects returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - a list of the levels, for confidence interval calculation}
#' }
#'
#' @examples
#' library(SuperLearner)
#' library(ranger)
#' ## generate the data
#' ## generate X
#' p <- 2
#' n <- 100
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.ranger"
#'
#' ## using Super Learner (with a small number of folds, for illustration only)
#' est_2 <- vimp_regression(Y = y, X = x, indx = 2, V = 2,
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 2))
#'
#' est_1 <- vimp_regression(Y = y, X = x, indx = 1, V = 2,
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 2))
#'
#' ests <- merge_vim(est_1, est_2)
#' @importFrom magrittr "%>%"
#' @importFrom rlang "!!" sym
#' @export
merge_vim <- function(...) {
  ## capture the arguments
  L <- list(...)
  names(L) <- unlist(match.call(expand.dots=F)$...)
  p <- length(L)

  ## extract the estimates and CIs from each element of the list
  ests <- do.call(c, lapply(L, function(z) z$est))
  naives <- do.call(c, lapply(L, function(z) z$naive))
  cis <- do.call(rbind, lapply(L, function(z) z$ci))
  ses <- do.call(c, lapply(L, function(z) z$se))
  tests <- do.call(c, lapply(L, function(z) z$test))
  p_values <- do.call(c, lapply(L, function(z) z$p_value))
  predictivenesses_full <- do.call(c, lapply(L, function(z) z$predictiveness_full))
  predictivenesses_reduced <- do.call(c, lapply(L, function(z) z$predictiveness_reduced))
  predictiveness_cis_full <- do.call(rbind, lapply(L, function(z) z$predictiveness_ci_full))
  predictiveness_cis_reduced <- do.call(rbind, lapply(L, function(z) z$predictiveness_ci_reduced))
  hyp_test_predictivenesses_full <- do.call(c, lapply(L, function(z) z$hyp_test_predictiveness_full))
  hyp_test_predictivenesses_reduced <- do.call(c, lapply(L, function(z) z$hyp_test_predictiveness_red))
  hyp_test_predictiveness_cis_full <- do.call(rbind, lapply(L, function(z) z$hyp_test_predictiveness_ci_full))
  hyp_test_predictiveness_cis_reduced <- do.call(rbind, lapply(L, function(z) z$hyp_test_predictiveness_ci_reduced))
  delta <- min(do.call(c, lapply(L, function(z) z$delta)))
  scale <- unique(unlist(lapply(L, function(z) z$scale)))
  
  ## put on names
  names(ests) <- "est"
  names(tests) <- "test"
  names(p_values) <- "p_value"

  ## now get lists of the remaining components
  call <- match.call()
  updates <- lapply(L, function(z) z$update)
  s <- do.call(c, lapply(L, function(z) z$s))
  SL.library <- lapply(L, function(z) z$SL.library)
  full_fit <- lapply(L, function(z) z$full_fit)
  red_fit <- lapply(L, function(z) z$red_fit)
  full_mod <- lapply(L, function(z) z$full_mod)
  red_mod <- lapply(L, function(z) z$red_mod)
  alpha <- min(unlist(lapply(L, function(z) z$alpha)))
  scale <- unique(unlist(lapply(L, function(z) z$scale)))

  ## combine into a tibble
  mat <- do.call(dplyr::bind_rows, lapply(L, function(z) z$mat)) %>% 
    dplyr::arrange(dplyr::desc(!! rlang::sym("est")))

  ## create output list
  output <- list(call = call,
              s = s, SL.library = SL.library, full_fit = full_fit,
              red_fit = red_fit, est = mat$est, naive = naives, update = updates,
              se = mat$se, ci = cbind(mat$cil, mat$ciu),
              predictiveness_full = predictivenesses_full,
              predictiveness_reduced = predictivenesses_reduced,
              predictiveness_ci_full = predictiveness_cis_full,
              predictiveness_ci_reduced = predictiveness_cis_reduced,
              test = mat$test,
              p_value = mat$p_value,
              hyp_test_predictiveness_full = hyp_test_predictivenesses_full,
              hyp_test_predictiveness_red = hyp_test_predictivenesses_reduced,
              hyp_test_predictiveness_ci_full = hyp_test_predictiveness_cis_full,
              hyp_test_predictiveness_ci_reduced = hyp_test_predictiveness_cis_reduced,
              mat = mat,
              full_mod = full_mod, red_mod = red_mod,
              alpha = alpha,
              delta = delta,
              scale = scale)
  tmp <- class(output)
  classes <- unlist(lapply(L, function(z) class(z)[2]))
  class(output) <- c("vim", classes, tmp)

  return(output)
}
