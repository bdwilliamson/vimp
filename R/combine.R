#' Combine multiple \code{npvi} objects
#'
#' Take the output from multiple different calls to \code{vim} and
#' combine into a single \code{npvi} object; mostly used for plotting results.
#'
#' @param ... an arbitrary number of \code{npvi} objects, separated by commas.
#'
#' @return an object of class \code{npvi} containing all of the output
#' from the individual \code{npvi} objects. This results in a list containing:
#' \itemize{
#'  \item{call}{ - a list of the calls to \code{vim}}
#'  \item{full.f}{ - a list of either the formula for the full regression or the fitted values for the full regressions, based on the \code{call}}
#'  \item{red.f}{ - a list of either the formula for the reduced regression or the fitted values for the reduced regressions, based on the \code{call}}
#'  \item{data}{ - the data used by the function}
#'  \item{j}{ - a list of the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - a list of the libraries of learners passed to \code{SuperLearner}}
#'  \item{full.fit}{ - a list of the fitted values of the chosen method fit to the full data}
#'  \item{red.fit}{ - a list of the fitted values of the chosen method fit to the reduced data}
#'  \item{mat}{ - a matrix with the estimated variable importance, the standard errors, and the \eqn{(1-\alpha) x 100}\% confidence intervals}
#'  \item{full.mod}{ - a list of the objects returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red.mod}{ - a list of the objects returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - a list of the levels, for confidence interval calculation}
#' }
#'
#' @examples
#' \dontrun{
#' require(SuperLearner)
#' ## generate the data
#' ## generate X
#' x <- replicate(p, stats::runif(n, -5, 5))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- smooth + stats::rnorm(n, 0, 1)
#'
#' testdat <- as.data.frame(cbind(y, x))
#'
#' ## set up a library for SuperLearner
#' learners <- "SL.gam"
#'
#' ## using class "formula"
#' est.2 <- vim(y ~ x, fit ~ x, data = testdat, y = testdat[, 1],
#'            n = length(y), j = 2, standardized = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' est.1 <- vim(est.2$full.fit, fit ~ x, data = testdat, y = testdat[, 1],
#' n = length(y), j = 1, standardized = TRUE, alpha = 0.05, SL.library = learners,
#' cvControl = list(V = 10))
#'
#' ests <- combine(est.1, est.2)
#' }
#' @export
combine <- function(...) {
  ## capture the arguments
  L <- list(...)
  names(L) <- unlist(match.call(expand.dots=F)$...)
  p <- length(L)

  ## extract the estimates and CIs from each element of the list
  ests <- do.call(rbind.data.frame, lapply(L, function(z) z$est))
  cis <- do.call(rbind.data.frame, lapply(L, function(z) z$ci))

  ## combine into a matrix
  tmp <- cbind(ests, cis)
  names(tmp) <- c("est", "cil", "ciu")
  ## put in decreasing order
  mat <- tmp[order(tmp$est, decreasing = TRUE), ]

  ## now get lists of the remaining components
  call <- lapply(L, function(z) z$call)
  full.f <- lapply(L, function(z) z$full.f)
  red.f <- lapply(L, function(z) z$red.f)
  data <- L[[1]]$data
  j <- lapply(L, function(z) z$j)
  SL.library <- lapply(L, function(z) z$SL.library)
  full.fit <- lapply(L, function(z) z$full.fit)
  red.fit <- lapply(L, function(z) z$red.fit)
  full.mod <- lapply(L, function(z) z$full.mod)
  red.mod <- lapply(L, function(z) z$red.mod)
  alpha <- lapply(L, function(z) z$alpha)

  ## create output list
  output <- list(call = call, full.f = full.f, red.f = red.f, data = data,
              j = j, SL.library = SL.library, full.fit = full.fit,
              red.fit = red.fit, mat = mat,
              full.mod = full.mod, red.mod = red.mod,
              alpha = alpha)
  tmp.cls <- class(mat)
  class(output) <- c("npvi", tmp.cls)

  return(output)
}
