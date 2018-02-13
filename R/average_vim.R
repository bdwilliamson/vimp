#' Average multiple independent importance estimates 
#' 
#' Average the output from multiple calls to vim, for different independent groups, into a single estimate with a corresponding standard error and confidence interval.
#' 
#' @param ... an arbitrary number of \code{vim} objects
#' @param weights how to average the vims together, and must sum to 1; defaults to 1/(number of vims) for each vim, corresponding to the arithmetic mean
#' 
#' @return an object of class \code{vim} containing the (weighted) average of the individual importance estimates, as well as the appropriate standard error and confidence interval. 
#' This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{average_vim()}}
#'  \item{full.f}{ - a list of individual formulas or fitted values from the full regressions}
#'  \item{red.f}{ - a list of individual formulas or fitted values from the reduced regressions}
#'  \item{data}{ - the data used by the function}
#'  \item{j}{ - a list of the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - a list of the libraries of learners passed to \code{SuperLearner}}
#'  \item{full.fit}{ - a list of the fitted values of the chosen method fit to the full data}
#'  \item{red.fit}{ - a list of the fitted values of the chosen method fit to the reduced data}
#'  \item{mat}{ - a matrix with the estimated variable importance, the standard error, and the \eqn{(1-\alpha) x 100}\% confidence interval}
#'  \item{full.mod}{ - a list of the objects returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red.mod}{ - a list of the objects returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }

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
#' ests <- average_vim(est.1, est.2, weights = c(1, 1))
#' }
#' @export
average_vim <- function(..., weights = rep(1/length(...), length(...))) {
	## capture the arguments
  	L <- list(...)
  	names(L) <- unlist(match.call(expand.dots=F)$...)
  	p <- length(L)

  	## check if weights sum to 1; if not, break
  	if (sum(weights) != 1) stop("Weights must sum to one.")

  	## extract the estimates and SEs from each element of the list
  	## also get the sample sizes
  	ests <- do.call(rbind.data.frame, lapply(L, function(z) z$est))
  	ses <- do.call(rbind.data.frame, lapply(L, function(z) z$se))

  	## create the (weighted) average
  	est_avg <- weights*sum(ests)

  	## combine the variances correctly
  	## will need to use the covariance, if not independent
  	sd_avg <- sqrt(matrix(weights^2, nrow = 1)%*%as.matrix(ses^2))

  	## create a CI
  	ci_avg <- variableImportanceCI(est_avg, sd_avg)

  	## create the output matrix
  	mat <- cbind(est_avg, sd_avg, ci_avg)
  	names(mat) <- c("est", "se", "cil", "ciu")

  	## now get lists of the remaining components
  	call <- match.call()
  	full.f <- lapply(L, function(z) z$full.f)
  	red.f <- lapply(L, function(z) z$red.f)
  	data <- lapply(L, function(z) z$data)
  	j <- lapply(L, function(z) z$j)
  	SL.library <- lapply(L, function(z) z$SL.library)
  	full.fit <- lapply(L, function(z) z$full.fit)
  	red.fit <- lapply(L, function(z) z$red.fit)
  	full.mod <- lapply(L, function(z) z$full.mod)
  	red.mod <- lapply(L, function(z) z$red.mod)
  	alpha <- lapply(L, function(z) z$alpha)
  	
  	output <- list(call = call, full.f = full.f, red.f = red.f, data = data,
              j = j, SL.library = SL.library, full.fit = full.fit,
              red.fit = red.fit, mat = mat,
              full.mod = full.mod, red.mod = red.mod,
              alpha = alpha)
  tmp.cls <- class(mat)
  class(output) <- c("vim", tmp.cls)
}