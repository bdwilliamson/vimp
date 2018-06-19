#' Average multiple independent importance estimates 
#' 
#' Average the output from multiple calls to \code{vimp_regression}, for different independent groups, into a single estimate with a corresponding standard error and confidence interval.
#' 
#' @param ... an arbitrary number of \code{vim} objects
#' @param weights how to average the vims together, and must sum to 1; defaults to 1/(number of vims) for each vim, corresponding to the arithmetic mean
#' 
#' @return an object of class \code{vim} containing the (weighted) average of the individual importance estimates, as well as the appropriate standard error and confidence interval. 
#' This results in a list containing:
#' \itemize{
#'  \item{call}{ - the call to \code{average_vim()}}
#'  \item{s}{ - a list of the column(s) to calculate variable importance for}
#'  \item{SL.library}{ - a list of the libraries of learners passed to \code{SuperLearner}}
#'  \item{full_fit}{ - a list of the fitted values of the chosen method fit to the full data}
#'  \item{red_fit}{ - a list of the fitted values of the chosen method fit to the reduced data}
#'  \item{est}{- a vector with the corrected estimates}
#'  \item{naive}{- a vector with the naive estimates}
#'  \item{update}{- a list with the influence curve-based updates}
#'  \item{mat}{ - a matrix with the estimated variable importance, the standard error, and the \eqn{(1-\alpha) \times 100}\% confidence interval}
#'  \item{full_mod}{ - a list of the objects returned by the estimation procedure for the full data regression (if applicable)}
#'  \item{red_mod}{ - a list of the objects returned by the estimation procedure for the reduced data regression (if applicable)}
#'  \item{alpha}{ - the level, for confidence interval calculation}
#' }

#' @examples
#' library(SuperLearner)
#' library(gam)
#' ## generate the data
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
#' learners <- "SL.gam"
#' 
#' ## get estimates on independent splits of the data
#' samp <- sample(1:n, n/2, replace = FALSE)
#'
#' ## using Super Learner
#' est_2 <- vimp_regression(Y = y[samp], X = x[samp, ], indx = 2, 
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' est_1 <- vimp_regression(Y = y[-samp], X = x[-samp, ], indx = 2, 
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 10))
#'
#' ests <- average_vim(est_1, est_2, weights = c(1/2, 1/2))
#' @export
average_vim <- function(..., weights = rep(1/length(list(...)), length(list(...)))) {
	## capture the arguments
  	L <- list(...)
  	names(L) <- unlist(match.call(expand.dots=F)$...)
  	p <- length(L)

  	## check if weights sum to 1; if not, break
  	if (sum(weights) != 1) stop("Weights must sum to one.")

  	## extract the estimates and SEs from each element of the list
  	## also get the sample sizes
  	ests <- do.call(rbind.data.frame, lapply(L, function(z) z$est))
    naives <- do.call(rbind.data.frame, lapply(L, function(z) z$naive))
  	ses <- do.call(rbind.data.frame, lapply(L, function(z) z$se))
  	
  	names(ests) <- "est"
  	names(ses) <- "se"

  	## create the (weighted) average
  	est_avg <- sum(weights*ests)

  	## combine the variances correctly
  	## will need to use the covariance, if not independent
  	se_avg <- sqrt(matrix(weights^2, nrow = 1)%*%as.matrix(ses^2))

  	## create a CI
  	alpha <- min(unlist(lapply(L, function(z) z$alpha)))
  	ci_avg <- vimp_ci(est_avg, se_avg, level = 1 - alpha)

  	## create the output matrix
  	mat <- cbind.data.frame(est_avg, se_avg, ci_avg)
  	colnames(mat) <- c("est", "se", "cil", "ciu")

  	## now get lists of the remaining components
    call <- match.call()
    updates <- lapply(L, function(z) z$update)
    s_lst <- lapply(L, function(z) z$s)
    s <- paste0("avg_", paste(unlist(s_lst), collapse = "_"))  
    SL.library <- lapply(L, function(z) z$SL.library)
    full_fit <- lapply(L, function(z) z$full_fit)
    red_fit <- lapply(L, function(z) z$red_fit)
    full_mod <- lapply(L, function(z) z$full_mod)
    red_mod <- lapply(L, function(z) z$red_mod)
    alpha <- min(unlist(lapply(L, function(z) z$alpha)))

    ## create output list
    output <- list(call = call,
              s = s, SL.library = SL.library, full_fit = full_fit,
              red_fit = red_fit, est = mat$est, naive = naives, update = updates, 
              se = mat$se, ci = cbind(mat$cil, mat$ciu),
              mat = mat,
              full_mod = full_mod, red_mod = red_mod,
              alpha = alpha)
    tmp <- class(output)
    class(output) <- c("vim", "list", tmp)

    return(output)
}