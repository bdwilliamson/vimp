#' Average multiple independent importance estimates
#'
#' Average the output from multiple calls to \code{vimp_regression}, for different independent groups, into a single estimate with a corresponding standard error and confidence interval.
#'
#' @param ... an arbitrary number of \code{vim} objects.
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
#'  \item{y}{ - a list of the outcomes}
#' }

#' @examples
#' library(SuperLearner)
#' library(ranger)
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
#' learners <- "SL.ranger"
#'
#' ## get estimates on independent splits of the data
#' samp <- sample(1:n, n/2, replace = FALSE)
#'
#' ## using Super Learner (with a small number of folds, for illustration only)
#' est_2 <- vimp_regression(Y = y[samp], X = x[samp, ], indx = 2, V = 2,
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 2))
#'
#' est_1 <- vimp_regression(Y = y[-samp], X = x[-samp, ], indx = 2, V = 2,
#'            run_regression = TRUE, alpha = 0.05,
#'            SL.library = learners, cvControl = list(V = 2))
#'
#' ests <- average_vim(est_1, est_2, weights = c(1/2, 1/2))
#' 
#' @importFrom rlang "!!" sym
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
  	ests <- do.call(c, lapply(L, function(z) z$est))
   naives <- do.call(c, lapply(L, function(z) z$naive))
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
  	hyp_test_ses_full <- do.call(rbind, lapply(L, function(z) z$hyp_test_se_full))
  	hyp_test_ses_redu <- do.call(rbind, lapply(L, function(z) z$hyp_test_se_reduced))
  	test_statistics <- do.call(rbind, lapply(L, function(z) z$test_statistic))
  	delta <- min(do.call(c, lapply(L, function(z) z$delta)))
  	scale <- unique(unlist(lapply(L, function(z) z$scale)))

  	names(ests) <- "est"
  	names(ses) <- "se"
   names(naives) <- "naive"

  	## create the (weighted) average
  	est_avg <- sum(weights*ests)
  	predictiveness_full <- sum(weights*predictivenesses_full)
  	predictiveness_reduced <- sum(weights*predictivenesses_reduced)
  	hyp_test_predictiveness_full <- sum(weights*hyp_test_predictivenesses_full)
  	hyp_test_predictiveness_redu <- sum(weights*hyp_test_predictivenesses_reduced)

  	## combine the variances correctly
  	## will need to use the covariance, if not independent
  	se_avg <- sqrt(matrix(weights^2, nrow = 1)%*%as.matrix(ses^2))
  	hyp_test_se_full <- sqrt(matrix(weights^2, nrow = 1)%*%as.matrix(hyp_test_ses_full^2))
  	hyp_test_se_redu <- sqrt(matrix(weights^2, nrow = 1)%*%as.matrix(hyp_test_ses_redu^2))

  	## create a CI
  	alpha <- min(unlist(lapply(L, function(z) z$alpha)))
  	ci_avg <- vimp_ci(est_avg, se_avg, level = 1 - alpha, scale = scale[1])

  	## hypothesis test:
  	# test_statistic <- (hyp_test_predictiveness_full - hyp_test_predictiveness_redu - delta)/sqrt(hyp_test_se_full^2 + hyp_test_se_redu^2)
  	test_statistic <- sum(weights * test_statistics)
  	p_value <- 1 - pnorm(test_statistic)
  	hyp_test <- p_value < alpha
  	
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

  	## combine into a tibble
  	mat <- tibble::tibble(s = s, est = est_avg, se = se_avg, cil = ci_avg[, 1], ciu = ci_avg[, 2],
  	                      test = hyp_test, p_value = p_value) %>% 
  	   dplyr::arrange(dplyr::desc(!! rlang::sym("est")))
  	
  	## create some of the necessary output
  	if (is.null(hyp_test_predictiveness_cis_full)) {
  	   ret_hyp_test_predictiveness_cis_full <- ret_hyp_test_predictiveness_cis_reduced <- NULL
  	} else {
  	   ret_hyp_test_predictiveness_cis_full <- colSums(weights*hyp_test_predictiveness_cis_full)
  	   ret_hyp_test_predictiveness_cis_reduced <- colSums(weights*hyp_test_predictiveness_cis_reduced)
  	}

  	## create output list
    output <- list(call = call,
              s = s, SL.library = SL.library, full_fit = full_fit,
              red_fit = red_fit, est = mat$est, naive = naives, update = updates,
              se = mat$se, ci = cbind(mat$cil, mat$ciu),
              predictiveness_full = predictiveness_full,
              predictiveness_reduced = predictiveness_reduced,
              predictiveness_ci_full = sum(weights*predictiveness_cis_full),
              predictiveness_ci_reduced = sum(weights*predictiveness_cis_reduced),
              test = hyp_test,
              p_value = p_value,
              hyp_test_predictiveness_full = hyp_test_predictiveness_full,
              hyp_test_predictiveness_reduced = hyp_test_predictiveness_redu,
              hyp_test_predictiveness_ci_full = ret_hyp_test_predictiveness_cis_full,
              hyp_test_predictiveness_ci_reduced = ret_hyp_test_predictiveness_cis_reduced,
              hyp_test_se_full = hyp_test_se_full,
              hyp_test_se_reduced = hyp_test_se_redu,
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
