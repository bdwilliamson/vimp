#' Generate test data
#'
#' Generates data used in the low-dimensional simulations.
#'
#' @param n the sample size
#' @param p the number of covariates. Defaults to 2, and many more will result in increased computation time for \code{np}.
#'
#' @examples
#' \dontrun{
#' generateData(100)
#' }

generateData <- function(n, p=2){

  ## generate X
  x <- replicate(p, stats::runif(n, -5, 5))

  ## apply the function to the x's
  smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2

  ## generate Y ~ Normal (smooth, 1)
  y <- smooth + stats::rnorm(n, 0, 1)

  return(as.data.frame(cbind(x,y)))
}
