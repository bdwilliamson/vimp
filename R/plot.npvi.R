#' Plot \code{npvi} objects
#'
#' Plot the estimates and standard errors for a set of \code{npvi} objects.
#'
#' @param x an \code{npvi} object created from a call to \code{combine}.
#' @param y a vector of names, given in the same order as the list.
#' @param ... other options, see the generic \code{plot} function.
#' @export
plot.npvi <- function(x, y, ...) {

  ## order based on the estimate
  ord.mat <- x$mat[order(x$mat$est), ]

  ## plot
  graphics::plot(ord.mat$est, 1:dim(ord.mat)[1], ...)
  graphics::abline(v = 0, col = "red", lty = 2)
  graphics::arrows(unlist(ord.mat$cil), 1:dim(ord.mat)[1], unlist(ord.mat$ciu), 1:dim(ord.mat)[1],
         length = 0, angle = 90, lwd = 2)
  graphics::axis(side = 2, at = 1:dim(ord.mat)[1], label = y[order(ord.mat$est)], las = 2)
  graphics::axis(side = 1, at = seq(0, 1, 0.1))
  graphics::box()
}
