#' Plot \code{vim} objects
#'
#' Plot the estimates and standard errors for a set of \code{vim} objects.
#'
#' @param x an \code{vim} object.
#' @param y a vector of names, given in the same order as the estimates in the \code{vim} object.
#' @param ... other options, see the generic \code{plot} function.
#' @export
plot.vim <- function(x, y, ...) {

  ## order based on the estimate
  if (!is.null(x$mat)) {
    ord.mat <- x$mat[order(x$mat$est), ]
    if (missing(y)) {
      tmp <- x$s
      y <- paste("s = ", tmp, sep = "")
    }
  } else {
    tmp <- data.frame(est = x$est, se = x$se, x$ci)
    names(tmp) <- c("est", "se", "cil", "ciu")
    ord.mat <- tmp[order(tmp$est), ]
    if (missing(y)) {
      y <- paste("s = ", x$s, sep = "")
    }
  }


  ## get the graphical parameters passed in
  L <- list(...)

  ## set up default parameters, if they didn't get passed in
  if (!("mar" %in% names(L))) {
    mar <- c(5, 12, 4, 2) + 0.1
  } else {
    mar <- L$mar
  }
  if (!("xlim" %in% names(L))) {
    xlim <- c(0, 1)
  } else {
    xlim <- L$xlim
  }
  if (!("ylim" %in% names(L))) {
    ylim <- c(0, dim(ord.mat)[1])
  } else {
    ylim <- L$ylim
  }
  # if (!("cex") %in% names(L)) {
  #   cex <- 1
  # } else {
  #   cex <- L$cex
  # }
  # if (!("cex.axis" %in% names(L))) {
  #   cex.axis <- 1
  # } else {
  #   cex.axis <- L$cex.axis
  # }

  ## plot
  graphics::par(mar = mar)
  graphics::plot.new()
  graphics::plot.window(xlim, ylim)
  graphics::points(ord.mat$est, 1:dim(ord.mat)[1], ...)
  graphics::abline(v = 0, col = "red", lty = 2)
  graphics::arrows(unlist(ord.mat$cil), 1:dim(ord.mat)[1], unlist(ord.mat$ciu), 1:dim(ord.mat)[1],
         length = 0, angle = 90, lwd = 2)
  graphics::axis(side = 2, at = 1:dim(ord.mat)[1], label = y[order(ord.mat$est)], las = 2, ...)
  graphics::axis(side = 1, at = seq(xlim[1], xlim[2], xlim[2]/10), ...)
  graphics::title(xlab = L$xlab)
  graphics::title(main = L$main)
  graphics::box()
}
