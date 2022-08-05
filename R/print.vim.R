#' Print \code{vim} objects
#'
#' Prints out the table of estimates, confidence intervals, and standard errors for a \code{vim} object.
#'
#' @param x the \code{vim} object of interest.
#' @param ... other options, see the generic \code{print} function.
#' @export
print.vim <- function(x, ...) {
  # print out the matrix
  cat("Variable importance estimates:\n")
  print(format(x, ...), quote = "FALSE")
  invisible(x)
}
