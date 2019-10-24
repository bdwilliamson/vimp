#' Print a \code{vim} object
#'
#' Prints out the table of estimates, confidence intervals, and standard errors for a \code{vim} object.
#'
#' @param x the \code{vim} object of interest.
#' @param ... other options, see the generic \code{print} function.
#' @export

print.vim <- function(x, ...) {

  ## print out the call
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

  ## print out the matrix
  cat("\nVariable importance estimates:\n")
  print(format(x, ...), quote = FALSE)
}
