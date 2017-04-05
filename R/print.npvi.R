#' Print a \code{npvi} object
#'
#' Prints out the table of estimates, confidence intervals, and standard errors for a \code{npvi} object.
#'
#' @param x the \code{npvi} object of interest.
#' @param ... other options, see the generic \code{print} function.
#' @export

print.npvi <- function(x, ...) {

  ## print out the call
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

  ## print out the matrix
  cat("\nVariable importance estimates:\n")
  print(format(x, ...), quote = FALSE)
}
