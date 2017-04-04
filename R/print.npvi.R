#' Print a \code{npvi} object
#'
#' Prints out the table of estimates, confidence intervals, and standard errors for a \code{npvi} object.
#'
#' @param x the \code{npvi} object of interest.
#' @param digits the number of digits to display results to. Defaults to the maximum of 3 and the "digits" option minus 3.
#' @param ... other options, see the generic \code{print} function.
#' @export

print.npvi <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  ## create the output matrix
  ## if it is a combined object, we need to print the matrix instead
  if (!is.null(x$mat)) {
    output <- x$mat
    colnames(output) <- c("Estimate", "SE", paste(100 - 100*x$alpha[[1]], "% CIL", sep = ""), paste(100 - 100*x$alpha[[1]], "% CIU", sep = ""))
    ## print out the call
    cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
  } else {
    output <- cbind(round(x$est, digits), round(x$se, digits), round(x$ci, digits))
    rownames(output) <- paste("j = ", x$j, sep = "")
    colnames(output) <- c("Estimate", "SE", paste(100 - 100*x$alpha, "% CIL", sep = ""), paste(100 - 100*x$alpha, "% CIU", sep = ""))
    ## print out the call
    cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")
  }


  ## print out the matrix
  cat("\nEstimates:\n")
  print(output)
}
