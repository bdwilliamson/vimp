#' Print a \code{npvi} object
#'
#' Prints out the table of estimates, confidence intervals, and standard errors for a \code{npvi} object.
#'
#' @param obj the \code{npvi} object of interest.
#' @param digits the number of digits to display results to. Defaults to the maximum of 3 and the "digits" option minus 3.
#'
#' @export

print.npvi <- function(obj, digits = max(3, getOption("digits") - 3)) {

  ## create the output matrix
  output <- cbind(round(obj$est, digits), round(obj$se, digits), round(obj$ci, digits))
  rownames(output) <- paste("j = ", obj$j, sep = "")
  colnames(output) <- c("Estimate", "SE", paste(1 - obj$alpha, "% CIL", sep = ""), paste(1 - obj$alpha, "% CIU", sep = ""))

  ## print out the call
  cat("\nCall:\n", paste(deparse(obj$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  ## print out the matrix
  cat("\nEstimates:\n")
  print(output)
}
