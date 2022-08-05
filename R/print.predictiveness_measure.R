#' Print \code{predictiveness_measure} objects
#'
#' Prints out a table of the point estimate and standard error for a \code{predictiveness_measure} object.
#'
#' @param x the \code{predictiveness_measure} object of interest.
#' @param ... other options, see the generic \code{print} function.
#' @export
print.predictiveness_measure <- function(x, ...) {
  print(format(x, ...), quote = FALSE)
  invisible(x)
}
