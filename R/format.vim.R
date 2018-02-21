#' Format a \code{vim} object
#'
#' Nicely formats the output from a \code{vim} object for printing.
#'
#' @param x the \code{vim} object of interest.
#' @param ... other options, see the generic \code{format} function.
#' @export
format.vim <- function(x, ...) {
  ## create the output matrix
  ## if it is a combined object, we need to print the matrix instead
  if (!is.null(x$mat)) {
    ## combine the columns for cis
    tmp.ci <- cbind(format(x$mat$cil, ...), format(x$mat$ciu, ...))
    output <- cbind(format(x$mat$est, ...), format(x$mat$se, ...), apply(tmp.ci, 1, function(x) paste("[", paste(x, collapse = ", "), "]", sep = "")))
    ## tag on row names
    tmp.j <- x$s
    rownames(output) <- paste("s = ", tmp.j, sep = "")
  } else {
    output <- cbind(format(x$est, ...), format(x$se, ...), paste("[", paste(format(x$ci, ...), collapse = ", "), "]", sep = ""))
    rownames(output) <- paste("s = ", paste(x$s, collapse = ", "), sep = "")
  }
  colnames(output) <- c("Estimate", "SE", paste(100 - 100*x$alpha[[1]], "% CI", sep = ""))

  return(output)
}
