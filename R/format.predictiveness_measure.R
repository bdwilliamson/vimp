#' Format a \code{predictiveness_measure} object
#'
#' Nicely formats the output from a \code{predictiveness_measure} object for printing.
#'
#' @param x the \code{predictiveness_measure} object of interest.
#' @param ... other options, see the generic \code{format} function.
#' @export
format.predictiveness_measure <- function(x, ...) {
  if (!all(is.na(x$eif))) {
    # grab the point estimate
    est <- x$point_est
    # compute the SE
    se <- sqrt(mean(x$eif ^ 2) / length(x$eif))
    output <- cbind(attr(x, "type"), format(est, ...), format(se, ...))
  } else {
    output <- cbind(attr(x, "type"), "", "")
  }
  # create the output matrix
  col_nms <- c("Type", "Estimate", "SE")
  colnames(output) <- col_nms
  return(output)
}
