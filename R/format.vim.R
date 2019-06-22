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
    if (!any(grepl("anova", class(x)))) {
      output <- cbind(format(x$mat$est, ...), format(x$mat$se, ...), 
                      apply(tmp.ci, 1, function(x) paste("[", paste(x, collapse = ", "), "]", sep = "")),
                      format(x$mat$test, ...),
                      format(x$mat$p_value, ...))  
    } else {
      output <- cbind(format(x$mat$est, ...), format(x$mat$se, ...), 
                      apply(tmp.ci, 1, function(x) paste("[", paste(x, collapse = ", "), "]", sep = "")))  
    }
    
    ## tag on row names
    print_s <- apply(matrix(x$s), 1, 
                     function(x) ifelse(length(x) <= 10, 
                                        paste(x, collapse = ", "), 
                                        paste(c(x[1:10], "..."), collapse = ", ")))
    rownames(output) <- paste("s = ", print_s, sep = "")
  } else {
    if (!any(grepl("anova", class(x)))) {
      output <- cbind(format(x$est, ...), format(x$se, ...), 
                      paste("[", paste(format(x$ci, ...), collapse = ", "), "]", sep = ""),
                      format(x$test, ...),
                      format(x$p_value, ...))  
    } else {
      output <- cbind(format(x$est, ...), format(x$se, ...), 
                      paste("[", paste(format(x$ci, ...), collapse = ", "), "]", sep = ""))
    }
    
    print_s <- ifelse(length(x$s) <= 10, 
                      paste(x$s, collapse = ", "), 
                      paste(c(x$s[1:10], "..."), collapse = ", "))
    rownames(output) <- paste("s = ", print_s, sep = "")
  }
  if (!any(grepl("anova", class(x)))) {
    colnames(output) <- c("Estimate", "SE", paste(100 - 100*x$alpha[[1]], "% CI", sep = ""), 
                          "VIMP > 0", "p_value")
  } else {
    colnames(output) <- c("Estimate", "SE", paste(100 - 100*x$alpha[[1]], "% CI", sep = ""))
  }
  return(output)
}
