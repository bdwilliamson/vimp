#' Format a \code{vim} object
#'
#' Nicely formats the output from a \code{vim} object for printing.
#'
#' @param x the \code{vim} object of interest.
#' @param ... other options, see the generic \code{format} function.
#' @export
format.vim <- function(x, ...) {
  ## create the output matrix
  nice_s <- function(x) ifelse(length(x) <= 10, paste(x, collapse = ", "), paste(c(x[1:10], "..."), collapse = ", "))
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
    print_s <- lapply(as.list(x$mat$s), function(x) nice_s(strsplit(x, ",", fixed = TRUE)[[1]]))
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
    
    print_s <- nice_s(x$s)
    rownames(output) <- paste("s = ", print_s, sep = "")
  }
  col_nms <- c("Estimate", "SE", paste(100 - 100*x$alpha[[1]], "% CI", sep = ""))
  if (!any(grepl("anova", class(x)))) {
    tmp <- c(col_nms, paste0("VIMP > ", x$delta), "p-value")
    col_nms <- tmp
  } 
  if (grepl("log", x$scale) & !grepl("logit", x$scale)) {
    col_nms[2] <- "SE (log scale)"
  } else if (grepl("logit", x$scale)) {
    col_nms[2] <- "SE (logit scale)"
  } else {

  }
  colnames(output) <- col_nms
  return(output)
}
