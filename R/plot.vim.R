#' Plot \code{vim} objects
#'
#' Plot the estimates and standard errors for a set of \code{vim} objects.
#'
#' @param vimp_obj a \code{vim} object.
#' @param labels a vector of names, given in the same order as the estimates in the \code{vim} object.
#' @param ... other options, see the \code{cowplot} implementation of \code{ggplot}.
#'
#' @seealso \code{\link[ggplot2]{ggplot}} for specific usage of the \code{ggplot} function.
#' @export
plot.vim <- function(vimp_obj, labels, ...) {

    ## if vimp_obj has a "mat", turn it into a tibble ordered by the estimate
    if (!is.null(vimp_obj$mat)) {
        vimp_tib <- tibble::as_tibble(vimp_obj$mat) %>% 
          mutate(s = vimp_obj$s) %>% 
          arrange(desc(est))
    } else { ## otherwise, create a tibble with the estimate
      vimp_tib <- tibble::tibble(est = vimp_obj$est, se = vimp_obj$se[1], 
                                 cil = vimp_obj$ci[1], ciu = vimp_obj$ci[2], test = vimp_obj$est, 
                                 p_value = vimp_obj$p_value, s = est$s)
    }
    ## if labels aren't passed in, use s instead
    if (missing(labels)) {
      vimp_tib <- vimp_tib %>% 
        mutate(plot_labels = s)
    } else {
      vimp_tib <- vimp_tib %>% 
        mutate(plot_labels = labels)
    }
    ## get the type of variable importance estimate
    vimp_cls <- class(vimp_obj)[2]
    if (vimp_cls == "r_squared") {
      x_label <- expression(paste("Variable importance estimate: ", R^2, sep = ""))
    } else if (vimp_cls == "deviance") {
      x_label <- "Variable importance estimate: Deviance"
    } else if (vimp_cls == "auc") {
      x_label <- "Variable importance estiamte: Difference in AUC"
    } else if (vimp_cls == "accuracy") {
      x_label <- "Variable importance estimate: Difference in Accuracy"
    } else {
      stop("We haven't implemented this variable importance parameter yet!")
    }
    ## plot it
    vimp_plt <- vimp_tib %>% 
      ggplot(aes(x = est, y = plot_labels)) +
      geom_point() +
      geom_errorbarh(aes(xmin = cil, xmax = ciu)) +
      xlab(x_label) +
      ylab("") +
      ggtitle("Variable importance estimates")
    vimp_plt
}
