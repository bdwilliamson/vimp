#' Obtain a Point Estimate and Efficient Influence Function Estimate for a Given Predictiveness Measure
#'
#' @param x an object of class \code{"predictiveness_measure"}
#'
#' @return A list with the point estimate, naive point estimate (for ANOVA only),
#'   estimated EIF, and the predictions for coarsened data EIF (for coarsened data settings only)
#'
#' @export
estimate.predictiveness_measure <- function(x) {
  arg_lst <- unclass(x)
  arg_lst$ipc_fit_type <- attr(x, "ipc_fit_type")
  arg_lst$ipc_est_type <- attr(x, "ipc_est_type")
  arg_lst$scale <- attr(x, "scale")
  arg_lst$na.rm <- attr(x, "na.rm")
  type <- attr(x, "type")
  if (grepl("accuracy", type)) {
    est_lst <- do.call(measure_accuracy, arg_lst)
  } else if (grepl("anova", type)) {
    est_lst <- do.call(measure_anova, arg_lst)
  } else if (grepl("auc", type)) {
    est_lst <- do.call(measure_auc, arg_lst)
  } else if (grepl("average_value", type)) {
    est_lst <- do.call(measure_average_value, arg_lst)
  } else if (grepl("cross_entropy", type)) {
    est_lst <- do.call(measure_cross_entropy, arg_lst)
  } else if (grepl("deviance", type)) {
    est_lst <- do.call(measure_deviance, arg_lst)
  } else if (grepl("mse", type)) {
    est_lst <- do.call(measure_mse, arg_lst)
  } else if (grepl("r_squared", type)) {
    est_lst <- do.call(measure_r_squared, arg_lst)
  }
  return(est_lst)
}
