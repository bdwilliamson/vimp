#' Obtain a Point Estimate and Efficient Influence Function Estimate for a Given Predictiveness Measure
#'
#' @param x an object of class \code{"predictiveness_measure"}
#' @param ... other arguments to type-specific predictiveness measures (currently unused)
#'
#' @return A list with the point estimate, naive point estimate (for ANOVA only),
#'   estimated EIF, and the predictions for coarsened data EIF (for coarsened data settings only)
#'
#' @export
estimate.predictiveness_measure <- function(x, ...) {
  arg_lst <- unclass(x)
  arg_lst$ipc_fit_type <- attr(x, "ipc_fit_type")
  arg_lst$ipc_est_type <- attr(x, "ipc_est_type")
  arg_lst$scale <- attr(x, "scale")
  arg_lst$na.rm <- attr(x, "na.rm")
  type <- attr(x, "type")
  unhelpful_objects <- c("cross_fitting_folds", "K", "point_est", "eif",
                         "folds_Z")
  unhelpful_indices <- which(names(arg_lst) %in% unhelpful_objects)
  if (arg_lst$K == 1) {
    # apply the measure function to all observations
    est_lst <- estimate_type_predictiveness(arg_lst[-unhelpful_indices], type)
    x$point_est <- est_lst$point_est
    x$eif <- est_lst$eif
    x$ipc_eif_preds <- est_lst$ipc_eif_preds
    x$all_point_ests <- est_lst$point_est
    x$all_eifs <- list(x$eif)
  } else {
    # do cross-fitting; but don't need to do cross-fitting on components of
    # the predictiveness measure that don't involve the features (e.g., the
    # marginal outcome variance for R-squared and the marginal probability of Y = 1
    # for deviance)
    estimate_denominator <- FALSE
    full_type <- type
    if (any(grepl("r_squared", type))) {
      type <- "mse"
      estimate_denominator <- TRUE
    }
    if (any(grepl("deviance", type))) {
      type <- "cross_entropy"
      estimate_denominator <- TRUE
    }
    max_nrow <- max(sapply(1:arg_lst$K, function(k) length(arg_lst$C[arg_lst$folds_Z == k])))
    eifs <- vector("list", length = arg_lst$K)
    eif <- vector("numeric", length = length(arg_lst$C))
    ipc_eif_preds <-
    predictiveness_measures <- vector("list", length = arg_lst$K)
    for (k in seq_len(arg_lst$K)) {
      this_arg_lst <- get_test_set(arg_lst, k = k)
      this_est_lst <- estimate_type_predictiveness(this_arg_lst[-unhelpful_indices], type)
      predictiveness_measures[[k]] <- this_est_lst
      eifs[[k]] <- this_est_lst$eif
      eif[arg_lst$folds_Z == k] <- this_est_lst$eif
      ipc_eif_preds[arg_lst$folds_Z == k] <- this_est_lst$ipc_eif_preds
    }
    point_ests <- sapply(1:arg_lst$K, function(k) predictiveness_measures[[k]]$point_est)
    point_est <- mean(point_ests)
    ipc_eif_preds <- ipc_eif_preds
    # estimate the denominator, if necessary
    if (estimate_denominator) {
      do_ipcw <- as.numeric(!all(arg_lst$ipc_weights == 1))
      if (is.null(arg_lst$full_y)) {
        mn_y <- mean(arg_lst$y, na.rm = attr(x, "na.rm"))
      } else {
        mn_y <- mean(arg_lst$full_y, na.rm = attr(x, "na.rm"))
      }
      this_arg_lst <- arg_lst
      this_arg_lst$fitted_values <- rep(mn_y, length(arg_lst$y))
      this_arg_lst$C <- switch(do_ipcw + 1, rep(1, length(arg_lst$C)), arg_lst$C)
      if (grepl("r_squared", full_type)) {
        denominator <- do.call(measure_mse, this_arg_lst[-unhelpful_indices])
      } else {
        denominator <- do.call(measure_cross_entropy, this_arg_lst[-unhelpful_indices])
      }
      eif <- (-1) * as.vector(
        matrix(
          c(1 / denominator$point_est, (-1) * point_est / (denominator$point_est ^ 2)),
          nrow = 1
        ) %*% t(cbind(eif, denominator$eif))
      )
      all_eifs <- lapply(as.list(seq_len(arg_lst$K)), function(i) {
        (-1) * as.vector(
          matrix(
            c(1 / denominator$point_est, (-1) * point_ests[i] / (denominator$point_est ^ 2)),
            nrow = 1
          ) %*% t(cbind(eifs[[i]], denominator$eif[arg_lst$folds_Z == i]))
        )
      })
      point_ests <- 1 - point_ests / denominator$point_est
      eifs <- all_eifs
    }
    x$point_est <- mean(point_ests)
    x$eif <- eif
    x$ipc_eif_preds <- ipc_eif_preds
    x$all_point_ests <- point_ests
    x$all_eifs <- eifs
  }
  return(x)
}
