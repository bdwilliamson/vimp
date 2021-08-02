#' Extract sampled-split predictions from a CV.SuperLearner object
#'
#' Use the cross-validated Super Learner and a set of specified sample-splitting
#' folds to extract cross-fitted predictions on separate splits of the data. This
#' is primarily for use in cases where you have already fit a CV.SuperLearner
#' and want to use the fitted values to compute variable importance without having
#' to re-fit. The number of folds used in the CV.SuperLearner must be even.
#'
#' @param cvsl_obj An object of class \code{"CV.SuperLearner"}
#' @param sample_splitting logical; should we use sample-splitting or not?
#'   Defaults to \code{TRUE}.
#' @param sample_splitting_folds A vector of folds to use for sample splitting
#' @param full logical; is this the fit to all covariates (\code{TRUE}) or not
#'   (\code{FALSE})?
#'
#' @seealso \code{\link[SuperLearner]{CV.SuperLearner}} for usage of the
#'   \code{CV.SuperLearner} function.
#' @return The predictions on validation data in each split-sample fold; a
#'   list of length two, each element of which is a list with the predictions
#'   on the split-sample cross-validation data.
#' @export
extract_sampled_split_predictions <- function(cvsl_obj = NULL,
                                              sample_splitting = TRUE,
                                              sample_splitting_folds = NULL,
                                              full = TRUE) {
  if (is.null(cvsl_obj) | !(class(cvsl_obj) == "CV.SuperLearner")) {
    stop("Please enter a CV.SuperLearner object.")
  }
  if (is.null(sample_splitting_folds)) {
    stop("Please enter sample-splitting folds.")
  }
  all_preds <- cvsl_obj$SL.predict
  # extract the folds used for cross-fitting
  cross_fitting_folds <- get_cv_sl_folds(cvsl_obj$folds)
  unique_cf_folds <- unique(cross_fitting_folds)
  # get which sample-splitting fold to use
  this_sample_split <- ifelse(full, 1, 2)
  if (sample_splitting) {
    # use V / 2 for inner cross-fitting within cv_vim;
    # note that if the input V is odd, the code below will still work
    V <- sum(sample_splitting_folds == this_sample_split)
  } else {
    V <- length(unique(cross_fitting_folds))
  }
  lst <- vector("list", length = V)
  these_cf_folds <- sort(unique_cf_folds)[sample_splitting_folds == this_sample_split]
  for (v in 1:V) {
    lst[[v]] <- all_preds[cross_fitting_folds == these_cf_folds[[v]]]
  }
  lst
}

#' Get a numeric vector with cross-validation fold IDs from CV.SuperLearner
#'
#' @param cv_sl_folds The folds from a call to \code{CV.SuperLearner}; a list.
#'
#' @importFrom data.table rbindlist
#' @return A numeric vector with the fold IDs.
#' @export
get_cv_sl_folds <- function(cv_sl_folds) {
  folds_with_row_nums <- sapply(1:length(cv_sl_folds),
                                function(x)
                                  list(
                                    row_nums = cv_sl_folds[[x]],
                                    fold = rep(x, length(cv_sl_folds[[x]]))
                                  ),
                                simplify = FALSE
  )
  folds_df <- data.table::rbindlist(folds_with_row_nums)
  folds_df$fold[order(folds_df$row_nums)]
}
