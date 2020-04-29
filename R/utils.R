#' Utility Functions
#' 
#' Various utility functions used in the vimp package.

#' create folds for cross-fitting
#' @param y the outcome
#' @param V the number of folds
#' @param stratified should the folds be stratified based on the outcome?
#' @return a vector of folds
.make_folds <- function(y, V = 2, stratified = FALSE, probs = rep(1/V, V)) {
  folds <- vector("numeric", length(y))
  if (stratified) {
    folds_1 <- rep(seq_len(V), probs * sum(y == 1))
    folds_0 <- rep(seq_len(V), probs * sum(y == 0))
    folds_1 <- sample(folds_1)
    folds_0 <- sample(folds_0)
    folds[y == 1] <- folds_1
    folds[y == 0] <- folds_0  
  } else {
    folds <- rep(seq_len(V), probs * length(y))
    folds <- sample(folds)
  }
  return(folds)
}

#' Run a Super Learner for the provided subset of features
#'
#' Run a Super Learner for the provided subset of features.
#' @param Y the outcome
#' @param X the covariates
#' @param V the number of folds
#' @param SL.library the library of candidate learners
#' @param s the subset of interest
#' @param folds the CV folds
#' @param ... other arguments to Super Learner
#'
#' @return a list of length V, with the results of predicting on the hold-out data for each v in 1 through V
#'
run_sl <- function(Y, X, V, SL.library, s, folds, ...) {
  ## fit the super learner on each full/reduced pair
  if (missing(folds)) {
    folds <- .make_folds(Y, V = V, stratified = (length(unique(Y)) == 2))
  }
  fhat_ful <- list()
  fhat_red <- list()
  for (v in 1:V) {
    ## fit super learner
    fit <- SuperLearner::SuperLearner(Y = Y[folds != v, , drop = FALSE], X = X[folds != v, s, drop = FALSE], SL.library = SL.library, ...)
    fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
    ## get predictions on the validation fold
    fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, newdata = X[folds == v, s, drop = FALSE])$pred
  }
  return(list(preds = fhat_ful, folds = folds))
}
