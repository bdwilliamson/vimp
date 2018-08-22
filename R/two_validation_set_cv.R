#' V-fold cross-validation with two validation sets
#'
#' Set up V-fold cross-validation, where rather than the usual train/test split
#' for each fold, now there are two test datasets. In practice, this means that
#' each datum is in the training data V - 2 times, in the first test set once,
#' and in the second test set once. 
#'
#' @param n the sample size
#' @param V the number of folds
#'
#' @return an n by V matrix containing the train/test set 1/test set 2 data for each fold.
#'
#' @details This method is only different from V-fold cross-validation by how much data is used in the training sample, 
#' and the fact that two validation samples are needed. Specifically, in two-validation-set V-fold CV, n/V fewer observations are used in training than in V-fold CV. These n/V observations are used in the second validation set.
#'
#' @examples
#' n <- 100
#' V <- 5
#' ## set up two-validation-set 5-fold CV
#' folds <- two_validation_set_cv(n, V)
#'
#' @export

two_validation_set_cv <- function(n, V){
  ## generate a matrix of zeros
  folds <- matrix(0, nrow = n, ncol = V)
  ## randomly set 2 values in each row to 1 (these are test)
  sample_vec <- c(rep(0, V-2), 1, 2)
  folds_randomized <- t(apply(folds, 1, function(x) sample(sample_vec)))
  return(folds_randomized)
}