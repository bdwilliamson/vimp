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
#' @examples
#' ## run with a small number of folds (for illustration only)
#' library("SuperLearner")
#' library("ranger")
#' n <- 100
#' p <- 2
#' ## generate the data
#' x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
#'
#' ## apply the function to the x's
#' smooth <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2
#'
#' ## generate Y ~ Normal (smooth, 1)
#' y <- as.matrix(smooth + stats::rnorm(n, 0, 1))
#'
#' ## set up a library for SuperLearner
#' learners <- c("SL.mean", "SL.ranger")
#' ## run the Super Learner
#' set.seed(4747)
#' fitted_lst <- run_sl(y, x, V = 2, SL.library = learners, s = 1, 
#' cvControl = list(V = 2))
#'
#' @export
run_sl <- function(Y, X, V, SL.library, s, folds, ...) {
    ## fit the super learner on each full/reduced pair
    if (missing(folds)) {
        ## create folds for cross-fitting
        .make_folds <- function(y, V, stratified = FALSE) {
            folds <- vector("numeric", length(y))
            if (stratified) {
                folds_1 <- rep(seq_len(V), length = sum(y == 1))
                folds_0 <- rep(seq_len(V), length = sum(y == 0))
                folds_1 <- sample(folds_1)
                folds_0 <- sample(folds_0)
                folds[y == 1] <- folds_1
                folds[y == 0] <- folds_0
            } else {
                folds <- rep(seq_len(V), length = length(y))
                folds <- sample(folds)
            }
            return(folds)
        }
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
