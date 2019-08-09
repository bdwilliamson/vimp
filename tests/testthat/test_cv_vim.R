context("Test cross-validated difference in R^2")

## load required functions and packages
library("testthat")
library("SuperLearner")
library("gam")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)

## set up a library for SuperLearner
learners <- c("SL.step", "SL.gam", "SL.mean")

test_that("Test cross-validated estimator of ANOVA-based variable importance with single cv folds", {
  est <- cv_vim_nodonsker(Y = y, X = x, indx = 2, V = 5, type = "regression", run_regression = TRUE,
                SL.library = learners, alpha = 0.05)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

test_that("Test cross-validated estimator of ANOVA-based variable importance with double cv folds", {
  est <- cv_vim(Y = y, X = x, indx = 2, V = 5, type = "regression", run_regression = TRUE,
                SL.library = learners, alpha = 0.05)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

## create cv folds
set.seed(4747)
V <- 5
two_folds <- two_validation_set_cv(length(y), V = 5)
one_folds <- rep(seq_len(V), length = length(y))
one_folds <- sample(one_folds)
Y <- matrix(y)
indx <- 2

test_that("Test cross-validated vimp with single cv folds, pre-computed SL", {
  ## get the fitted values by fitting the super learner on each pair
  fhat_ful <- list()
  fhat_red <- list()
  for (v in 1:V) {
     ## fit super learner
     fit <- SuperLearner::SuperLearner(Y = Y[one_folds != v, , drop = FALSE],
      X = x[one_folds != v, , drop = FALSE], SL.library = learners, cvControl = list(V = 5))
     fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
     ## get predictions on the validation fold
     fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit, 
      newdata = x[one_folds == v, , drop = FALSE])$pred
     ## fit the super learner on the reduced covariates
     red <- SuperLearner::SuperLearner(Y = fitted_v,
      X = x[one_folds != v, -indx, drop = FALSE], SL.library = learners, cvControl = list(V = 5))
     ## get predictions on the validation fold
     fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red, 
      newdata = x[one_folds == v, -indx, drop = FALSE])$pred
  }
  est <- cv_vim_nodonsker(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
                          V = 5, folds = one_folds, type = "regression", run_regression = FALSE, alpha = 0.05)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

test_that("Test cross-validated vimp with double cv folds, pre-computed SL", {
  ## get the fitted values by fitting the super learner on each pair
  fhat_ful <- list()
  fhat_red <- list()
  for (v in 1:V) {
      fhat_ful[[v]] <- list()
      fhat_red[[v]] <- list()
      ## fit super learner
      fit <- SuperLearner::SuperLearner(Y = Y[two_folds[, v] == 0, , drop = FALSE],
       X = x[two_folds[, v] == 0, , drop = FALSE], SL.library = learners)
      fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
      ## get predictions on the first validation fold
      fhat_ful[[v]][[1]] <- SuperLearner::predict.SuperLearner(fit,
       newdata = x[two_folds[, v] == 1, , drop = FALSE])$pred
      ## get predictions on the second validation fold
      fhat_ful[[v]][[2]] <- SuperLearner::predict.SuperLearner(fit,
       newdata = x[two_folds[, v] == 2, , drop = FALSE])$pred
      ## fit the super learner on the reduced covariates
      red <- SuperLearner::SuperLearner(Y = fitted_v,
       X = x[two_folds[, v] == 0, -indx, drop = FALSE], SL.library = learners)
      ## get predictions on the first validation fold
      fhat_red[[v]][[1]] <- SuperLearner::predict.SuperLearner(red,
       newdata = x[two_folds[, v] == 1, -indx, drop = FALSE])$pred
      ## get predictions on the second validation fold
      fhat_red[[v]][[2]] <- SuperLearner::predict.SuperLearner(red,
       newdata = x[two_folds[, v] == 2, -indx, drop = FALSE])$pred
  }
  est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
  V = 5, folds = two_folds, type = "regression", run_regression = FALSE, alpha = 0.05)
    expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

test_that("Error messages work", {
  expect_error(cv_vim(X = x))
  expect_error(cv_vim(Y = y))
  expect_error(cv_vim(Y = y, X = x, SL.library = NULL))
  expect_error(cv_vim(Y = y, X = x, run_regression = FALSE))
  expect_error(cv_vim(Y = y, f1 = mean(y)))
  expect_error(cv_vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})