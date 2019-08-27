## load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

test_that("Cross-validated variable importance using internally-computed regressions works", {
  est <- cv_vim(Y = y, X = x, indx = 2, V = 5, type = "r_squared", run_regression = TRUE,
                SL.library = learners, alpha = 0.05, cvControl = list(V = 5))
  ## check variable importance estimate
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.05)
  ## check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.05)
  ## check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  ## check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  ## check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  ## check that influence curve worked
  expect_length(est$update, length(y))
})

## set up the folds
indx <- 2
V <- 5
set.seed(4747)
folds <- rep(seq_len(V), length = n)
folds <- sample(folds)
## get the fitted values by fitting the super learner on each pair
fhat_ful <- list()
fhat_red <- list()
Y <- matrix(y)
for (v in 1:V) {
  ## fit super learner
  fit <- SuperLearner::SuperLearner(Y = Y[folds != v, , drop = FALSE],
                                    X = x[folds != v, , drop = FALSE], SL.library = learners, cvControl = list(V = 5))
  fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
  ## get predictions on the validation fold
  fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(fit,
                                                      newdata = x[folds == v, , drop = FALSE])$pred
  ## fit the super learner on the reduced covariates
  red <- SuperLearner::SuperLearner(Y = fitted_v,
                                    X = x[folds != v, -indx, drop = FALSE], SL.library = learners, cvControl = list(V = 5))
  ## get predictions on the validation fold
  fhat_red[[v]] <- SuperLearner::predict.SuperLearner(red,
                                                      newdata = x[folds == v, -indx, drop = FALSE])$pred
}
test_that("Cross-validated variable importance using externally-computed regressions works", {
  est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2,
  V = 5, folds = folds, type = "r_squared", run_regression = FALSE, alpha = 0.05)
  ## check variable importance estimate
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.05)
  ## check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.05)
  ## check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  ## check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  ## check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  ## check that influence curve worked
  expect_length(est$update, length(y))
})

test_that("Measures of predictiveness work", {
  full_rsquared <- cv_predictiveness_point_est(fhat_ful, y, folds = folds, type = "r_squared")
  expect_equal(full_rsquared$point_est, 0.44, tolerance = 0.05)
  expect_length(full_rsquared$all_ests, V)
  full_update <- cv_predictiveness_update(fhat_red, y, folds = folds, type = "r_squared")
  expect_length(full_update$ic, length(y))
  expect_equal(dim(full_update$all_ics)[2], V)
})

test_that("Error messages work", {
  expect_error(cv_vim(X = x))
  expect_error(cv_vim(Y = y))
  expect_error(cv_vim(Y = y, X = x, SL.library = NULL))
  expect_error(cv_vim(Y = y, X = x, run_regression = FALSE))
  expect_error(cv_vim(Y = y, f1 = mean(y)))
  expect_error(cv_vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})
