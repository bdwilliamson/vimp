## load required functions and packages
library("testthat")
library("SuperLearner")
library("quadprog")
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
SL.xgboost1 <- function(..., max_depth = 1, ntrees = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntrees = ntrees, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

## fit the data with all covariates
full_fit <- SuperLearner(Y = y, X = x, SL.library = learners, method = "method.CC_LS")
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit <- SuperLearner(Y = full_fitted, X = x[, -2, drop = FALSE], SL.library = learners,
                            method = "method.CC_LS")
reduced_fitted <- predict(reduced_fit)$pred

test_that("Nonparametric R^2 with pre-computed fitted values works", {
  print(full_fit)
  est <- vimp_regression(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2,
                         env = environment())
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
  ## check that the formatted object works
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate")
  ## check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  ## check that influence curve worked
  expect_length(est$update, length(y))
  ## check plotting
  expect_silent(plot(est))
})

test_that("Nonparametric R^2 with internally-computed fitted values works", {
  est <- vimp_regression(Y = y, X = x, SL.library = learners, run_regression = TRUE, indx = 2,
                         env = environment())
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

test_that("Error messages work", {
  expect_error(vimp_regression(X = x))
  expect_error(vimp_regression(Y = y))
  expect_error(vimp_regression(Y = y, X = x, SL.library = NULL))
  expect_error(vimp_regression(Y = y, X = x, run_regression = FALSE))
  expect_error(vimp_regression(Y = y, f1 = mean(y)))
  expect_error(vimp_regression(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})

