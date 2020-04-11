## load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")
library("xgboost")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)
folds <- sample(rep(seq_len(2), length = length(y)))

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

test_that("General variable importance estimates using internally-computed fitted values work", {
  est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                SL.library = learners, alpha = 0.05, cvControl = list(V = 3),
             env = environment(), folds = folds)
  ## check that the estimate is approximately correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.2, scale = 1)
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
  expect_length(est$update, sum(folds == 1))
})

## fit the data with all covariates
full_fit <- SuperLearner(Y = y[folds == 1], X = x[folds == 1, ], SL.library = learners, cvControl = list(V = 3))
full_fitted <- predict(full_fit)$pred
## fit the data with only X1
reduced_fit <- SuperLearner(Y = full_fitted, X = x[folds == 2, -2, drop = FALSE], SL.library = learners, cvControl = list(V = 3))
reduced_fitted <- predict(reduced_fit)$pred
test_that("General variable importance estimates using externally-computed fitted values work", {
    ## expect a message with no folds
    expect_error(est <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE))
    est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = FALSE, 
               f1 = full_fitted, f2 = reduced_fitted,
             folds = folds)
    ## check that estimate worked
    expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.2, scale = 1)
    ## check that p-value exists
    expect_length(est$p_value, 1)
})


test_that("Measures of predictiveness work", {
  full_rsquared <- predictiveness_point_est(full_fitted, y[folds == 1], type = "r_squared")
  expect_equal(full_rsquared, 0.44, tolerance = 0.1)
  full_update <- predictiveness_update(full_fitted, y[folds == 1], type = "r_squared")
  expect_length(full_update, sum(folds == 1))
})

test_that("Error messages work", {
    expect_error(vim(X = x))
    expect_error(vim(Y = y))
    expect_error(vim(Y = y, X = x, SL.library = NULL))
    expect_error(vim(Y = y, X = x, run_regression = FALSE))
    expect_error(vim(Y = y, f1 = mean(y)))
    expect_error(vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})
test_that("ANOVA-based R^2 with pre-computed fitted values works", {
  expect_warning(est <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2, type = "anova", folds = folds))
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.2, scale = 1)
})
