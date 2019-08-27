## load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -1, 1)))
## apply the function to the x's
y <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2 + stats::rnorm(n, 0, 1)

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

## fit the data with all covariates
full_fit <- SuperLearner(Y = y, X = x, SL.library = learners, cvControl = list(V = 3))
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit <- SuperLearner(Y = full_fitted, X = x[, -2, drop = FALSE], SL.library = learners, cvControl = list(V = 3))
reduced_fitted <- predict(reduced_fit)$pred

test_that("ANOVA-based R^2 with pre-computed fitted values and old function name works", {
  ## check deprecated message
  expect_warning(vimp_regression(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2))
})

test_that("ANOVA-based R^2 with pre-computed fitted values works", {
    est <- vimp_anova(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2)
    ## check that the estimate is nearly correct
    expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})

test_that("R^2-based variable importance works", {
  est <- vimp_rsquared(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2)
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
})
