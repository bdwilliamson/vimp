context("test_vimp_regression.R")

## load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)

## set up a library for SuperLearner
learners <- "SL.gam"

## fit the data with all covariates
full_fit <- SuperLearner(Y = y, X = x, SL.library = learners)
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit <- SuperLearner(Y = full_fitted, X = x[, -2, drop = FALSE], SL.library = learners)
reduced_fitted <- predict(reduced_fit)$pred

test_that("Test ANOVA-based variable importance", {
  est <- vimp_regression(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 2)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.02)
})
