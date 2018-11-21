context("test_vimp_deviance.R")

## load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")

## generate the data
set.seed(4747)
f <- function(x) 0.5 + 0.3*x[1] + 0.2*x[2]
t <- c(0.09723093, 0.04629081)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -1, 1)))
smooth <- apply(x, 1, function(z) f(z))
y <- matrix(stats::rbinom(n, size = 1, prob = smooth))

## set up a library for SuperLearner
learners <- "SL.gam"

## fit the data with all covariates
full_fit <- SuperLearner(Y = y, X = x, SL.library = learners, family = "binomial")
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit <- SuperLearner(Y = y, X = x[, -1, drop = FALSE], SL.library = learners, family = "binomial")
reduced_fitted <- predict(reduced_fit)$pred

test_that("Test AUC-based variable importance", {
  est <- vimp_deviance(Y = y, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 1)
  expect_equal(est$est, t[1], tolerance = 0.02)
})
