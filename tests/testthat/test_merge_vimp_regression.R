context("Test merging two nonparametric R^2 objects")

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

## fit the data with all covariates
full_fit <- SuperLearner(Y = y, X = x, SL.library = learners)
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit_1 <- SuperLearner(Y = full_fitted, X = x[, -2, drop = FALSE], SL.library = learners)
reduced_fitted_1 <- predict(reduced_fit_1)$pred

## fit the data with only X2
reduced_fit_2 <- SuperLearner(Y = full_fitted, X = x[, -1, drop = FALSE], SL.library = learners)
reduced_fitted_2 <- predict(reduced_fit_2)$pred

test_that("Test merging of ANOVA-based variable importance", {
  est_1 <- vimp_regression(Y = y, f1 = full_fitted, f2 = reduced_fitted_1, run_regression = FALSE, indx = 2)
  est_2 <- vimp_regression(Y = y, f1 = full_fitted, f2 = reduced_fitted_2, run_regression = FALSE, indx = 1)

  merged_ests <- merge_vim(est_1, est_2)
  expect_equal(merged_ests$est[1], (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
  expect_equal(merged_ests$est[2], (2497/7875)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
  expect_output(print(merged_ests), "Estimate  SE          95% CI", fixed = TRUE)
})
