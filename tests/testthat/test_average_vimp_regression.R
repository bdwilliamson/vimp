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

## split the data in half
samp <- sample(1:n, n/2, replace = FALSE)

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

## fit the data with all covariates
full_fit_1 <- SuperLearner(Y = y[samp], X = x[samp, ], SL.library = learners,
                           method = "method.CC_LS")
full_fitted_1 <- predict(full_fit_1)$pred

full_fit_2 <- SuperLearner(Y = y[-samp], X = x[-samp, ], SL.library = learners,
                           method = "method.CC_LS")
full_fitted_2 <- predict(full_fit_2)$pred

## fit the first split; importance for X2
reduced_fit_1 <- SuperLearner(Y = full_fitted_1, X = x[samp, -2, drop = FALSE], SL.library = learners,
                              method = "method.CC_LS")
reduced_fitted_1 <- predict(reduced_fit_1)$pred

## fit the second split; importance for X2
reduced_fit_2 <- SuperLearner(Y = full_fitted_2, X = x[-samp, -2, drop = FALSE], SL.library = learners,
                              method = "method.CC_LS")
reduced_fitted_2 <- predict(reduced_fit_2)$pred

test_that("Test averaging of ANOVA-based variable importance", {
  est_1 <- vimp_regression(Y = y[samp], f1 = full_fitted_1, f2 = reduced_fitted_1, run_regression = FALSE, indx = 2)
  est_2 <- vimp_regression(Y = y[-samp], f1 = full_fitted_2, f2 = reduced_fitted_2, run_regression = FALSE, indx = 2)

  est <- average_vim(est_1, est_2)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1)
  expect_length(est$mat, 4)
  expect_output(print(est), "Estimate")
})
