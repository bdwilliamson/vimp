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

## split the data in half
samp <- sample(1:n, n/2, replace = FALSE)
y_samp <- y[samp]
x_samp <- x[samp, ]
y_nsamp <- y[-samp]
x_nsamp <- x[-samp, ]
## set up hypothesis testing folds
folds <- sample(rep(seq_len(2), length = length(samp)))

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'reg:squarederror', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")
V <- 2

## fit the data with all covariates
full_fit_1 <- SuperLearner(Y = y_samp[folds == 1], X = x_samp[folds == 1, ], SL.library = learners, cvControl = list(V = V))
full_fitted_1 <- predict(full_fit_1)$pred

full_fit_2 <- SuperLearner(Y = y_nsamp[folds == 1], X = x_nsamp[folds == 1, ], SL.library = learners, cvControl = list(V = V))
full_fitted_2 <- predict(full_fit_2)$pred

## fit the first split; importance for X2
reduced_fit_1 <- SuperLearner(Y = full_fitted_1, X = x_samp[folds == 2, -2, drop = FALSE], SL.library = learners, cvControl = list(V = V))
reduced_fitted_1 <- predict(reduced_fit_1)$pred

## fit the second split; importance for X2
reduced_fit_2 <- SuperLearner(Y = full_fitted_2, X = x_nsamp[folds == 2, -2, drop = FALSE], SL.library = learners, cvControl = list(V = V))
reduced_fitted_2 <- predict(reduced_fit_2)$pred

test_that("Averaging variable importance estimates works", {
  est_1 <- vim(Y = y_samp, f1 = full_fitted_1, f2 = reduced_fitted_1, run_regression = FALSE, indx = 2, type = "r_squared", folds = folds)
  est_2 <- vim(Y = y_nsamp, f1 = full_fitted_2, f2 = reduced_fitted_2, run_regression = FALSE, indx = 2, type = "r_squared", folds = folds)

  est <- average_vim(est_1, est_2)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.2, scale = 1)
  expect_length(est$mat, 7)
  expect_output(print(est), "Estimate", fixed = TRUE)
})
