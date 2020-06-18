## load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
library("vimp")

## generate the data
make_y <- function(b, p) rbinom(b, 1, p)
make_x <- function(b, mu_0, mu_1, sigma, y) {
  n_col <- ifelse(!is.null(dim(mu_0)), dim(mu_0)[2], length(mu_0))
  x <- matrix(0, nrow = b, ncol = n_col)
  x[y == 0, ] <- MASS::mvrnorm(n = sum(y == 0), mu = mu_0, Sigma = sigma)
  x[y == 1, ] <- MASS::mvrnorm(n = sum(y == 1), mu = mu_1, Sigma = sigma)
  if (n_col == 1) {
    x <- cbind(x, stats::rnorm(b, 0, 1))
  }
  return(x)
}
gen_data <- function(a, mu_0, mu_1, sigma, p, j) {
  ## create y
  y <- make_y(a, p)
  # create x
  # x <- make_x(a)
  x <- make_x(a, mu_0, mu_1, sigma, y)
  red_x <- x[, -j]

  return(list(x = x, red_x = red_x, y = y, j = j))
}
mu_0 <- matrix(c(0, 0), nrow = 1)
mu_1 <- matrix(c(1.5, 2), nrow = 1)
Sigma <- diag(1, nrow = 2)
t_acc <- c(0.05110135, 0.1158337)
t_dev <- c(0.14293485, 0.3001422)
t_auc <- c(0.04011305, 0.1058621)
n <- 10000
set.seed(4747)
dat <- gen_data(n, mu_0, mu_1, Sigma, p = 0.6, j = 1)
y <- dat$y
x <- as.data.frame(dat$x)
folds <- sample(rep(seq_len(2), length = length(y)))

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'binary:logistic', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")
V <- 2

## fit the data with all covariates
full_fit <- SuperLearner(Y = y[folds == 1], X = x[folds == 1, ], SL.library = learners, family = "binomial", cvControl = list(V = V))
full_fitted <- predict(full_fit)$pred

## fit the data with only X1
reduced_fit <- SuperLearner(Y = y[folds == 2], X = x[folds == 2, -1, drop = FALSE], SL.library = learners, family = "binomial", cvControl = list(V = V))
reduced_fitted <- predict(reduced_fit)$pred

est <- vimp_accuracy(Y = y, X = x, run_regression = TRUE, SL.library = learners, indx = 1, V = V, family = "binomial",
                     env = environment())
test_that("Accuracy-based variable importance works", {
  expect_equal(est$est, t_acc[1], tolerance = 0.1, scale = 1)
  
  est_noncv <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 1, type = "accuracy", folds = folds)
  expect_equal(est_noncv$est, t_acc[1], tolerance = 0.1, scale = 1)
})

test_that("AUC-based variable importance works", {
  est <- vimp_auc(Y = y, X = x, f1 = est$full_fit, f2 = est$red_fit, folds = est$folds, run_regression = FALSE, indx = 1, V = V,
                  env = environment())
  expect_equal(est$est, t_auc[1], tolerance = 0.1, scale = 1)
  
  est_noncv <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 1, type = "auc", folds = folds)
  expect_equal(est_noncv$est, t_auc[1], tolerance = 0.1, scale = 1)
})

test_that("Deviance-based variable importance works", {
  est <- vimp_deviance(Y = y, X = x, f1 = est$full_fit, f2 = est$red_fit, folds = est$folds, run_regression = FALSE, indx = 1, V = V,
                       env = environment())
  expect_equal(est$est, t_dev[1], tolerance = 0.1, scale = 1)
  
  est_noncv <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted, run_regression = FALSE, indx = 1, type = "deviance", folds = folds)
  expect_equal(est_noncv$est, t_dev[1], tolerance = 0.1, scale = 1)
})

test_that("Measures of predictiveness work", {
 full_auc <- predictiveness_point_est(full_fitted, y[folds == 1], type = "auc")
 expect_equal(full_auc, 0.96, tolerance = 0.1, scale = 1)
 weighted_auc <- predictiveness_point_est(full_fitted, y[folds == 1], weights = 0.5 * y[folds == 1] + (1 - y[folds == 1]), type = "auc")
 expect_equal(weighted_auc, 0.96, tolerance = 0.1, scale = 1)
 full_acc <- predictiveness_point_est(full_fitted, y[folds == 1], type = "accuracy")
 expect_equal(full_acc, 0.9, tolerance = 0.1, scale = 1)
 full_dev <- predictiveness_point_est(full_fitted, y[folds == 1], type = "deviance")
 expect_equal(full_dev, -0.34, tolerance = 0.1, scale = 1)
 full_ce <- predictiveness_point_est(full_fitted, y[folds == 1], type = "cross_entropy")
 expect_equal(full_ce, -0.5, tolerance = 0.1, scale = 1)
}) 
