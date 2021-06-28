# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed
set.seed(4747)
p <- 2
n <- 5e4
x <- replicate(p, stats::rnorm(n, 0, 1))
x_df <- as.data.frame(x)
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- mean((y - mean(y)) ^ 2)
# note that true difference in R-squareds for variable j, under independence, is 
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var

# folds for sample-splitting
folds <- sample(rep(seq_len(2), length = length(y)))
y_1 <- y[folds == 1]
y_2 <- y[folds == 2]
x_1 <- subset(x_df, folds == 1)
x_2 <- subset(x_df, folds == 2)

# set up a library for SuperLearner
learners <- c("SL.glm")
V <- 2

# fit the data with all covariates
set.seed(1234)
full_fit_1 <- SuperLearner::SuperLearner(Y = y_1, X = x_1, 
                         SL.library = learners, cvControl = list(V = V))
full_fitted_1 <- SuperLearner::predict.SuperLearner(full_fit_1)$pred

full_fit_2 <- SuperLearner::SuperLearner(Y = y_2, X = x_2, 
                                         SL.library = learners, cvControl = list(V = V))
full_fitted_2 <- SuperLearner::predict.SuperLearner(full_fit_2)$pred

# fit the data with only X1
reduced_fit_1 <- SuperLearner::SuperLearner(Y = full_fitted_2, 
                              X = x_2[, -2, drop = FALSE], 
                              SL.library = learners, cvControl = list(V = V))
reduced_fitted_1 <- SuperLearner::predict.SuperLearner(reduced_fit_1)$pred

# fit the data with only X2
reduced_fit_2 <- SuperLearner::SuperLearner(Y = full_fitted_1, 
                              X = x_1[, -1, drop = FALSE], 
                              SL.library = learners, cvControl = list(V = V))
reduced_fitted_2 <- SuperLearner::predict.SuperLearner(reduced_fit_2)$pred

set.seed(4747)
test_that("Merging variable importance estimates works", {
  est_1 <- vim(Y = y, f1 = full_fitted_1, f2 = reduced_fitted_1, 
               run_regression = FALSE, indx = 2, type = "r_squared", 
               sample_splitting_folds = folds)
  expect_warning(est_2 <- vim(Y = y, f1 = full_fitted_2, f2 = reduced_fitted_2, 
                              run_regression = FALSE, indx = 1, type = "r_squared", 
                              sample_splitting_folds = folds))

  merged_ests <- merge_vim(est_1, est_2)
  expect_equal(merged_ests$est[1], r2_two, tolerance = 0.2, scale = 1)
  expect_equal(merged_ests$est[2], r2_one, tolerance = 0.4, scale = 1)
  expect_output(print(merged_ests), "Estimate", fixed = TRUE)
})

test_that("Merging cross-validated variable importance estimates works", {
  est_1 <- cv_vim(Y = y, X = x_df, run_regression = TRUE, indx = 2, 
                  V = V, cvControl = list(V = V), SL.library = learners,
                  env = environment(), na.rm = TRUE)
  est_2 <- cv_vim(Y = y, X = x_df, run_regression = TRUE, indx = 1, 
                  V = V, cvControl = list(V = V), SL.library = learners,
                  env = environment(), na.rm = TRUE)

  merged_ests <- merge_vim(est_1, est_2)
  expect_equal(merged_ests$est[1], r2_two, tolerance = 0.1, scale = 1)
  expect_equal(merged_ests$est[2], r2_one, tolerance = 0.1, scale = 1)
  expect_output(print(merged_ests), "Estimate", fixed = TRUE)
})
