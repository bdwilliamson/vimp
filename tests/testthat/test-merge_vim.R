# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data -- note that this is a simple setting, for speed -----------
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
folds_lst <- lapply(as.list(seq_len(2)), function(i) which(folds == i))

# fit nuisance regressions -----------------------------------------------------
# set up a library for SuperLearner
learners <- c("SL.glm")
V <- 2

# fit the data with all covariates
set.seed(1234)
# fit a CV.SL for sample-splitting
full_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = y, X = x_df,
                                SL.library = learners,
                                cvControl = list(V = 2, validRows = folds_lst),
                                innerCvControl = list(list(V = V)))
)
full_fitted <- SuperLearner::predict.SuperLearner(full_fit)$pred

# fit the data with only X1
reduced_fit_1 <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = full_fitted, X = x_df[, -2, drop = FALSE],
                                SL.library = learners,
                                cvControl = list(V = 2, validRows = full_fit$folds),
                                innerCvControl = list(list(V = V)))
)
reduced_fitted_1 <- SuperLearner::predict.SuperLearner(reduced_fit_1)$pred

# fit data with only X2
reduced_fit_2 <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = full_fitted, X = x_df[, -1, drop = FALSE],
                                SL.library = learners,
                                cvControl = list(V = 2, validRows = full_fit$folds),
                                innerCvControl = list(list(V = V)))
)
reduced_fitted_2 <- SuperLearner::predict.SuperLearner(reduced_fit_2)$pred

# test merging -----------------------------------------------------------------
set.seed(4747)
test_that("Merging variable importance estimates works", {
  est_1 <- vim(Y = y, f1 = full_fitted, f2 = reduced_fitted_1,
               run_regression = FALSE, indx = 2, type = "r_squared",
               sample_splitting_folds = folds)
  est_2 <- vim(Y = y, f1 = full_fitted, f2 = reduced_fitted_2,
               run_regression = FALSE, indx = 1, type = "r_squared",
               sample_splitting_folds = folds)

  merged_ests <- merge_vim(est_1, est_2)
  expect_equal(merged_ests$est[1], r2_two, tolerance = 0.05, scale = 1)
  expect_equal(merged_ests$est[2], r2_one, tolerance = 0.05, scale = 1)
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
