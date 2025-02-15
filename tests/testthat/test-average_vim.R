# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 2
n <- 5e4
x <- replicate(p, stats::rnorm(n, 0, 1))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- mean((y - mean(y)) ^ 2)
# note that true difference in R-squareds for variable j, under independence, is 
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var

# split the data in half
samp <- sample(1:n, n/2, replace = FALSE)
y_samp <- y[samp]
x_samp <- x[samp, ]
y_nsamp <- y[-samp]
x_nsamp <- x[-samp, ]
# set up hypothesis testing folds
folds <- sample(rep(seq_len(2), length = length(samp)))

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 2

# average variable importance --------------------------------------------------
# fit the data with all covariates
set.seed(1234)
full_fit_1 <- SuperLearner::SuperLearner(Y = y_samp, 
                                         X = as.data.frame(x_samp),
                           SL.library = learners, cvControl = list(V = V))
full_fitted_1 <- SuperLearner::predict.SuperLearner(full_fit_1)$pred

full_fit_2 <- SuperLearner::SuperLearner(Y = y_nsamp,
                                         X = as.data.frame(x_nsamp), 
                           SL.library = learners, cvControl = list(V = V))
full_fitted_2 <- SuperLearner::predict.SuperLearner(full_fit_2)$pred

# fit the first split; importance for X2
reduced_fit_1 <- SuperLearner::SuperLearner(Y = full_fitted_1, 
                              X = as.data.frame(x_samp[, -2, drop = FALSE]), 
                              SL.library = learners, cvControl = list(V = V))
reduced_fitted_1 <- SuperLearner::predict.SuperLearner(reduced_fit_1)$pred

# fit the second split; importance for X2
reduced_fit_2 <- SuperLearner::SuperLearner(Y = full_fitted_2, 
                              X = as.data.frame(x_nsamp[, -2, drop = FALSE]), 
                              SL.library = learners, cvControl = list(V = V))
reduced_fitted_2 <- SuperLearner::predict.SuperLearner(reduced_fit_2)$pred

test_that("Averaging variable importance estimates works", {
  est_1 <- vim(Y = y_samp, f1 = full_fitted_1, f2 = reduced_fitted_1, 
               run_regression = FALSE, indx = 2, type = "r_squared", 
               sample_splitting = FALSE)
  est_2 <- vim(Y = y_nsamp, f1 = full_fitted_2, f2 = reduced_fitted_2, 
               run_regression = FALSE, indx = 2, type = "r_squared", 
               sample_splitting = FALSE)

  est <- average_vim(est_1, est_2)
  expect_equal(est$est, r2_two, 
               tolerance = 0.2, scale = 1)
  expect_length(est$mat, 7)
  expect_output(print(est), "Estimate", fixed = TRUE)
})
