# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed -----------
# make continuous data
set.seed(4747)
p <- 2
n <- 5e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the covariates
y_continuous <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- 1 + .5 ^ 2 + .75 ^ 2
# note that true difference in R-squareds for variable j, under independence, is
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var
mse_full <- 1
r_squared_full <- 1 - mse_full / true_var

# make binary data
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
  # create y
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
accuracy_full <- 0.927
auc_full <- 0.984
cross_entropy_full <- -0.163
deviance_full <- 0.764
n <- 5e4
set.seed(4747)
dat <- gen_data(n, mu_0, mu_1, Sigma, p = 0.6, j = 1)
y_binary <- dat$y
x_binary <- as.data.frame(dat$x)
# make data for average value
set.seed(4747)
x_av <- as.data.frame(replicate(p, sample(0:3, n, replace = TRUE)))
a <- rbinom(n, 1, 0.5 + 0.1 * x[, 1] + .05 * x[, 2])
y_av <- 1 + a * 0.5 * x_av[, 1] + 0.75 * x_av[, 2] + stats::rnorm(n, 0, 1)
# create the potential outcomes
f <- (1 + 1 * 0.5 * x_av[, 1] + 0.75 * x_av[, 2]) > (1 + 0 * 0.5 * x_av[, 1] + 0.75 * x_av[, 2])
f1 <- (1 + 1 + 0.75 * x_av[, 2]) > (1 + 0 + 0.75 * x_av[, 2])
f2 <- (1 + 1 * 0.5 * x_av[, 1]) > (1 + 0 * 0.5 * x_av[, 1])
y_f <- 1 + f * 0.5 * x_av[, 1] + 0.75 * x_av[, 2] + stats::rnorm(n, 0, 1)
y_f1 <- 1 + f1 * 0.5 * x_av[, 1] + 0.75 * x_av[, 2] + stats::rnorm(n, 0, 1)
y_f2 <- 1 + f2 * 0.5 * x_av[, 1] + 0.75 * x_av[, 2] + stats::rnorm(n, 0, 1)
# compute the true value
average_value_full <- mean(y_f)

# set up a library for SuperLearner
learners <- c("SL.glm")
V <- 2

# folds for cross-fitting
set.seed(1234)
folds_c <- make_folds(y = y_continuous, V = 2)
folds_b <- make_folds(y = y_binary, V = 2, stratified = TRUE)
sl_folds_c <- lapply(as.list(1:2), function(i) which(folds_c == i))
sl_folds_b <- lapply(as.list(1:2), function(i) which(folds_b == i))

# fit nuisance regressions -----------------------------------------------------
# for continuous y
set.seed(4747)
full_fit_c <- SuperLearner::SuperLearner(Y = y_continuous, X = x,
                                         SL.library = learners,
                                         cvControl = list(V = V))
full_fitted_c <- SuperLearner::predict.SuperLearner(full_fit_c, onlySL = TRUE)$pred
# also a cross-fitted version
full_fit_c_cv <- SuperLearner::CV.SuperLearner(Y = y_continuous, X = x,
                                               SL.library = learners,
                                               cvControl = list(validRows = sl_folds_c, V = 2),
                                               innerCvControl = list(list(V = V)))
full_fitted_c_cv <- full_fit_c_cv$SL.predict
# for binary y
set.seed(4747)
full_fit_b <- SuperLearner::SuperLearner(Y = y_binary, X = x_binary,
                                         SL.library = learners,
                                         cvControl = list(V = V),
                                         family = "binomial")
full_fitted_b <- SuperLearner::predict.SuperLearner(full_fit_b, onlySL = TRUE)$pred
# also a cross-fitted version
full_fit_b_cv <- SuperLearner::CV.SuperLearner(Y = y_binary, X = x_binary,
                                               SL.library = learners,
                                               cvControl = list(validRows = sl_folds_b, V = 2),
                                               innerCvControl = list(list(V = V)),
                                               family = "binomial")
full_fitted_b_cv <- full_fit_b_cv$SL.predict

# for the average value
set.seed(1234)
# estimate the outcome regression
q_n <- SuperLearner::SuperLearner(Y = y_av, X = cbind.data.frame(a = a, x_av), SL.library = learners,
                                  cvControl = list(V = V))
# estimate the optimal rule
f_n <- as.numeric(predict(q_n, newdata = cbind.data.frame(a = 1, x_av))$pred >
                    predict(q_n, newdata = cbind.data.frame(a = 0, x_av))$pred)
# estimate the propensity score
g_n <- SuperLearner::SuperLearner(Y = f_n, X = x_av, SL.library = learners, cvControl = list(V = V))
nuisances <- list(f_n = f_n, q_n = predict(q_n, newdata = cbind.data.frame(a = f_n, x_av))$pred,
                  g_n = predict(g_n, newdata = cbind.data.frame(x_av))$pred)

# test R-squared, MSE ----------------------------------------------------------
# MSE
test_that("MSE using S3 class works", {
  full_mse_object <- predictiveness_measure(type = "mse", y = y_continuous,
                                            fitted_values = full_fitted_c, full_y = y_continuous)
  full_mse <- estimate(full_mse_object)
  expect_equal(full_mse$point_est, mse_full, tolerance = 0.1)
  expect_length(full_mse$eif, length(y_continuous))
})
test_that("MSE works (est_predictiveness)", {
  full_mse <- est_predictiveness(full_fitted_c, y_continuous,
                                 type = "mse")
  expect_equal(full_mse$point_est, mse_full, tolerance = 0.1)
  expect_length(full_mse$eif, length(y_continuous))
})
test_that("Cross-validated MSE using S3 class works", {
  full_mse_object_cv <- predictiveness_measure(type = "mse", y = y_continuous,
                                               fitted_values = full_fitted_c_cv,
                                               cross_fitting_folds = folds_c,
                                               full_y = y_continuous)
  full_mse_cv <- estimate(full_mse_object_cv)
  expect_equal(full_mse_cv$point_est, mse_full, tolerance = 0.1)
  expect_length(full_mse_cv$eif, length(y_continuous))
})


# R-squared
test_that("R-squared using S3 class works", {
  full_r_squared_object <- predictiveness_measure(type = "r_squared", y = y_continuous,
                                            fitted_values = full_fitted_c, full_y = y_continuous)
  full_r_squared <- estimate(full_r_squared_object)
  expect_equal(full_r_squared$point_est, r_squared_full, tolerance = 0.1)
  expect_length(full_r_squared$eif, length(y_continuous))
})
test_that("R-squared works (est_predictiveness)", {
  full_rsquared <- est_predictiveness(full_fitted_c, y_continuous,
                                      type = "r_squared")
  expect_equal(full_rsquared$point_est, r_squared_full, tolerance = 0.1)
  expect_length(full_rsquared$eif, length(y_continuous))
})
test_that("Cross-validated R-squared using S3 class works", {
  full_r_squared_object_cv <- predictiveness_measure(type = "r_squared", y = y_continuous,
                                               fitted_values = full_fitted_c_cv,
                                               cross_fitting_folds = folds_c,
                                               full_y = y_continuous)
  full_r_squared_cv <- estimate(full_r_squared_object_cv)
  expect_equal(full_r_squared_cv$point_est, r_squared_full, tolerance = 0.1)
  expect_length(full_r_squared_cv$eif, length(y_continuous))
})

# test AUC, accuracy, deviance, cross-entropy ----------------------------------
# accuracy
test_that("Accuracy using S3 class works", {
  full_accuracy_object <- predictiveness_measure(type = "accuracy", y = y_binary,
                                                 fitted_values = full_fitted_b, full_y = y_binary)
  full_accuracy <- estimate(full_accuracy_object)
  expect_equal(full_accuracy$point_est, accuracy_full, tolerance = 0.1)
  expect_length(full_accuracy$eif, length(y_binary))
})
test_that("Accuracy works (est_predictiveness)", {
  full_accuracy <- est_predictiveness(full_fitted_b, y_binary,
                                      type = "accuracy")
  expect_equal(full_accuracy$point_est, accuracy_full, tolerance = 0.1)
  expect_length(full_accuracy$eif, length(y_binary))
})
test_that("Cross-validated accuracy using S3 class works", {
  full_accuracy_object_cv <- predictiveness_measure(type = "accuracy", y = y_binary,
                                                     fitted_values = full_fitted_b_cv,
                                                     cross_fitting_folds = folds_b,
                                                     full_y = y_binary)
  full_accuracy_cv <- estimate(full_accuracy_object_cv)
  expect_equal(full_accuracy_cv$point_est, accuracy_full, tolerance = 0.1)
  expect_length(full_accuracy_cv$eif, length(y_binary))
})

# AUC
test_that("AUC using S3 class works", {
  full_auc_object <- predictiveness_measure(type = "auc", y = y_binary,
                                            fitted_values = full_fitted_b, full_y = y_binary)
  full_auc <- estimate(full_auc_object)
  expect_equal(full_auc$point_est, auc_full, tolerance = 0.1)
  expect_length(full_auc$eif, length(y_binary))
})
test_that("AUC works (est_predictiveness)", {
  full_auc <- est_predictiveness(full_fitted_b, y_binary,
                                      type = "auc")
  expect_equal(full_auc$point_est, auc_full, tolerance = 0.1)
  expect_length(full_auc$eif, length(y_binary))
})
test_that("Cross-validated AUC using S3 class works", {
  full_auc_object_cv <- predictiveness_measure(type = "auc", y = y_binary,
                                                    fitted_values = full_fitted_b_cv,
                                                    cross_fitting_folds = folds_b,
                                                    full_y = y_binary)
  full_auc_cv <- estimate(full_auc_object_cv)
  expect_equal(full_auc_cv$point_est, auc_full, tolerance = 0.1)
  expect_length(full_auc_cv$eif, length(y_binary))
})

# Cross-entropy
test_that("Cross Entropy using S3 class works", {
  full_cross_entropy_object <- predictiveness_measure(type = "cross_entropy", y = y_binary,
                                            fitted_values = full_fitted_b, full_y = y_binary)
  full_cross_entropy <- estimate(full_cross_entropy_object)
  expect_equal(full_cross_entropy$point_est, cross_entropy_full, tolerance = 0.1)
  expect_length(full_cross_entropy$eif, length(y_binary))
})
test_that("Cross Entropy works (est_predictiveness)", {
  full_cross_entropy <- est_predictiveness(full_fitted_b, y_binary,
                                      type = "cross_entropy")
  expect_equal(full_cross_entropy$point_est, cross_entropy_full, tolerance = 0.1)
  expect_length(full_cross_entropy$eif, length(y_binary))
})
test_that("Cross-validated cross entropy using S3 class works", {
  full_cross_entropy_object_cv <- predictiveness_measure(type = "cross_entropy", y = y_binary,
                                                    fitted_values = full_fitted_b_cv,
                                                    cross_fitting_folds = folds_b,
                                                    full_y = y_binary)
  full_cross_entropy_cv <- estimate(full_cross_entropy_object_cv)
  expect_equal(full_cross_entropy_cv$point_est, cross_entropy_full, tolerance = 0.1)
  expect_length(full_cross_entropy_cv$eif, length(y_binary))
})


# Deviance
test_that("Deviance using S3 class works", {
  full_deviance_object <- predictiveness_measure(type = "deviance", y = y_binary,
                                            fitted_values = full_fitted_b, full_y = y_binary)
  full_deviance <- estimate(full_deviance_object)
  expect_equal(full_deviance$point_est, deviance_full, tolerance = 0.15)
  expect_length(full_deviance$eif, length(y_binary))
})
test_that("Deviance works (est_predictiveness)", {
  full_deviance <- est_predictiveness(full_fitted_b, y_binary,
                                      type = "deviance")
  expect_equal(full_deviance$point_est, deviance_full, tolerance = 0.15)
  expect_length(full_deviance$eif, length(y_binary))
})
test_that("Cross-validated deviance using S3 class works", {
  full_deviance_object_cv <- predictiveness_measure(type = "deviance", y = y_binary,
                                                    fitted_values = full_fitted_b_cv,
                                                    cross_fitting_folds = folds_b,
                                                    full_y = y_binary)
  full_deviance_cv <- estimate(full_deviance_object_cv)
  expect_equal(full_deviance_cv$point_est, deviance_full, tolerance = 0.15)
  expect_length(full_deviance_cv$eif, length(y_binary))
})


# test average value -----------------------------------------------------------
test_that("Average Value using S3 class works", {
  full_average_value_object <- predictiveness_measure(type = "average_value", y = y_av,
                                                      a = a, full_y = y_av,
                                                      fitted_values = nuisances$q_n,
                                                      nuisance_estimators = nuisances)
  full_average_value <- estimate(full_average_value_object)
  expect_equal(full_average_value$point_est, average_value_full, tolerance = 0.1)
  expect_length(full_average_value$eif, length(y_av))
})
test_that("Average value works (est_predictiveness)", {
  average_value <- est_predictiveness(fitted_values = nuisances$q_n, y = y_av,
                                      a = a,
                                      nuisance_estimators = nuisances,
                                      type = "average_value")
  expect_equal(average_value$point_est, average_value_full, tolerance = 0.1)
  expect_length(average_value$eif, length(y_av))
})
