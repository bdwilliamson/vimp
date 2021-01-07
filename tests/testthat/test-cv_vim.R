# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed
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

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 2

test_that("Cross-validated variable importance using internally-computed regressions works", {
  est <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                run_regression = TRUE, SL.library = learners, 
                alpha = 0.05, delta = 0, cvControl = list(V = V),
                env = environment(), na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  # check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est$eif, sum(est$folds[[1]] == 1))
})

# set up the folds
indx <- 2
Y <- matrix(y)
set.seed(4747)
outer_folds <- rep_len(seq_len(2), dim(Y)[1])
outer_folds <- sample(outer_folds)
inner_folds_1 <- rep_len(seq_len(V), 
                         dim(Y[outer_folds == 1, , drop = FALSE])[1])
inner_folds_1 <- sample(inner_folds_1)
inner_folds_2 <- rep_len(seq_len(V),
                         dim(Y[outer_folds == 2, , drop = FALSE])[1])
inner_folds_2 <- sample(inner_folds_2)
# fit the super learner on each full/reduced pair
fhat_ful <- list()
fhat_red <- list()
for (v in 1:V) {
  # fit super learner
  fit <- SuperLearner::SuperLearner(
    Y = Y[outer_folds == 1, , drop = FALSE][inner_folds_1 != v, , drop = FALSE], 
    X = x[outer_folds == 1, , drop = FALSE][inner_folds_1 != v, , drop = FALSE], 
    SL.library = learners, cvControl = list(V = V)
    )
  fitted_v <- SuperLearner::predict.SuperLearner(fit)$pred
  # get predictions on the validation fold
  fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(
    fit, 
    newdata = x[outer_folds == 1, , drop = FALSE][inner_folds_1 == v, , 
                                                  drop = FALSE]
    )$pred
  # fit the super learner on the reduced covariates
  fit_2 <- SuperLearner::SuperLearner(
    Y = Y[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE], 
    X = x[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, , drop = FALSE], 
    SL.library = learners, cvControl = list(V = V)
    )
  fitted_v_2 <- SuperLearner::predict.SuperLearner(fit_2)$pred
  red <- SuperLearner::SuperLearner(
    Y = fitted_v_2, 
    X = x[outer_folds == 2, , drop = FALSE][inner_folds_2 != v, -indx, drop = FALSE], 
    SL.library = learners, cvControl = list(V = V)
    )
  # get predictions on the validation fold
  fhat_red[[v]] <- SuperLearner::predict.SuperLearner(
    red, 
    newdata = x[outer_folds == 2, , drop = FALSE][inner_folds_2 == v, 
                                                  -indx, drop = FALSE]
    )$pred
}
folds <- list(outer_folds = outer_folds, 
              inner_folds = list(inner_folds_1, inner_folds_2))
test_that("Cross-validated variable importance using externally-computed regressions works", {
  est <- cv_vim(Y = y, f1 = fhat_ful, f2 = fhat_red, indx = 2, 
                delta = 0, V = V, folds = folds, type = "r_squared", 
                run_regression = FALSE, alpha = 0.05, na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  # check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est$eif, sum(outer_folds == 1))
})

test_that("Measures of predictiveness work", {
  full_rsquared <- est_predictiveness_cv(fhat_ful, 
                                         y[outer_folds == 1], 
                                         folds = inner_folds_1, 
                                         type = "r_squared", na.rm = TRUE)
  expect_equal(full_rsquared$point_est, 0.44, tolerance = 0.1, scale = 1)
  expect_length(full_rsquared$all_ests, V)
  expect_length(full_rsquared$eif, sum(outer_folds == 1))
  expect_equal(dim(full_rsquared$all_eifs)[2], V)
})

test_that("Error messages work", {
  expect_error(cv_vim(X = x))
  expect_error(cv_vim(Y = y))
  expect_error(cv_vim(Y = y, X = x, SL.library = NULL))
  expect_error(cv_vim(Y = y, X = x, run_regression = FALSE))
  expect_error(cv_vim(Y = y, f1 = mean(y)))
  expect_error(cv_vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})
