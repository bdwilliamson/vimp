# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed
set.seed(4747)
p <- 2
n <- 5e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# get the 'true' SPVIMs
true_var <- 1 + .5 ^ 2 + .75 ^ 2
mse_one <- mean((y - (1 + 0.5 * x[, 1])) ^ 2)
mse_two <- mean((y - (1 + 0.75 * x[, 2])) ^ 2)
mse_full <- mean((y - (1 + 0.5 * x[, 1] + 0.75 * x[, 2])) ^ 2)
# get the true SPVIMs
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var
shapley_val_1 <- (1/2) * (r2_one - 0) + (1/2) * (r2_full - r2_two)
shapley_val_2 <- (1/2) * (r2_two - 0) + (1/2) * (r2_full - r2_one)

# set up a library for SuperLearner
learners <- c("SL.glm")
univariate_learners <- "SL.glm"
V <- 2

set.seed(1234)
test_that("Estimating SPVIMs works", {
  est <- sp_vim(Y = y, X = x, V = V, type = "r_squared",
                SL.library = learners, gamma = .1, alpha = 0.05, delta = 0,
                cvControl = list(V = V), env = environment())
  # check that the estimate is approximately correct
  expect_equal(as.numeric(est$est[2]), shapley_val_1, tolerance = 0.2)
  # check that the SE, CI work
  expect_length(est$ci, 4)
  expect_length(est$se, 3)
  # check that the p-value worked
  expect_length(est$p_value, 2)
  expect_true(est$test[1])
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check the actual point estimates
  # expect_equal(sprintf("%.15f", as.numeric(est$est[2])), 
  #              "0.135665058694294")
})
set.seed(1234)
test_that("Estimating SPVIMs without sample-splitting works", {
  est_no_ss <- sp_vim(Y = y, X = x, V = V, type = "r_squared",
                      SL.library = learners, gamma = .1, alpha = 0.05, delta = 0,
                      sample_splitting = FALSE,
                      cvControl = list(V = V), env = environment())
  # check that the estimate is approximately correct
  expect_equal(as.numeric(est_no_ss$est[2]), shapley_val_1, tolerance = 0.2)
})
set.seed(5678)
test_that("Estimating SPVIMs with special univariate library works", {
  est_uni <- sp_vim(Y = y, X = x, V = V, type = "r_squared",
                    SL.library = learners, 
                    univariate_SL.library = univariate_learners,
                    gamma = .1, alpha = 0.05, delta = 0,
                    cvControl = list(V = V), env = environment())
  # check that the estimate is approximately correct
  expect_equal(as.numeric(est_uni$est[2]), shapley_val_1, tolerance = 0.2)
})
set.seed(91011)
test_that("Estimating SPVIMs with a single library function works", {
  est_single <- sp_vim(Y = y, X = x, V = V, type = "r_squared",
                       SL.library = "SL.glm", 
                       univariate_SL.library = univariate_learners,
                       gamma = .1, alpha = 0.05, delta = 0,
                       cvControl = list(V = V), env = environment())
  # check that the estimate is approximately correct
  expect_equal(as.numeric(est_single$est[2]), shapley_val_1, tolerance = 0.2)
})
set.seed(121314)
test_that("Estimating SPVIMs with verbose = TRUE works", {
  expect_message(est_verbose <- sp_vim(Y = y, X = x, V = V, type = "r_squared",
                                       SL.library = "SL.glm", 
                                       univariate_SL.library = "SL.glm",
                                       gamma = .1, alpha = 0.05, delta = 0,
                                       cvControl = list(V = V), env = environment(), 
                                       verbose = TRUE),
                 "Fitting learners",
                 all = FALSE)
  # check that the estimate is approximately correct
  expect_equal(as.numeric(est_verbose$est[2]), shapley_val_1, tolerance = 0.2)
})


test_that("Error messages work", {
    expect_error(sp_vim(X = x))
    expect_error(sp_vim(Y = y))
    expect_error(sp_vim(Y = y, X = x, SL.library = NULL))
})
