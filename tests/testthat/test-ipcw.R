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
# get the 'true' SPVIMs
true_var <- mean((y - mean(y)) ^ 2)
mse_one <- mean((y - (1 + 0.5 * x[, 1])) ^ 2)
mse_two <- mean((y - (1 + 0.75 * x[, 2])) ^ 2)
mse_full <- mean((y - (1 + 0.5 * x[, 1] + 0.75 * x[, 2])) ^ 2)
# get the true SPVIMs
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var
shapley_val_1 <- (1/2) * (r2_one - 0) + (1/2) * (r2_full - r2_two)
shapley_val_2 <- (1/2) * (r2_two - 0) + (1/2) * (r2_full - r2_one)

# get the true conditional VIM
true_vim <- 0.75 ^ 2 * 1 / true_var

# make this a two-phase study, assume that X is only measured on
# subjects in the second phase; note C = 1 is inclusion
C <- rbinom(n, size = 1, prob = exp(y) / (1 + exp(y)))
tmp_x <- x
tmp_x[C == 0, ] <- NA
x <- tmp_x
ipc_weights <- 1 / predict(glm(C ~ y, family = "binomial"), type = "response")

learners <- c("SL.glm", "SL.mean")
V <- 2

set.seed(1234)
# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, 
             SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = V), env = environment())
  expect_equal(est$est, true_vim, tolerance = 0.2, scale = 1)
})
set.seed(5678)
# test that VIM with inverse probability of coarsening weights and cross-fitting works
test_that("VIM with inverse probability of coarsening weights and cross-fitting works", {
  est_cv <- cv_vim(Y = y, X = x, indx = 1, type = "r_squared", V = 2, 
                   run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = V), env = environment())
  expect_equal(est_cv$est, true_vim, tolerance = 0.3, scale = 1)
})

univariate_learners <- "SL.glm"
set.seed(91011)
# test that SPVIM with inverse probability of coarsening weights works
test_that("SPVIM with inverse probability of coarsening weights works", {
    est_spvim <- sp_vim(Y = y, X = x, type = "r_squared", V = 2, 
                      SL.library = learners,
                      univariate_SL.library = univariate_learners, gamma = 0.1,
                      alpha = 0.05, delta = 0, C = C, Z = "Y", 
                      ipc_weights = ipc_weights,
                      cvControl = list(V = 2), env = environment())
  expect_equal(est_spvim$est[3], shapley_val_2, tolerance = 0.3, scale = 1)
})
