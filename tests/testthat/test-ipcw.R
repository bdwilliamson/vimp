# load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")

# generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
true_var <- 1 + var((x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2)

# apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)
# get the true SPVIMs
mse_one <- mean((y - ((x[,1]/5)^2*(x[,1]+7)/5 + 25/27))^2)
mse_two <- mean((y - (7/15 + (x[, 2]/3)^2))^2)
mse_full <- mean((y - ((x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2))^2)
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var
shapley_val_1 <- (1/2) * (r2_one - 0) + (1/2) * (r2_full - r2_two)
shapley_val_2 <- (1/2) * (r2_two - 0) + (1/2) * (r2_full - r2_one)

# get the true conditional VIM
true_vim <- (500/729)/(1 + 2497/7875 + 500/729)

# make this a two-phase study, assume that X is only measured on
# subjects in the second phase; note C = 1 is inclusion
C <- rbinom(n, size = 1, prob = exp(y) / (1 + exp(y)))
tmp_x <- x
tmp_x[C == 0, ] <- NA
x <- tmp_x
ipc_weights <- 1 / predict(glm(C ~ y, family = "binomial"), type = "response")

learners <- c("SL.glm.interaction", "SL.ranger", "SL.mean")

set.seed(1234)
# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = 3), env = environment())
  expect_equal(est$est, true_vim, tolerance = 0.2, scale = 1)
})
# test that VIM with IPCW and scale = "logit" works
set.seed(4747)
test_that("VIM with inverse probability of coarsening weights and scale = 'logit' works", {
  est_log <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             scale = "log",
             cvControl = list(V = 3), env = environment())
  expect_equal(est_log$est, true_vim, tolerance = 0.3, scale = 1)
})

set.seed(5678)
# test that VIM with inverse probability of coarsening weights and cross-fitting works
test_that("VIM with inverse probability of coarsening weights and cross-fitting works", {
  est_cv <- cv_vim(Y = y, X = x, indx = 1, type = "r_squared", V = 2, run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = 3), env = environment())
  expect_equal(est_cv$est, true_vim, tolerance = 0.3, scale = 1)
})

univariate_learners <- "SL.polymars"
set.seed(91011)
# test that SPVIM with inverse probability of coarsening weights works
test_that("SPVIM with inverse probability of coarsening weights works", {
  est_spvim <- sp_vim(Y = y, X = x, type = "r_squared", V = 2, SL.library = learners,
                      univariate_SL.library = univariate_learners, gamma = 0.1,
                      alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
                      cvControl = list(V = 2), env = environment())
  expect_equal(est_spvim$est[3], shapley_val_2, tolerance = 0.3, scale = 1)
})
