## load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")
library("xgboost")

## generate the data
set.seed(4747)
p <- 2
n <- 5000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)
folds <- sample(rep(seq_len(2), length = length(y)))
## true vals
true_var <- 1 + var((x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2)
mse_one <- mean((y - ((x[,1]/5)^2*(x[,1]+7)/5 + 25/27))^2)
mse_two <- mean((y - (7/15 + (x[, 2]/3)^2))^2)
mse_full <- mean((y - ((x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2))^2)
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var
shapley_val_1 <- (1/2) * (r2_one - 0) + (1/2) * (r2_full - r2_two)
shapley_val_2 <- (1/2) * (r2_two - 0) + (1/2) * (r2_full - r2_one)

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'reg:squarederror', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.glmnet", "SL.mean")
univariate_learners <- "SL.polymars"
V <- 2

test_that("Estimating SPVIMs works", {
  expect_warning(est <- sp_vim(Y = y, X = x, V = V, type = "r_squared", 
                               SL.library = learners, 
                gamma = .1, alpha = 0.05, delta = 0, 
                cvControl = list(V = V), env = environment()))
  ## check that the estimate is approximately correct
  expect_equal(as.numeric(est$est[2]), shapley_val_1, tolerance = 0.2)
  ## check that the SE, CI work
  expect_length(est$ci, 4)
  expect_length(est$se, 3)
  ## check that the p-value worked
  expect_length(est$p_value, 2)
  expect_true(est$test[1])
  ## check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
})
test_that("Estimating SPVIMs with special univariate library works", {
  est <- sp_vim(Y = y, X = x, V = V, type = "r_squared", 
                SL.library = learners, univariate_SL.library = univariate_learners,
                gamma = .1, alpha = 0.05, delta = 0, 
                cvControl = list(V = V), env = environment())
  ## check that the estimate is approximately correct
  expect_equal(as.numeric(est$est[2]), shapley_val_1, tolerance = 0.2)
})


test_that("Error messages work", {
    expect_error(sp_vim(X = x))
    expect_error(sp_vim(Y = y))
    expect_error(sp_vim(Y = y, X = x, SL.library = NULL))
})
