# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed
set.seed(4747)
p <- 2
n <- 5e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the covariates
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- 1 + .5 ^ 2 + .75 ^ 2
# note that true difference in R-squareds for variable j, under independence, is 
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var

# set up a library for SuperLearner
learners <- c("SL.glm")
V <- 2

set.seed(1234)
test_that("Bootstrap without cross-fitting works", {
  est <- vim(Y = y, X = x, run_regression = TRUE, SL.library = learners, 
             cvControl = list(V = V), indx = 1, type = "r_squared",
             sample_splitting = FALSE, env = environment(), 
             bootstrap = TRUE, b = 100)
  expect_true(est$ci[1] <= r2_one & est$ci[2] >= r2_one)
})
set.seed(4747)
test_that("Bootstrap with cross-fitting works", {
  est <- cv_vim(Y = y, X = x, run_regression = TRUE, SL.library = learners, 
                cvControl = list(V = V), indx = 1, V = V, type = "r_squared",
                sample_splitting = FALSE, env = environment(), 
                bootstrap = TRUE, b = 100, cross_fitted_se = FALSE)
  # check that the estimate is nearly correct
  expect_true(est$ci[1] <= r2_one & est$ci[2] >= r2_one)
})
