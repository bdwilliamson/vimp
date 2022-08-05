# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed -----------
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

# test ANOVA -------------------------------------------------------------------
set.seed(1234)
test_that("ANOVA-based R^2 with old function name works", {
  # check deprecated message
  expect_warning(vimp_regression(Y = y, X = x, run_regression = TRUE,
                                 SL.library = learners,
                                 cvControl = list(V = V), indx = 1, V = V,
                                 env = environment()))
})
set.seed(5678)
test_that("ANOVA-based R^2 with new function name works", {
  expect_warning(est <- vimp_anova(Y = y, X = x, run_regression = TRUE,
                                   SL.library = learners,
                                   cvControl = list(V = V), indx = 2, V = V,
                                   env = environment()))
  # check that the estimate is nearly correct
  expect_equal(est$est, r2_two, tolerance = 0.4, scale = 1)
})
set.seed(91011)
test_that("ANOVA-based R^2 without cross-fitting works", {
  est <- vim(Y = y, X = x, run_regression = TRUE, SL.library = learners,
             cvControl = list(V = V), indx = 2, type = "anova",
             sample_splitting = FALSE)
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
})

# test R-squared ---------------------------------------------------------------
set.seed(121314)
test_that("R^2-based variable importance works", {
  est <- vimp_rsquared(Y = y, X = x, run_regression = TRUE,
                       SL.library = learners, cvControl = list(V = V),
                       indx = 2, V = V, env = environment())
  # check that the estimate is nearly correct
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
})
