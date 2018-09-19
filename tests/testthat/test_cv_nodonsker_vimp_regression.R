context("test_cv_nodonsker_vimp_regression.R")

## load required functions and packages
library("testthat")
library("SuperLearner")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))
## apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)

## set up a library for SuperLearner
learners <- c("SL.gam", "SL.mean")

test_that("Test cross-validated estimator of ANOVA-based variable importance", {
  est <- cv_vim_nodonsker(Y = y, X = x, indx = 2, V = 5, type = "regression", run_regression = TRUE,
                SL.library = learners, alpha = 0.05)
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.05)
})
