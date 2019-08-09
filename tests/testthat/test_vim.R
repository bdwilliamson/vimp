context("Test general variable importance estimates")

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
learners <- c("SL.step", "SL.gam", "SL.mean")

test_that("General variable importance estimates using internally-computed fitted values work", {
  est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                SL.library = learners, alpha = 0.05, cvControl = list(V = 5))
  ## check that the estimate is approximately correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.05)
  ## check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate  SE (logit scale) 95% CI", fixed = TRUE)
  ## check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  ## check that influence curve worked
  expect_length(est$update, length(y))
  ## check that warning for hypothesis test works with no splits
  expect_warning(vim(Y = y, X = x, indx = 2, V = 5, type = "r_squared", run_regression = TRUE,
                SL.library = learners, alpha = 0.05))
})

test_that("Hypothesis testing works", {
    folds <-
    est <- vim(Y = y, X = x, SL.library = learners, run_regression = TRUE)
})


test_that("Measures of predictiveness work", {

})

test_that("Error messages work", {
    expect_error(vim(X = x))
    expect_error(vim(Y = y))
    expect_error(vim(Y = y, X = x, SL.library = NULL))
    expect_error(vim(Y = y, X = x, run_regression = FALSE))
    expect_error(vim(Y = y, f1 = mean(y)))
    expect_error(vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})
