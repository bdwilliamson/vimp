## load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
library("vimp")

## generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -1, 1)))
## apply the function to the x's
y <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2 + stats::rnorm(n, 0, 1)

## set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'reg:squarederror', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")
V <- 2

test_that("ANOVA-based R^2 with old function name works", {
  ## check deprecated message
  expect_warning(vimp_regression(Y = y, X = x, run_regression = TRUE, SL.library = learners, cvControl = list(V = V), indx = 1, V = V, env = environment()))
})
test_that("ANOVA-based R^2 with new function name works", {
  est <- vimp_anova(Y = y, X = x, run_regression = TRUE, SL.library = learners, cvControl = list(V = V), indx = 2, V = V, env = environment())
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.4, scale = 1)
})

test_that("R^2-based variable importance works", {
  est <- vimp_rsquared(Y = y, X = x, run_regression = TRUE, SL.library = learners, cvControl = list(V = V), indx = 2, V = V, env = environment())
  ## check that the estimate is nearly correct
  expect_equal(est$est, (500/729)/(1 + 2497/7875 + 500/729), tolerance = 0.1, scale = 1)
})
