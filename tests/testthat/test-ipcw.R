# load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
library("data.table")
library("vimp")

# generate the data
set.seed(4747)
p <- 2
n <- 10000
x <- data.frame(replicate(p, stats::runif(n, -5, 5)))

# apply the function to the x's
y <- (x[,1]/5)^2*(x[,1]+7)/5 + (x[,2]/3)^2 + rnorm(n, 0, 1)

# make this a two-phase study, assume that X is only measured on 
# subjects in the second phase; note C = 1 is inclusion
C <- rbinom(n, size = 1, prob = exp(y) / (1 + exp(y)))
tmp_x <- x
tmp_x[C == 0, ] <- NA
x <- tmp_x
ipc_weights <- 1 / predict(glm(C ~ y, family = "binomial"), type = "response")

# set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'reg:squarederror', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

true_vim <- (500/729)/(1 + 2497/7875 + 500/729)

set.seed(1234)
# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "y", ipc_weights = ipc_weights,
             cvControl = list(V = 3))
  expect_equal(est$est, true_vim, tolerance = 0.2, scale = 1)
})

# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, C = C, Z = "y", ipc_weights = ipc_weights,
             cvControl = list(V = 3))
  expect_equal(est$est, true_vim, tolerance = 0.2, scale = 1)
})