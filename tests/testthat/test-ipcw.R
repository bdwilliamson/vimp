# load required functions and packages
library("testthat")
library("SuperLearner")
library("xgboost")
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
C <- sample(0:1, size = n, replace = TRUE)
ipc_weights <- 1 / predict(glm(C ~ y, family = "binomial"), type = "response")

# sample-splitting folds, for hypothesis testing
folds <- sample(rep(seq_len(2), length = length(y)))

# set up a library for SuperLearner
SL.xgboost1 <- function(..., max_depth = 1, ntree = 500, shrinkage = 0.1){
  SL.xgboost(..., objective = 'reg:squarederror', max_depth = max_depth, ntree = ntree, shrinkage = shrinkage)
}
learners <- c("SL.glm.interaction", "SL.xgboost1", "SL.mean")

# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x, indx = 1, type = "r_squared", run_regression = TRUE, SL.library = learners,
             alpha = 0.05, delta = 0, folds = folds, C = C, ipc_weights = ipc_weights,
             ipc_fit_type = "SL", cvControl = list(V = 3))
})