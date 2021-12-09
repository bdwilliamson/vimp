# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 2
n <- 5e4
x <- as.data.frame(replicate(p, sample(0:3, n, replace = TRUE)))
a <- rbinom(n, 1, 0.5 + 0.1 * x[, 1] + .05 * x[, 2])
y <- 1 + a * 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# create the potential outcomes
f <- (1 + 1 * 0.5 * x[, 1] + 0.75 * x[, 2]) > (1 + 0 * 0.5 * x[, 1] + 0.75 * x[, 2])
f1 <- (1 + 1 + 0.75 * x[, 2]) > (1 + 0 + 0.75 * x[, 2])
f2 <- (1 + 1 * 0.5 * x[, 1]) > (1 + 0 * 0.5 * x[, 1])
y_f <- 1 + f * 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
y_f1 <- 1 + f1 * 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
y_f2 <- 1 + f2 * 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# compute the true value
true_avg_value <- mean(y_f)
avg_value_1 <- true_avg_value - mean(y_f1)
avg_value_2 <- true_avg_value - mean(y_f2)

# set up a library for SuperLearner
learners <- c("SL.glm.interaction")
V <- 2

set.seed(1234)
# estimate the outcome regression
q_n <- SuperLearner::SuperLearner(Y = y, X = cbind.data.frame(a = a, x), SL.library = learners,
                                  cvControl = list(V = V))
q_n1 <- SuperLearner::SuperLearner(Y = q_n$SL.predict, X = cbind.data.frame(a = a, x[, -1, drop = FALSE]), SL.library = learners,
                                   cvControl = list(V = V))
q_n2 <- SuperLearner::SuperLearner(Y = q_n$SL.predict, X = cbind.data.frame(a = a, x[, -2, drop = FALSE]), SL.library = learners,
                                   cvControl = list(V = V))
# estimate the optimal rule
f_n <- as.numeric(predict(q_n, newdata = cbind.data.frame(a = 1, x))$pred > 
                    predict(q_n, newdata = cbind.data.frame(a = 0, x))$pred)
f_n1 <- as.numeric(predict(q_n1, newdata = cbind.data.frame(a = 1, x[, -1, drop = FALSE]))$pred > 
                     predict(q_n1, newdata = cbind.data.frame(a = 0, x[, -1, drop = FALSE]))$pred)
f_n2 <- as.numeric(predict(q_n2, newdata = cbind.data.frame(a = 1, x[, -2, drop = FALSE]))$pred > 
                     predict(q_n2, newdata = cbind.data.frame(a = 0, x[, -2, drop = FALSE]))$pred)
# estimate the propensity score
g_n <- SuperLearner::SuperLearner(Y = f_n, X = x, SL.library = learners, cvControl = list(V = V))
g_n1 <- SuperLearner::SuperLearner(Y = f_n1, X = x[, -1, drop = FALSE], SL.library = learners, cvControl = list(V = V))
g_n2 <- SuperLearner::SuperLearner(Y = f_n2, X = x[, -2, drop = FALSE], SL.library = learners, cvControl = list(V = V))
# test average value -----------------------------------------------------------
# estimate the average value
nuisances <- list(f_n = f_n, q_n = predict(q_n, newdata = cbind.data.frame(a = f_n, x))$pred,
                  g_n = predict(g_n, newdata = cbind.data.frame(x))$pred)
est_avg_value <- measure_average_value(nuisance_estimators = nuisances, y = y,
                                       a = a)
test_that("Estimating the average value works", {
  expect_equal(est_avg_value$point_est, true_avg_value, tol = 0.05, scale = 1)
})

nuisances1 <- list(f_n = f_n1, q_n = predict(q_n1, newdata = cbind.data.frame(a = f_n1, x[, -1, drop = FALSE]))$pred,
                   g_n = predict(g_n1, newdata = cbind.data.frame(a = f_n1, x[, -1, drop = FALSE]))$pred)
est_avg_value1 <- measure_average_value(nuisance_estimators = nuisances1, y = y,
                                        a = a)
nuisances2 <- list(f_n = f_n2, q_n = predict(q_n2, newdata = cbind.data.frame(a = f_n2, x[, -2, drop = FALSE]))$pred,
                   g_n = predict(g_n2, newdata = cbind.data.frame(a = f_n2, x[, -2, drop = FALSE]))$pred)
est_avg_value2 <- measure_average_value(nuisance_estimators = nuisances2, y = y,
                                        a = a)
test_that("Estimating the difference in average values works", {
  expect_equal(est_avg_value$point_est - est_avg_value1$point_est, avg_value_1, tol = 0.1, scale = 1)
  expect_equal(est_avg_value$point_est - est_avg_value2$point_est, avg_value_2, tol = 0.1, scale = 1)
})



# test_that("Average-value-based VIM works", {
#   
# })