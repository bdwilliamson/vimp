# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 2
n <- 5e4
x <- replicate(p, stats::rnorm(n, 0, 1))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
# get the 'true' SPVIMs
true_var <- mean((y - mean(y)) ^ 2)
mse_one <- mean((y - (1 + 0.5 * x[, 1])) ^ 2)
mse_two <- mean((y - (1 + 0.75 * x[, 2])) ^ 2)
mse_full <- mean((y - (1 + 0.5 * x[, 1] + 0.75 * x[, 2])) ^ 2)
# get the true SPVIMs
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var
shapley_val_1 <- (1/2) * (r2_one - 0) + (1/2) * (r2_full - r2_two)
shapley_val_2 <- (1/2) * (r2_two - 0) + (1/2) * (r2_full - r2_one)
# create a binomial outcome, get true AUC
y_bin <- as.numeric(y > 0)
true_auc <- cvAUC::AUC(pnorm(1 + 0.5 * x[, 1] + 0.75 * x[, 2]), y_bin)
# make this a two-phase study, assume that X is only measured on
# subjects in the second phase; note C = 1 is inclusion
C <- rbinom(n, size = 1, prob = exp(y) / (1 + exp(y)))
tmp_x <- x
tmp_x[C == 0, ] <- NA
x <- tmp_x
x_df <- as.data.frame(x)
ipc_weights <- 1 / predict(glm(C ~ y, family = "binomial"), type = "response")

learners <- c("SL.glm", "SL.mean")
V <- 2

# test IPW VIM -----------------------------------------------------------------
set.seed(1234)
# test that VIM with inverse probability of coarsening weights works
test_that("VIM with inverse probability of coarsening weights works", {
  est <- vim(Y = y, X = x_df, indx = 1, type = "r_squared", run_regression = TRUE,
             SL.library = learners, method = "method.CC_LS",
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = V), env = environment())
  expect_equal(est$est, r2_one, tolerance = 0.2, scale = 1)
})
cc <- complete.cases(x_df)
y_cc <- y_bin[cc]
x_cc <- x_df[cc, ]
weights_cc <- ipc_weights[cc]
set.seed(121314)
# test that AUC estimation with the mean works
test_that("IPW AUC estimation with the mean works", {
  sl_fit <- SuperLearner(Y = y_cc, X = x_cc, family = binomial(),
                         SL.library = "SL.mean", obsWeights = weights_cc)
  est_auc <- measure_auc(fitted_values = sl_fit$SL.predict, y = y_cc,
                         full_y = y_bin, C = cc, Z = data.frame(Y = y_bin),
                         ipc_est_type = "ipw",
                         ipc_weights = ipc_weights, ipc_fit_type = "SL",
                         SL.library = "SL.glm", method = "method.CC_LS",)
  expect_equal(est_auc$point_est, 0.5, tolerance = 0.001, scale = 1)
})
set.seed(121314)
# test that AUC estimation with a better learner works
test_that("IPW AUC estimation with a better learner works", {
  expect_warning(sl_fit <- SuperLearner(Y = y_cc, X = x_cc, family = "binomial",
                                        SL.library = "SL.glm", obsWeights = weights_cc))
  est_auc <- measure_auc(fitted_values = sl_fit$SL.predict, y = y_cc,
                         full_y = y_bin, C = cc, Z = data.frame(Y = y_bin),
                         ipc_est_type = "ipw",
                         ipc_weights = ipc_weights, ipc_fit_type = "SL",
                         SL.library = "SL.glm", method = "method.CC_LS",)
  expect_equal(est_auc$point_est, true_auc, tolerance = 0.1, scale = 1)
})

# test IPW CV-VIM --------------------------------------------------------------
# test that VIM with inverse probability of coarsening weights and cross-fitting works
set.seed(5678)
test_that("CV-VIM with inverse probability of coarsening weights works", {
  est_cv <- cv_vim(Y = y, X = x_df, indx = 1, type = "r_squared", V = 2,
                   run_regression = TRUE, SL.library = learners[1], method = "method.CC_LS",
             alpha = 0.05, delta = 0, C = C, Z = "Y", ipc_weights = ipc_weights,
             cvControl = list(V = V), env = environment())
  expect_equal(est_cv$est, r2_one, tolerance = 0.3, scale = 1)
})

univariate_learners <- "SL.glm"
set.seed(91011)

# test IPW SPVIM ---------------------------------------------------------------
# test that SPVIM with inverse probability of coarsening weights works
test_that("SPVIM with inverse probability of coarsening weights works", {
  expect_warning(est_spvim <- sp_vim(Y = y, X = x_df, type = "r_squared", V = 2,
                      SL.library = learners, method = "method.CC_LS",
                      univariate_SL.library = univariate_learners, gamma = 0.1,
                      alpha = 0.05, delta = 0, C = C, Z = "Y",
                      ipc_weights = ipc_weights,
                      cvControl = list(V = 2), env = environment()))
  expect_equal(est_spvim$est[3], shapley_val_2, tolerance = 0.3, scale = 1)
})
# binary SPVIM
set.seed(1234)
n <- 250
W <- runif(n, min = -1,  max = 1)
A <- rbinom(n, size = 1, prob = plogis(W))
Y <- rbinom(n,1,plogis(A * (1 + W + 2*W^2) + sin(5*W)))
propW <- 1/rnorm(n,0.5,0.1)
censoringW <-  runif(n, min = 1,  max = 5)
data <- data.frame(W,A,Y, propW, censoringW)

X_for_vim <- data.frame(X1 = W, X2=A)

set.seed(5678)
test_that("SPVIM with IPW and binary outcome works", {
  expect_warning(est <- sp_vim(Y = Y, X = X_for_vim, V = 2, type = "auc",
                               SL.library = learners,
                               univariate_SL.library = univariate_learners, gamma = 0.1,
                               stratified = TRUE, C = rep(1,nrow(X_for_vim)),
                               cvControl = list(V = 2),
                               ipc_weights = propW*censoringW, ipc_est_type = "ipw",
                               Z = c("Y","X1","X2")))
  expect_equal(est$est[2], 0.1, tolerance = 0.05, scale = 1)
})

