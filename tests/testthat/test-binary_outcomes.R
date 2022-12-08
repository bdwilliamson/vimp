# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data ------------------------------------------------------------
make_y <- function(b, p) rbinom(b, 1, p)
make_x <- function(b, mu_0, mu_1, sigma, y) {
  n_col <- ifelse(!is.null(dim(mu_0)), dim(mu_0)[2], length(mu_0))
  x <- matrix(0, nrow = b, ncol = n_col)
  x[y == 0, ] <- MASS::mvrnorm(n = sum(y == 0), mu = mu_0, Sigma = sigma)
  x[y == 1, ] <- MASS::mvrnorm(n = sum(y == 1), mu = mu_1, Sigma = sigma)
  if (n_col == 1) {
    x <- cbind(x, stats::rnorm(b, 0, 1))
  }
  return(x)
}
gen_data <- function(a, mu_0, mu_1, sigma, p, j) {
  # create y
  y <- make_y(a, p)
  # create x
  # x <- make_x(a)
  x <- make_x(a, mu_0, mu_1, sigma, y)
  red_x <- x[, -j]

  return(list(x = x, red_x = red_x, y = y, j = j))
}
mu_0 <- matrix(c(0, 0), nrow = 1)
mu_1 <- matrix(c(1.5, 2), nrow = 1)
Sigma <- diag(1, nrow = 2)
t_acc <- c(0.05110135, 0.1158337)
t_dev <- c(0.14293485, 0.3001422)
t_auc <- c(0.04011305, 0.1058621)
n <- 10000
set.seed(4747)
dat <- gen_data(n, mu_0, mu_1, Sigma, p = 0.6, j = 1)
y <- dat$y
x <- as.data.frame(dat$x)
folds <- sample(rep(seq_len(2), length = length(y)))
folds_lst <- lapply(as.list(seq_len(2)), function(i) which(folds == i))
indx <- 1

# estimate nuisance functions --------------------------------------------------
# set up a library for SuperLearner
learners <- c("SL.glm")
V <- 2

# fit the data with all covariates
set.seed(5678)
full_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = y, X = x,
                                          SL.library = learners,
                                          family = "binomial",
                                          cvControl = list(V = 2, validRows = folds_lst),
                                          innerCvControl = list(list(V = V)))
)
full_fitted <- SuperLearner::predict.SuperLearner(full_fit)$pred

# fit the data with only X1
reduced_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = y, X = x[, -indx, drop = FALSE],
                                             SL.library = learners,
                                             family = "binomial",
                                             cvControl = list(V = 2, validRows = folds_lst),
                                             innerCvControl = list(list(V = V)))
)
reduced_fitted <- SuperLearner::predict.SuperLearner(reduced_fit)$pred

# test accuracy ----------------------------------------------------------------
set.seed(1234)
est_acc <- vimp_accuracy(Y = y, X = x, run_regression = TRUE,
                         SL.library = learners, indx = 1, V = V,
                         family = "binomial", env = environment())
test_that("Accuracy-based variable importance works", {
  expect_equal(est_acc$est, t_acc[1], tolerance = 0.1, scale = 1)

  est_acc_noncv <- vim(Y = y, X = x, f1 = full_fitted,
                       f2 = reduced_fitted, run_regression = FALSE,
                       indx = 1, type = "accuracy", sample_splitting_folds = folds)
  expect_equal(est_acc_noncv$est, t_acc[1], tolerance = 0.1, scale = 1)
})

# test AUC ---------------------------------------------------------------------
set.seed(5678)
test_that("AUC-based variable importance works", {
  est_auc <- vimp_auc(Y = y, X = x, cross_fitted_f1 = est_acc$full_fit,
                      cross_fitted_f2 = est_acc$red_fit,
                      cross_fitting_folds = est_acc$cross_fitting_folds,
                      sample_splitting_folds = est_acc$sample_splitting_folds,
                      run_regression = FALSE, indx = 1, V = V,
                      env = environment())
  expect_equal(est_auc$est, t_auc[1], tolerance = 0.1, scale = 1)

  est_auc_noncv <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted,
                       run_regression = FALSE, indx = 1, type = "auc",
                       sample_splitting_folds = folds)
  expect_equal(est_auc_noncv$est, t_auc[1], tolerance = 0.1, scale = 1)
})

# test deviance ----------------------------------------------------------------
set.seed(91011)
test_that("Deviance-based variable importance works", {
  est_dev <- vimp_deviance(Y = y, X = x, cross_fitted_f1 = est_acc$full_fit,
                           cross_fitted_f2 = est_acc$red_fit,
                           cross_fitting_folds = est_acc$cross_fitting_folds,
                           sample_splitting_folds = est_acc$sample_splitting_folds,
                           run_regression = FALSE, indx = 1,
                           V = V, env = environment())
  expect_equal(est_dev$est, t_dev[1], tolerance = 0.1, scale = 1)

  est_dev_noncv <- vim(Y = y, X = x, f1 = full_fitted, f2 = reduced_fitted,
                       run_regression = FALSE, indx = 1, type = "deviance",
                       sample_splitting_folds = folds)
  expect_equal(est_dev_noncv$est, t_dev[1], tolerance = 0.1, scale = 1)
})

# test measures of predictiveness ----------------------------------------------
test_that("Measures of predictiveness work", {
 auc_lst <- est_predictiveness(full_fitted, y,
                                type = "auc")
 full_auc <- auc_lst$point_est
 expect_equal(full_auc, 0.96, tolerance = 0.1, scale = 1)
 full_acc <- est_predictiveness(full_fitted, y,
                                type = "accuracy")$point_est
 expect_equal(full_acc, 0.9, tolerance = 0.1, scale = 1)
 full_dev <- est_predictiveness(full_fitted, y,
                                type = "deviance")$point_est
 expect_equal(full_dev, 0.63, tolerance = 0.1, scale = 1)
 full_ce <- est_predictiveness(full_fitted, y,
                               type = "cross_entropy")$point_est
 expect_equal(full_ce, -0.24, tolerance = 0.1, scale = 1)
})

# check against cvAUC ----------------------------------------------------------
set.seed(1234)
full_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = y, X = x, SL.library = learners, cvControl = list(V = V),
  innerCvControl = list(list(V = V))
))
full_cv_mean_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = y, X = x, SL.library = "SL.mean", cvControl = list(V = V),
  innerCvControl = list(list(V = V))
))
full_cv_preds <- full_cv_fit$SL.predict
full_cv_mean_preds <- full_cv_mean_fit$SL.predict
cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
test_that("vimp and cvAUC agree", {
  auc_lst <- est_predictiveness_cv(fitted_values = full_cv_preds, y = y,
                                   full_y = y, folds = cross_fitting_folds,
                                   type = "auc")
  auc_lst_mean <- est_predictiveness_cv(fitted_values = full_cv_mean_preds, y = y,
                                        full_y = y, folds = cross_fitting_folds,
                                        type = "auc")
  full_auc <- auc_lst$point_est
  cvauc_var <- mean(unlist(lapply(auc_lst$all_eifs,
                                  function(x) mean(x ^ 2))))
  cvauc_var_mean <- mean(unlist(lapply(auc_lst_mean$all_eifs,
                                  function(x) mean(x ^ 2))))
  cvauc_se <- sqrt(cvauc_var / length(y))
  cvauc_se_mean <- sqrt(cvauc_var_mean / length(y))
  auc_ci <- vimp_ci(full_auc, cvauc_se)
  cvauc_lst <- cvAUC::ci.cvAUC(predictions = full_cv_fit$SL.predict,
                               labels = y,
                               folds = full_cv_fit$folds)
  cvauc_lst_mean <- cvAUC::ci.cvAUC(predictions = full_cv_mean_fit$SL.predict,
                                    labels = y,
                                    folds = full_cv_mean_fit$folds)
  expect_equal(full_auc, cvauc_lst$cvAUC, tolerance = 1e-10, scale = 1)
  expect_equal(cvauc_se, cvauc_lst$se, tolerance = 1e-10, scale = 1)
  expect_equal(auc_lst_mean$point_est, cvauc_lst_mean$cvAUC, tolerance = 1e-2, scale = 1)
  expect_equal(cvauc_se_mean, cvauc_lst_mean$se, tolerance = 1e-2, scale = 1)
  # sprintf("%.16f", cvauc_se)
  # sprintf("%.16f", cvauc_lst$se)
  # sprintf("%.16f", cvauc_se_mean)
  # sprintf("%.16f", cvauc_lst_mean$se)
})
