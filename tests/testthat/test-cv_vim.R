# load required functions and packages
library("testthat")
library("SuperLearner")

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 2
n <- 5e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the x's
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- 1 + .5 ^ 2 + .75 ^ 2
# note that true difference in R-squareds for variable j, under independence, is 
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 2

# no sample-splitting ----------------------------------------------------------
set.seed(4747)
test_that("CV-VIM without sample splitting works", {
  est_no_ss <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                      run_regression = TRUE, SL.library = learners, 
                      alpha = 0.05, delta = 0, cvControl = list(V = V),
                      env = environment(), na.rm = TRUE, sample_splitting = FALSE)
  # check variable importance estimate
  expect_equal(est_no_ss$est, r2_two, tolerance = 0.1, scale = 1)
  # expect_equal(sprintf("%.15f", est_no_ss$est), "0.305201823514129")
})

set.seed(1234)
test_that("CV-VIM with no SS and a single algorithm", {
  est_no_ss_single_alg <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                                 run_regression = TRUE, SL.library = "SL.glm", 
                                 alpha = 0.05, delta = 0, cvControl = list(V = V),
                                 env = environment(), na.rm = TRUE, sample_splitting = FALSE)
  # check variable importance estimate
  expect_equal(est_no_ss_single_alg$est, r2_two, tolerance = 0.1, scale = 1)
  # expect_equal(sprintf("%.15f", est_no_ss_single_alg$est), "0.305073651770309")
})

set.seed(4747)
test_that("CV-VIM with no SS, non-cross-fitted SE, multiple algorithms works", {
  est_no_ss_non_cf_se <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                                           run_regression = TRUE, SL.library = learners, 
                                           alpha = 0.05, delta = 0, cvControl = list(V = V),
                                           env = environment(), na.rm = TRUE, 
                                           sample_splitting = FALSE, cross_fitted_se = FALSE)
  # check variable importance estimate
  expect_equal(est_no_ss_non_cf_se$est, r2_two, tolerance = 0.1, scale = 1)
  # expect_equal(sprintf("%.15f", est_no_ss_non_cf_se$est), 
  #              "0.305202533328328")
})

set.seed(5678)
test_that("CV-VIM with no SS, non-cross-fitted SE, single algorithm works", {
  est_no_ss_single_alg_non_cf_se <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                                           run_regression = TRUE, SL.library = "SL.glm", 
                                           alpha = 0.05, delta = 0, cvControl = list(V = V),
                                           env = environment(), na.rm = TRUE, 
                                           sample_splitting = FALSE, cross_fitted_se = FALSE)
  # check variable importance estimate
  expect_equal(est_no_ss_single_alg_non_cf_se$est, r2_two, tolerance = 0.1, scale = 1)
  # expect_equal(sprintf("%.15f", est_no_ss_single_alg_non_cf_se$est), 
               # "0.305288025743816")
})

# cross-fitting and sample-splitting -------------------------------------------
set.seed(101112)
test_that("Cross-validated variable importance using internally-computed regressions works", {
  est <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared", 
                run_regression = TRUE, SL.library = learners, 
                alpha = 0.05, delta = 0, cvControl = list(V = V),
                env = environment(), na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  # check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est$eif, length(y) / 2)
  # check that the point estimate is what it is supposed to be
  # expect_equal(sprintf("%.15f", est$est), "0.307418610446996")
})

set.seed(101112)
test_that("Cross-validated variable importance with odd number of outer folds works", {
  est <- cv_vim(Y = y, X = x, indx = 2, V = 3, type = "r_squared", 
                run_regression = TRUE, SL.library = learners, 
                alpha = 0.05, delta = 0, cvControl = list(V = V),
                env = environment(), na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est$ci, 2)
  expect_length(est$se, 1)
  # check that the p-value worked
  expect_length(est$p_value, 1)
  expect_true(est$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est)[1])
  expect_output(print(est), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_equal(sum(!is.na(est$eif_full)), 24999)
  expect_equal(sum(!is.na(est$eif_redu)), 25001)
  # check that the point estimate is what it is supposed to be
  # expect_equal(sprintf("%.15f", est$est), "0.307418610446996")
})

# using pre-computed regression functions --------------------------------------
# cross-fitted estimates of the full and reduced regressions,
# for point estimate of variable importance.
indx <- 2
Y <- matrix(y)
set.seed(4747)
full_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = Y, X = x, SL.library = learners, cvControl = list(V = 2 * V),
  innerCvControl = list(list(V = V))
))
# use the same cross-fitting folds for reduced
reduced_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = Y, X = x[, -indx, drop = FALSE], SL.library = learners, 
  cvControl = SuperLearner::SuperLearner.CV.control(
    V = 2 * V, validRows = full_cv_fit$folds
  ), 
  innerCvControl = list(list(V = V))
))
# extract the predictions on split portions of the data, for hypothesis testing
cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
set.seed(1234)
sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
full_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = full_cv_fit, sample_splitting = TRUE, 
  sample_splitting_folds = sample_splitting_folds, full = TRUE
)
reduced_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = reduced_cv_fit, sample_splitting = TRUE, 
  sample_splitting_folds = sample_splitting_folds, full = FALSE
)
set.seed(5678)
# refit without cross-fitting (for non-cross-fitted SE estimation)
cf_folds_1 <- sort(unique(cross_fitting_folds))[sample_splitting_folds == 1]
full_ss_folds <- ifelse(cross_fitting_folds %in% cf_folds_1, 1, 2)
full_fit <- SuperLearner::SuperLearner(
  Y = Y[full_ss_folds == 1, ], X = x[full_ss_folds == 1, ], 
  SL.library = learners, cvControl = list(V = V)
)
fhat_ful <- SuperLearner::predict.SuperLearner(full_fit, onlySL = TRUE)$pred
reduced_fit <- SuperLearner::SuperLearner(
  Y = Y[full_ss_folds == 2, ], X = x[full_ss_folds == 2, -indx, drop = FALSE], 
  SL.library = learners, cvControl = list(V = V)
)
fhat_red <- SuperLearner::predict.SuperLearner(reduced_fit, onlySL = TRUE)$pred
test_that("Cross-validated variable importance using externally-computed regressions works", {
  est_prefit <- cv_vim(Y = y, cross_fitted_f1 = full_cv_preds, 
                       cross_fitted_f2 = reduced_cv_preds, indx = 2, delta = 0, V = V, 
                       type = "r_squared", cross_fitting_folds = cross_fitting_folds, 
                       sample_splitting_folds = sample_splitting_folds,
                       run_regression = FALSE, alpha = 0.05, na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est_prefit$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est_prefit$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est_prefit$ci, 2)
  expect_length(est_prefit$se, 1)
  # check that the p-value worked
  expect_length(est_prefit$p_value, 1)
  expect_true(est_prefit$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est_prefit)[1])
  expect_output(print(est_prefit), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est_prefit$eif, length(y) / 2)
  # check the actual value of the point estimate
  # expect_equal(sprintf("%.15f", est_prefit$est), "0.311079577886281")
})

# non-cross-fitted SEs
test_that("Cross-validated variable importance using externally-computed regressions and non-cross-fitted SEs works", {
  est_prefit_no_cf_se <- cv_vim(Y = y, cross_fitted_f1 = full_cv_preds, 
                                cross_fitted_f2 = reduced_cv_preds, f1 = fhat_ful,
                                f2 = fhat_red, indx = 2, delta = 0, V = V, type = "r_squared", 
                                cross_fitting_folds = cross_fitting_folds, 
                                sample_splitting_folds = sample_splitting_folds,
                                run_regression = FALSE, alpha = 0.05, na.rm = TRUE, 
                                cross_fitted_se = FALSE)
  # check variable importance estimate
  expect_equal(est_prefit_no_cf_se$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est_prefit_no_cf_se$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est_prefit_no_cf_se$ci, 2)
  expect_length(est_prefit_no_cf_se$se, 1)
  # check that the p-value worked
  expect_length(est_prefit_no_cf_se$p_value, 1)
  expect_true(est_prefit_no_cf_se$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est_prefit_no_cf_se)[1])
  expect_output(print(est_prefit_no_cf_se), "Estimate", fixed = TRUE)
  # check that influence curve worked
  expect_length(est_prefit_no_cf_se$eif, length(y) / 2)
  # check the actual value of the point estimate
  # expect_equal(sprintf("%.15f", est_prefit_no_cf_se$est), "0.311079577886281")
})

# measures of predictiveness ---------------------------------------------------
test_that("Measures of predictiveness work", {
  k_fold_lst <- make_kfold(cross_fitting_folds, sample_splitting_folds)    
  full_test <- (k_fold_lst$sample_splitting_folds == 1)
  full_rsquared <- est_predictiveness_cv(fitted_values = full_cv_preds, 
                                         y = y[full_test],
                                         full_y = y,
                                         folds = k_fold_lst$full, 
                                         type = "r_squared", na.rm = TRUE)
  expect_equal(full_rsquared$point_est, 0.44, tolerance = 0.1, scale = 1)
  expect_length(full_rsquared$all_ests, V)
  expect_length(full_rsquared$eif, length(cross_fitting_folds) / 2)
  expect_equal(length(full_rsquared$all_eifs), V)
})

# error messages ---------------------------------------------------------------
test_that("Error messages work", {
  expect_error(cv_vim(X = x))
  expect_error(cv_vim(Y = y))
  expect_error(cv_vim(Y = y, X = x, SL.library = NULL))
  expect_error(cv_vim(Y = y, X = x, run_regression = FALSE))
  expect_error(cv_vim(Y = y, f1 = mean(y)))
  expect_error(cv_vim(Y = y, f1 = rep(mean(y), length(y)), f2 = mean(y)))
})

# pre-computed regression functions with an odd number of folds ----------------
# with an odd number of outer folds
indx <- 2
Y <- matrix(y)
set.seed(4747)
full_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = Y, X = x, SL.library = learners, cvControl = list(V = 5),
  innerCvControl = list(list(V = V))
))
# use the same cross-fitting folds for reduced
reduced_cv_fit <- suppressWarnings(SuperLearner::CV.SuperLearner(
  Y = Y, X = x[, -indx, drop = FALSE], SL.library = learners, 
  cvControl = SuperLearner::SuperLearner.CV.control(
    V = 5, validRows = full_cv_fit$folds
  ), 
  innerCvControl = list(list(V = V))
))
# extract the predictions on split portions of the data, for hypothesis testing
cross_fitting_folds <- get_cv_sl_folds(full_cv_fit$folds)
set.seed(1234)
sample_splitting_folds <- make_folds(unique(cross_fitting_folds), V = 2)
full_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = full_cv_fit, sample_splitting = TRUE, 
  sample_splitting_folds = sample_splitting_folds, full = TRUE
)
reduced_cv_preds <- extract_sampled_split_predictions(
  cvsl_obj = reduced_cv_fit, sample_splitting = TRUE, 
  sample_splitting_folds = sample_splitting_folds, full = FALSE
)
test_that("Cross-validated VIM works with externally-computed regressions and an odd number of folds", {
  est_prefit <- cv_vim(Y = y, cross_fitted_f1 = full_cv_preds, 
                       cross_fitted_f2 = reduced_cv_preds, indx = 2, delta = 0, V = 2, 
                       type = "r_squared", cross_fitting_folds = cross_fitting_folds, 
                       sample_splitting_folds = sample_splitting_folds,
                       run_regression = FALSE, alpha = 0.05, na.rm = TRUE)
  # check variable importance estimate
  expect_equal(est_prefit$est, r2_two, tolerance = 0.1, scale = 1)
  # check full predictiveness estimate
  expect_equal(est_prefit$predictiveness_full, 0.44, tolerance = 0.1, scale = 1)
  # check that the SE, CI work
  expect_length(est_prefit$ci, 2)
  expect_length(est_prefit$se, 1)
  # check that the p-value worked
  expect_length(est_prefit$p_value, 1)
  expect_true(est_prefit$test)
  # check that printing, plotting, etc. work
  expect_silent(format(est_prefit)[1])
  expect_output(print(est_prefit), "Estimate", fixed = TRUE)
  # check that influence curve worked; note that we used 5-fold outer CV and 
  # 3 of those went to the full nuisance function
  expect_length(est_prefit$eif, length(y) / 5 * 3)
})