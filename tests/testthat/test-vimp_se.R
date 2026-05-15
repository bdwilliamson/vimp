# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))

# generate the data -- note that this is a simple setting, for speed -----------
set.seed(4747)
p <- 3
n <- 1e4
x <- as.data.frame(replicate(p, stats::rnorm(n, 0, 1)))
# apply the function to the covariates
y <- 1 + 0.5 * x[, 1] + 0.75 * x[, 2] + stats::rnorm(n, 0, 1)
true_var <- 1 + .5 ^ 2 + .75 ^ 2
# note that true difference in R-squareds for variable j, under independence, is
# beta_j^2 * var(x_j) / var(y)
r2_one <- 0.5 ^ 2 * 1 / true_var
r2_two <- 0.75 ^ 2 * 1 / true_var
folds <- sample(rep(seq_len(2), length = length(y)))
folds_lst <- lapply(as.list(seq_len(2)), function(i) which(folds == i))

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 2

# fit nuisance regressions -----------------------------------------------------
# fit the data with all covariates and sample-splitting (note V = 2 in CV.SL)
set.seed(4747)
full_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = y, X = x, SL.library = learners,
                                cvControl = list(V = 2, validRows = folds_lst),
                                innerCvControl = list(list(V = V)))
)
full_fitted <- SuperLearner::predict.SuperLearner(full_fit, onlySL = TRUE)$pred
# fit the data with only X1
reduced_fit <- suppressWarnings(
  SuperLearner::CV.SuperLearner(Y = full_fitted,
                                X = x[, -2, drop = FALSE],
                                SL.library = learners,
                                cvControl = list(V = 2, validRows = full_fit$folds),
                                innerCvControl = list(list(V = V)))
)
reduced_fitted <- SuperLearner::predict.SuperLearner(reduced_fit, onlySL = TRUE)$pred

# in this case, using the EIF squared or using crossprod should give exactly the same answer,
# because the data are independent
test_that("Variable importance SEs without cross-fitting or sample-splitting works", {
  set.seed(20260515)
  est_eif_square <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                        SL.library = learners, alpha = 0.05, cvControl = list(V = V),
                        env = environment(), sample_splitting = FALSE, use_square = TRUE)
  set.seed(20260515)
  est_crossprod <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                       SL.library = learners, alpha = 0.05, cvControl = list(V = V),
                       env = environment(), sample_splitting = FALSE)
  expect_equal(est_eif_square$se, est_crossprod$se, tolerance = 0.01, scale = 1)
})

test_that("Variable importance SEs without cross-fitting with sample-splitting works", {
  set.seed(20260515)
  est_eif_square <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                        SL.library = learners, alpha = 0.05, cvControl = list(V = V),
                        env = environment(), sample_splitting = TRUE, use_square = TRUE)
  set.seed(20260515)
  est_crossprod <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
                       SL.library = learners, alpha = 0.05, cvControl = list(V = V),
                       env = environment(), sample_splitting = TRUE)
  expect_equal(est_eif_square$se, est_crossprod$se, tolerance = 0.01, scale = 1)
})

test_that("Variable importance SEs with cross-fitting but without sample-splitting works", {
  set.seed(20260515)
  est_eif_square <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                           run_regression = TRUE, SL.library = learners,
                           alpha = 0.05, delta = 0, cvControl = list(V = V),
                           env = environment(), na.rm = TRUE, sample_splitting = FALSE,
                           use_square = TRUE)
  set.seed(20260515)
  est_crossprod <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                          run_regression = TRUE, SL.library = learners,
                          alpha = 0.05, delta = 0, cvControl = list(V = V),
                          env = environment(), na.rm = TRUE, sample_splitting = FALSE)
  expect_equal(est_eif_square$se, est_crossprod$se, tolerance = 0.01, scale = 1)
})

test_that("Variable importance SEs with cross-fitting and sample-splitting works", {
  set.seed(20260515)
  est_eif_square <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                           run_regression = TRUE, SL.library = learners,
                           alpha = 0.05, delta = 0, cvControl = list(V = V),
                           env = environment(), na.rm = TRUE, use_square = TRUE)
  set.seed(20260515)
  est_crossprod <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                          run_regression = TRUE, SL.library = learners,
                          alpha = 0.05, delta = 0, cvControl = list(V = V),
                          env = environment(), na.rm = TRUE)
  expect_equal(est_eif_square$se, est_crossprod$se, tolerance = 0.01, scale = 1)
})
