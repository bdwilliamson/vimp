# load required functions and packages
library("testthat")
suppressWarnings(library("SuperLearner"))
suppressWarnings(library("dplyr"))
suppressWarnings(library("rsample"))

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
# create clusters
unique_ids <- 1e3
id <- rep(1:unique_ids, each = 10)

# set up a library for SuperLearner
learners <- c("SL.glm", "SL.mean")
V <- 2

# note that we need to set up folds for SL in a smart way
unique_clusters <- unique(id)
cluster_folds <- sample(rep(1:V, length.out = length(unique_clusters)))
fold_ids <- cluster_folds[match(id, unique_clusters)]
# check that this worked
check_folds <- data.frame("id" = id, "foldid" = fold_ids) %>%
  group_by(id) %>%
  summarize(n_folds = n_distinct(foldid)) %>%
  filter(n_folds > 1)
nrow(check_folds)

test_that("Sample splitting with clusters and no cross-fitting works", {
  set.seed(20260515)
  est <- vim(Y = y, X = x, indx = 2, type = "r_squared", run_regression = TRUE,
             SL.library = learners, alpha = 0.05, cvControl = list(V = V, validRows = fold_ids),
             env = environment(), sample_splitting = TRUE, clustered = TRUE,
             cluster_id = id)
  check_ss_folds <- data.frame("id" = id, "foldid" = est$sample_splitting_folds) %>%
    group_by(id) %>%
    summarize(n_folds = n_distinct(foldid)) %>%
    filter(n_folds > 1)
  expect_equal(nrow(check_ss_folds), 0, tolerance = 0.01, scale = 1)
})

test_that("Cross-fitting with clusters and no sample-splitting works", {
  set.seed(20260515)
  est <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                run_regression = TRUE, SL.library = learners,
                alpha = 0.05, delta = 0, cvControl = list(V = V, validRows = fold_ids),
                env = environment(), na.rm = TRUE, sample_splitting = FALSE,
                clustered = TRUE, cluster_id = id)
  check_cf_folds <- data.frame("id" = id, "foldid" = est$cross_fitting_folds) %>%
    group_by(id) %>%
    summarize(n_folds = n_distinct(foldid)) %>%
    filter(n_folds > 1)
  expect_equal(nrow(check_cf_folds), 0, tolerance = 0.01, scale = 1)
})


test_that("Cross-fitting and sample-splitting with clusters works", {
  set.seed(20260515)
  est <- cv_vim(Y = y, X = x, indx = 2, V = V, type = "r_squared",
                run_regression = TRUE, SL.library = learners,
                alpha = 0.05, delta = 0, cvControl = list(V = V, validRows = fold_ids),
                env = environment(), na.rm = TRUE, sample_splitting = TRUE,
                clustered = TRUE, cluster_id = id)
  check_cf_folds <- data.frame("id" = id, "foldid" = est$cross_fitting_folds) %>%
    group_by(id) %>%
    summarize(n_folds = n_distinct(foldid)) %>%
    filter(n_folds > 1)
  expect_equal(nrow(check_cf_folds), 0, tolerance = 0.01, scale = 1)
})
