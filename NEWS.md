# vimp 2.1.6

## Major changes

None

## Minor changes

* Updated links to DOIs and package vignettes throughout
* Updated all tests in `testthat/` to use `glm` rather than `xgboost` (increases speed)
* Updated all examples to use `glm` rather than `xgboost` or `ranger` (increases speed, even though the regression is now misspecified for the truth)
* Removed `forcats` from vignette

# vimp 2.1.5

## Major changes

None

## Minor changes

* Fixed a bug where if the number of rows in the different folds (for cross-fitting or sample-splitting) differed, the matrix of fold-specific EIFs had the wrong number of rows
* Changes to internals of `measure_accuracy` and `measure_auc` for project-wide consistency
* Update all tests in `testthat/` to not explicitly load `xgboost`

# vimp 2.1.4

## Major changes

None

## Minor changes

* Fixed a bug where if the number of rows in the different folds (for cross-fitting or sample-splitting) differed, the EIF had the wrong number of rows

# vimp 2.1.3

## Major changes

None

## Minor changes

* Compute logit transforms using `stats::qlogis` and `stats::plogis` rather than bespoke functions

# vimp 2.1.2

## Major changes

None

## Minor changes

* Bugfix from 2.1.1.1: compute the correction correctly

# vimp 2.1.1.1

## Major changes

None

## Minor changes

* Allow confidence interval (CI) and inverse probability of coarsening corrections on different scales (e.g., log) to ensure that estimates and CIs lie in the parameter space

# vimp 2.1.1

## Major changes

* Compute one-step estimators of variable importance if inverse probability of censoring weights are entered. You input the weights, indicator of coarsening,  and observed variables, and `vimp` will handle the rest.

## Minor changes

* Created new vignettes "Types of VIMs" and "Using precomputed regression function estimates in `vimp`"
* Updated main vignette to only use `run_regression = TRUE` for simplicity
* Added argument `verbose` to `sp_vim`; if `TRUE`, messages are printed throughout fitting that display progress and `verbose` is passed to `SuperLearner`
* Change names of internal functions from `cv_predictiveness_point_est` and  `predictiveness_point_est` to `est_predictiveness_cv` and `est_predictiveness`, respectively
* Removed functions `cv_predictiveness_update`, `cv_vimp_point_est`, `cv_vimp_update`, `predictiveness_update`, `vimp_point_est`, `vimp_update`; this functionality is now in `est_predictiveness_cv` and `est_predictiveness` (for the `*update*` functions) or directly in `vim` or `cv_vim` (for the `*vimp*` functions)
* Removed functions `predictiveness_se` and `predictiveness_ci` (functionality is now in `vimp_se` and `vimp_ci`, respectively)
* Changed `weights` argument to `ipc_weights`, clarifying that these weights are meant to be used as inverse probability of coarsening (e.g., censoring) weights

# vimp 2.1.0

## Major changes

Added functions `sp_vim`, `sample_subsets`, `spvim_ics`, `spvim_se`; these allow computation of Shapely Population Variable Importance (SPVIM)

## Minor changes

None

# vimp 2.0.2

## Major changes

* Removed functions `sp_vim` and helper functions `run_sl`, `sample_subsets`, `spvim_ics`, `spvim_se`; these will be added in a future release
* Removed function `cv_vim_nodonsker`, since `cv_vim` supersedes this function

## Minor changes

* Modify examples to pass all CRAN checks

# vimp 2.0.1

## Major changes

* Added new function `sp_vim` and helper functions `run_sl`, `sample_subsets`, `spvim_ics`, `spvim_se`; these functions allow computation of the Shapley Population Variable Importance Measure (SPVIM)
* Both `cv_vim` and `vim` now use an outer layer of sample splitting for hypothesis testing
* Added new functions `vimp_auc`, `vimp_accuracy`, `vimp_deviance`, `vimp_rsquared`
* `vimp_regression` is now deprecated; use `vimp_anova` instead
* added new function `vim`; each variable importance function is now a wrapper function around `vim` with the `type` argument filled in
* `cv_vim_nodonsker` is now deprecated; use `cv_vim` instead
* each variable importance function now returns a p-value based on the (possibly conservative) hypothesis test against the null of zero importance (with the exception of `vimp_anova`)
* each variable importance function now returns the estimates of the individual risks (with the exception of `vimp_anova`)
* added new functions to compute measures of predictiveness (and cross-validated measures of predictiveness), along with their influence functions

## Minor changes

* Return tibbles in cv_vim, vim, merge_vim, and average_vim

# vimp 1.1.6

## Major changes

None

## Minor changes

* Changed tests to handle `gam` package update by switching library to `SL.xgboost`, `SL.step`, and `SL.mean`
* Added small unit tests for internal functions

# vimp 1.1.5

## Major changes

None

## Minor changes

* Attempt to handle `gam` package update in unit tests

# vimp 1.1.4

## Major changes

None

## Minor changes

* `cv_vim` and`cv_vim_nodonsker` now return the cross-validation folds used within the function

# vimp 1.1.3

## Major changes

None

## Minor changes

* users may now only specify a `family` for the top-level SuperLearner if `run_regression = TRUE`; in call cases, the second-stage SuperLearner uses a `gaussian` family
* if the SuperLearner chooses `SL.mean` as the best-fitting algorithm, the second-stage regression is now run using the original outcome, rather than the first-stage fitted values


# vimp 1.1.2

## Major changes

* added function `cv_vim_nodonsker`, which computes the cross-validated naive estimator and the update on the same, single, validation fold. This does not allow for relaxation of the Donsker class conditions.

## Minor changes

None

# vimp 1.1.1

## Major changes

* added function `two_validation_set_cv`, which sets up folds for V-fold cross-validation with two validation sets per fold
* changed the functionality of `cv_vim`: now, the cross-validated naive estimator is computed on a first validation set, while the update for the corrected estimator is computed using the second validation set (both created from `two_validation_set_cv`); this allows for relaxation of the Donsker class conditions necessary for asymptotic convergence of the corrected estimator, while making sure that the initial CV naive estimator is not biased high (due to a higher R^2 on the training data)

## Minor changes

None

# vimp 1.1.0

## Major changes

None

## Minor changes

* changed the functionality of `cv_vim`: now, the cross-validated naive estimator is computed on the training data for each fold, while the update for the corrected cross-validated estimator is computed using the test data; this allows for relaxation of the Donsker class conditions necessary for asymptotic convergence of the corrected estimator

# vimp 1.0.0

## Major changes

* removed function `vim`, replaced with individual-parameter functions
* added function `vimp_regression` to match Python package
* `cv_vim` now can compute regression estimators
* renamed all internal functions; these are now `vimp_ci`, `vimp_se`, `vimp_update`, `onestep_based_estimator`
* edited vignette
* added unit tests

# vimp 0.0.3

## Major changes

None

## Minor changes

Bugfixes etc.
