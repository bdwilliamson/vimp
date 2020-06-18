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
