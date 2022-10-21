# vimp 2.3.0

## Major changes

* Predictiveness measures now have their own `S3` class, which makes internal code cleaner and facilitates simpler addition of new predictiveness measures.
* In this version, the default return value of `extract_sampled_split_predictions` is a vector, not a list. This facilitates proper use in the new version of the package.

## Minor changes

* You can now specify `truncate = FALSE` in `vimp_ci`

# vimp 2.2.11

## Major changes

* You can now compute variable importance using the average value under the optimal treatment rule. This includes functions `measure_avg_value` (computes the average value and efficient influence function) and updates to `vim`, `cv_vim`, and `sp_vim`.

## Minor changes

* None

# vimp 2.2.10

## Major changes

* None

## Minor changes

* Specify `method` and `family` for weighted EIF estimation within outer functions (`vim`, `cv_vim`, `sp_vim`) rather than the `measure*` functions. This allows compatibility for binary outcomes.
* Added a vignette for coarsened-data settings.

# vimp 2.2.9

## Major changes

* None

## Minor changes

* Allow for unequal numbers of cross-fitting folds between full and reduced predictiveness

# vimp 2.2.8

## Major changes

* None

## Minor changes

* Return objects in `sp_vim` that are necessary to compute the test statistics

# vimp 2.2.7

## Major changes

* None

## Minor changes

* Allow `parallel` argument to be specified for calls to `CV.SuperLearner` but not for calls to `SuperLearner`

# vimp 2.2.6

## Major changes

* None

## Minor changes

* Allow different types of bootstrap interval (e.g., percentile) to be computed
* More precise documentation for `Z` in coarsened-data settings; allow case-insensitive specification of covariate names/positions when creating `Z`
* `V` defaults to 5 if no cross-fitting folds are specified externally
* More precise documentation for `cross_fitted_f1` and `cross_fitted_f2` in `cv_vim`
* Allow non-list `cross_fitted_f1` and `cross_fitted_f2` in `cv_vim`

# vimp 2.2.5

## Major changes

* None

## Minor changes

* Update how `cv_vim` handles an odd number of outer folds being passed with pre-computed regression function estimates. Now, you can use an odd number of folds (e.g., 5) to estimate the full and reduced regression functions and still obtain cross-validated variable importance estimates.

# vimp 2.2.4

## Major changes

* None

## Minor changes

* Allow for odd number of folds in cross-fit and sampled-split VIM estimation
* Add `vrc01` data as an exported object
* Change dataset for vignettes to `vrc01` data

# vimp 2.2.3

## Major changes

* Updated computation of standard errors. Some of the changes in v2.2.0 (namely, that the efficient influence function can be estimated on the entire dataset regardless of whether or not sample-splitting was requested) do not match with the form of the standard error estimator that we use. In this update, we ensure that independent data are used to estimate both the predictiveness *and* the efficient influence function; however, the nuisance functions may still be estimated on a larger portion of the data than in versions prior to v2.2.0 when cross-fitting is used.

## Minor changes

* Added explicit-value tests for point estimates throughout testthat/
* Harmonized vignettes with new SE computation
* Allow `C` to not be specified in `make_folds`

# vimp 2.2.2

## Major changes

None

## Minor changes

* Increased tolerance for AUC vs CV-AUC

# vimp 2.2.1

## Major changes

* Updated the internals of `measure_auc` to hew more closely to `ROCR` and `cvAUC`, using computational tricks to speed up weighted AUC and EIF computation.

## Minor changes

* Added tests for IPW AUC

# vimp 2.2.0

## Major changes

* Added argument `cross_fitted_se` to `cv_vim` and `sp_vim`; this logical option allows the standard error to be estimated using cross-fitting. This can improve performance in cases where flexible algorithms are used to estimate the full and reduced regressions.
* Added bootstrap-based standard error estimates as an option to both `vim` and `cv_vim`; currently, this option is only available for non-sampled-split calls (i.e., with `sample_splitting = FALSE`)
* Updated sample-splitting behavior to match more closely with theoretical results (and improve power!): namely, that since estimation of the nuisance regression functions (i.e., the regression of outcome on all covariates and outcome on the reduced set of covariates) can be treated as fixed in making inference, sample-splitting is only necessary for evaluating predictiveness. Thus, the final regression functions from a call to `vim` are based on the entire dataset, while the full and reduced predictiveness (`predictiveness_full` and `predictiveness_reduced`, along with the corresponding confidence intervals) is evaluated using separate portions of the data for the full and reduced regressions.
* Added argument `sample_splitting` to `vim`, `cv_vim` and `sp_vim`; if `FALSE`, sample-splitting is not used to estimate predictiveness. Note that we recommend using the default, `TRUE`, in all cases, since inference using `sample_splitting = FALSE` will be invalid for variables with truly null variable importance.
* Updated cross-fitting (also referred to as cross-validation) behavior within `sample_splitting = TRUE` to match more closely with theoretical results (and improve power!). In this case, we first split the data into $2K$ cross-fitting folds, and split these folds equally into two sample-splitting folds. For the nuisance regression using all covariates, for each $k \in \{1, \ldots, K\}$ we set aside the data in sample-splitting fold 1 and cross-fitting fold $k$ [this comprises $1 / (2K)$ of the data]. We train using the remaining observations [comprising $(2K-1)/(2K)$ of the data] not in this testing fold, and we test on the originally withheld data. We repeat for the nuisance regression using the reduced set of covariates, but withhold data in sample-splitting fold 2. This update affects both `cv_vim` and `sp_vim`. If `sample_splitting = FALSE`, then we use standard cross-fitting.

## Minor changes

* Use `>=` in computing the numerator of AUC with inverse probability weights
* Update `roxygen2` documentation for wrappers (`vimp_*`) to inherit parameters and details from `cv_vim` (reduces potential for documentation mismatches)

# vimp 2.1.10

## Major changes
None

## Minor changes

* Automatically determine the `family` if it isn't specified; use `stats::binomial()` if there are only two unique outcome values, otherwise use `stats::gaussian()`

# vimp 2.1.9

## Major changes
None

## Minor changes

* Update sensitivity and specificity to use weak inequalities rather than strict inequalities (better aligns with `cvAUC`)
* Add a test of CV-AUC estimation against `cvAUC`
* Borrow information across folds for empirically estimated quantities (e.g., the outcome variance or probability of a certain class); asymptotically equivalent to the prior procedure, but could result in small-sample differences
* Use fold-specific EIFs for cross-validated SE estimation (again, asymptotically equivalent to the prior procedure, but could result in small-sample differences)

# vimp 2.1.8

## Major changes
None

## Minor changes

* Allow the user to specify either an augmented inverse probability of coarsening (AIPW, the default) estimator in coarsened-at-random settings, or specify an IPW estimator, using new argument `ipc_est_type` (available in `vim`, `cv_vim`, and `sp_vim`; also corresponding wrapper functions for each VIM and corresponding internal estimation functions)

# vimp 2.1.7

## Major changes
None

## Minor changes

* Updated internals so that stratified estimation can be performed in outer regression functions for binary outcomes, but that in the case of two-phase samples the stratification won't be used in any internal regressions with continuous outcomes
* Updated internals to allow stratification on both the outcome and observed status, so that there are sufficient cases per fold for both the phase 1 and phase 2 regressions (only used with two-phase samples)

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
