## Resubmission
This is a resubmission. In this version, I have:

* added function `two_validation_set_cv`, which sets up folds for V-fold cross-validation with two validation sets per fold
* changed the functionality of `cv_vim`: now, the cross-validated naive estimator is computed on a first validation set, while the update for the corrected estimator is computed using the second validation set (both created from `two_validation_set_cv`); this allows for relaxation of the Donsker class conditions necessary for asymptotic convergence of the corrected estimator, while making sure that the initial CV naive estimator is not biased high (due to a higher R^2 on the training data)
* Added a new function, cv_vim_nodonsker.R, which has different operating characteristics than cv_vim.R

## Test environments
* local ubuntu 17.04, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.5.0
* windows server 2012 (on appveyor), R 3.5.1
* local windows 10, R 3.4.4
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs; there was one NOTE, that I (Brian D. Williamson) am the package maintainer, and that "et al." may be misspelled in the DESCRIPTION.

## Downstream dependencies
None.