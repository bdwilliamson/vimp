## Resubmission
This is a resubmission. In this version, I have:

* Added new functions vimp_accuracy, vimp_auc, and vimp_rsquared
* Added new function vimp_anova to take the place of vimp_regression (now deprecated)
* Added new function vim; all vimp_ functions call vim under the hood
* Reorganized internals of vim and cv_vim
* Added new functions to measure predictiveness (prefixed with measure)
* Added new functions to compute predictiveness point estimates and influence curve estimates, for cv and non-cv

## Test environments
* local ubuntu 18.04, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.6.0
* windows server 2012 (on appveyor), R 3.5.1
* local windows 10, R 3.4.4
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs; there was one NOTE, that I (Brian D. Williamson) am the package maintainer, and that "et al." may be misspelled in the DESCRIPTION.

## Downstream dependencies
None.