## Resubmission
This is a resubmission. In this version, I have:

* Changed all tests to run with a small library rather than SL.gam (handles gam package updates) only or SL.xgboost only
* Added small unit tests for internal functions

## Test environments
* local ubuntu 18.04, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.0
* windows server 2012 (on appveyor), R 3.5.1
* local windows 10, R 3.4.4
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs; there was one NOTE, that I (Brian D. Williamson) am the package maintainer, and that "et al." may be misspelled in the DESCRIPTION.

## Downstream dependencies
None.