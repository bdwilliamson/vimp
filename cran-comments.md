## Resubmission
This is a resubmission. In this version, I have:

* Changed all tests to run with a small library rather than SL.gam (handles gam package updates) only 
* Added small unit tests for internal functions

## Test environments
* local ubuntu 18.04, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* windows server 2012 (on appveyor), R 3.6.1
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs; there was one NOTE, that I (Brian D. Williamson) am the package maintainer, and that "et al." may be misspelled in the DESCRIPTION.

## Downstream dependencies
None.