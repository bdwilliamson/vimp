## Resubmission
This is a resubmission. In this version, I have:

* Changed the under-the-hood behavior of cv_vim.R and cv_vim_nodonsker.R to remove error messages reported by users attempting to set the family to binomial()


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