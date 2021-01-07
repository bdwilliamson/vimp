## Resubmission
This is a resubmission. In this version, I have:

* Fixed ERRORs in CRAN checks
* Fixed NOTEs in CRAN checks
* Compute one-step estimators of variable importance if inverse probability of censoring weights are entered. You input the weights, indicator of coarsening, and observed variables, and `vimp` will handle the rest.

## Test environments
* local ubuntu 18.04, R 4.0.2
* ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
* windows server 2012 (on appveyor), R 3.6.3
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD check results
There were no ERRORs or WARNINGs; there was one NOTE, that I (Brian D. Williamson) am the package maintainer, and that "Biometrics", "arXiv", "Feng", and "ICML" may be misspelled in the DESCRIPTION.

## Downstream dependencies
None.
