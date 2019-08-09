# vimp: nonparametric variable importance assessment

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vimp)](https://cran.r-project.org/package=vimp)
[![Travis-CI Build Status](https://travis-ci.org/bdwilliamson/vimp.svg?branch=master)](https://travis-ci.org/bdwilliamson/vimp)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bdwilliamson/vimp?branch=master&svg=true)](https://ci.appveyor.com/project/bdwilliamson/vimp)
[![Coverage status](https://codecov.io/gh/bdwilliamson/vimp/branch/master/graph/badge.svg)](https://codecov.io/github/bdwilliamson/vimp?branch=master)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Software author:** [Brian Williamson](https://bdwilliamson.github.io/)

**Methodology authors:** [Brian Williamson](https://bdwilliamson.github.io/), [Peter Gilbert](https://www.fredhutch.org/en/faculty-lab-directory/gilbert-peter.html), [Noah Simon](http://faculty.washington.edu/nrsimon/), [Marco Carone](http://faculty.washington.edu/mcarone/about.html)

## Introduction

In predictive modeling applications, it is often of interest to determine the relative contribution of subsets of features in explaining an outcome; this is often called variable importance. It is useful to consider variable importance as a function of the unknown, underlying data-generating mechanism rather than the specific predictive algorithm used to fit the data. This package provides functions that, given fitted values from predictive algorithms, compute nonparametric estimates of and variance-based variable importance, along with asymptotically valid confidence intervals for the true importance.

More detail may be found in our [tech report](http://biostats.bepress.com/uwbiostat/paper422/).

This method works on low-dimensional and high-dimensional data.

## Issues

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/bdwilliamson/vimp/issues).

## R installation

You may install a stable release of `vimp` from [CRAN](https://cran.r-project.org/web/packages/vimp/index.html) via `install.packages("vimp")`. You may also install a stable release of `vimp` from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) by running the following code (you may replace `v1.1.3` with the tag for the specific release you wish to install):

```r
## install.packages("devtools") # only run this line if necessary
devtools::install_github(repo = "bdwilliamson/vimp@v1.1.3")
```

You may install a development release of `vimp` from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) by running the following code:

```r
## install.packages("devtools") # only run this line if necessary
devtools::install_github(repo = "bdwilliamson/vimp")
```

## Example

This example shows how to use `vimp` in a simple setting with simulated data, using `SuperLearner` to estimate the conditional mean functions. For more examples and detailed explanation, please see the [vignette](vignettes/introduction_to_vimp.Rmd).

```r
## load required functions and libraries
library("SuperLearner")
library("vimp")
library("xgboost")
library("glmnet")

## -------------------------------------------------------------
## problem setup
## -------------------------------------------------------------
## set up the data
n <- 100
p <- 2
s <- 1 # desire importance for X_1
x <- as.data.frame(replicate(p, runif(n, -1, 1)))
y <- (x[,1])^2*(x[,1]+7/5) + (25/9)*(x[,2])^2 + rnorm(n, 0, 1)

## -------------------------------------------------------------
## preliminary step: estimate the conditional means
## -------------------------------------------------------------
## set up the learner library, consisting of the mean, boosted trees,
## elastic net, and random forest
learner.lib <- c("SL.mean", "SL.xgboost", "SL.glmnet", "SL.randomForest")

## the full conditional mean
full_regression <- SuperLearner::SuperLearner(Y = y, X = x, family = gaussian(), SL.library = learner.lib)
full_fit <- full_regression$SL.predict

## the reduced conditional mean
reduced_regression <- SuperLearner::SuperLearner(Y = full_fit, X = x[, -s, drop = FALSE], family = gaussian(), SL.library = learner.lib)
reduced_fit <- reduced_regression$SL.predict

## -------------------------------------------------------------
## get variable importance!
## -------------------------------------------------------------
## get the variable importance estimate, SE, and CI
vimp <- vimp_anova(Y = y, f1 = full_fit, f2 = reduced_fit, indx = 1, run_regression = FALSE)
```
