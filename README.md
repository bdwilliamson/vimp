# R/`vimp`: inference on algorithm-agnostic variable importance <img src="man/figures/logo.png" align="right" width="120px"/>

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vimp)](https://cran.r-project.org/package=vimp)
[![Travis-CI Build Status](https://travis-ci.org/bdwilliamson/vimp.svg?branch=master)](https://travis-ci.org/bdwilliamson/vimp)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bdwilliamson/vimp?branch=master&svg=true)](https://ci.appveyor.com/project/bdwilliamson/vimp)
[![Coverage status](https://codecov.io/gh/bdwilliamson/vimp/branch/master/graph/badge.svg)](https://codecov.io/github/bdwilliamson/vimp?branch=master)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/vimp)](https://CRAN.R-project.org/package=vimp)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Software author:** [Brian Williamson](https://bdwilliamson.github.io/)

**Methodology authors:** [Brian Williamson](https://bdwilliamson.github.io/), [Peter Gilbert](https://www.fredhutch.org/en/faculty-lab-directory/gilbert-peter.html), [Noah Simon](http://faculty.washington.edu/nrsimon/), [Marco Carone](http://faculty.washington.edu/mcarone/about.html)

**Python package:** https://github.com/bdwilliamson/vimpy

## Introduction

In predictive modeling applications, it is often of interest to determine the relative contribution of subsets of features in explaining an outcome; this is often called variable importance. It is useful to consider variable importance as a function of the unknown, underlying data-generating mechanism rather than the specific predictive algorithm used to fit the data. This package provides functions that, given fitted values from predictive algorithms, compute algorithm-agnostic estimates of population variable importance, along with asymptotically valid confidence intervals for the true importance and hypothesis tests of the null hypothesis of zero importance.

Specifically, the types of variable importance supported by `vimp` include: difference in population classification accuracy, difference in population area under the receiver operating characteristic curve, difference in population deviance, difference in population R-squared.

More detail may be found in our paper on [R-squared-based variable importance](https://doi.org/10.1111/biom.13392) and in our tech reports on [general variable importance](https://arxiv.org/abs/2004.03683) and [general Shapley-based variable importance](https://arxiv.org/abs/2006.09481).

This method works on low-dimensional and high-dimensional data.

## Issues

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/bdwilliamson/vimp/issues).

## R installation

You may install a stable release of `vimp` from [CRAN](https://cran.r-project.org/web/packages/vimp/index.html) via `install.packages("vimp")`. You may also install a stable release of `vimp` from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) by running the following code (replace `v2.1.0` with the tag for the specific release you wish to install):

```r
## install.packages("devtools") # only run this line if necessary
devtools::install_github(repo = "bdwilliamson/vimp@v2.1.0")
```

You may install a development release of `vimp` from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) by running the following code:

```r
## install.packages("devtools") # only run this line if necessary
devtools::install_github(repo = "bdwilliamson/vimp")
```

## Example

This example shows how to use `vimp` in a simple setting with simulated data, using `SuperLearner` to estimate the conditional mean functions and specifying the importance measure of interest as the R-squared-based measure. For more examples and detailed explanation, please see the [vignette](https://bdwilliamson.github.io/vimp/introduction_to_vimp.html).

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
vimp <- vimp_rsquared(Y = y, f1 = full_fit, f2 = reduced_fit, indx = 1, run_regression = FALSE)
```

## Citation

After using the `vimp` package, please cite the following (for R-squared-based variable importance):

```
    @article{williamson2020,
      author={Williamson, BD and Gilbert, PB and Carone, M and Simon, R},
      title={Nonparametric variable importance assessment using machine learning techniques},
      journal={Biometrics},
      year={2020},
      doi={10.1111/biom.13392}
    }
```

or the following (for general variable importance parameters):

```
  @article{williamson2020,
    author={Williamson, BD and Gilbert, PB and Simon, NR and Carone, M},
    title={A unified approach for inference on algorithm-agnostic variable importance},
    journal={arXiv:2004.03683},
    year={2020},
    note={URL: https://arxiv.org/abs/2004.03683}
  }
```

or the following (for Shapley-based variable importance):

```
  @article{williamson2020,
      author={Williamson, BD and Feng, J},
      title={Efficient nonparametric statistical inference on population feature importance using Shapley values},
      journal={arXiv:2006.09481},
      year={2020},
      note={URL: https://arxiv.org/abs/2006.09481}
    }
```

## License

The contents of this repository are distributed under the MIT license. See below for details:

```
MIT License

Copyright (c) [2018-2020] [Brian D. Williamson]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Logo

The logo was created using [hexSticker](https://github.com/GuangchuangYu/hexSticker) and [lisa](https://github.com/tyluRp/lisa). Many thanks to the maintainers of these packages and the [Color Lisa](http://colorlisa.com/) team.
