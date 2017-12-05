## vimp

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bdwilliamson/vimp?branch=master&svg=true)](https://ci.appveyor.com/project/bdwilliamson/vimp)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

We have developed a parameter of interest for assessing variable importance nonparametrically. For a true data generating mechanism ![equation](http://latex.codecogs.com/gif.latex?P_0), we define the parameter ![equation](http://latex.codecogs.com/gif.latex?%5CPsi_s%28P_0%29%20%3A%3D%20%5Cfrac%7B%5Cint%20%5C%7BE_%7BP_0%7D%28Y%20%5Cmid%20X%20%3D%20x%29%20-%20E_%7BP_0%7D%28Y%20%5Cmid%20X_%7B%28-s%29%7D%20%3D%20x_%7B%28-s%29%7D%29%5C%7D%5E2%20dP_0%28x%29%7D%7Bvar_%7BP_0%7D%28Y%29%7D%2C) where for a vector ![equation](http://latex.codecogs.com/gif.latex?v) and a set of indices ![equation](http://latex.codecogs.com/gif.latex?r), ![equation](http://latex.codecogs.com/gif.latex?v_r) denotes the elements of ![equation](http://latex.codecogs.com/gif.latex?v) with index in ![equation](http://latex.codecogs.com/gif.latex?r) and ![equation](http://latex.codecogs.com/gif.latex?v_%7B%28-r%29%7D) denotes the elements of ![equation](http://latex.codecogs.com/gif.latex?v) with index not in ![equation](http://latex.codecogs.com/gif.latex?r). The parameter of interest defined above can then be interpreted as the additional proportion of the variability in the outcome explained by including ![equation](http://latex.codecogs.com/gif.latex?X_s) in the conditional mean. This is a generalization of ANOVA-derived variable importance to a nonparametric model. 

The parameter of interest and methods we use to estimate it are explained in more detail in our [tech report](http://biostats.bepress.com/uwbiostat/paper422/).

This method works on low-dimensional and high-dimensional data. 

## Motivation

In a regression setting, it is often of interest to quantify the importance of various features in predicting the response. Commonly, the variable importance measure used is determined by the regression technique employed. For this reason, practitioners often only resort to one of a few regression techniques for which a variable importance measure is naturally defined. Unfortunately, these regression techniques are often sub-optimal for predicting the response. Additionally, because the variable importance measures native to different regression techniques generally have a different interpretation, comparisons across techniques can be difficult. This work studies a novel variable importance measure that can be used with any regression technique, and whose interpretation is agnostic to the technique used. Specifically, we propose a generalization of the ANOVA-derived variable importance measure. Our theoretical results suggest that we can use possibly-complex machine learning techniques to flexibly and efficiently estimate the variable importance of a single feature or group of features, as well as a valid confidence interval.

## R installation

To install the package in R, run the following code:

```r
## install.packages("devtools") # only run this line if necessary
devtools::install_github(repo = "bdwilliamson/vimp")
```

## License

GNU GPLv3
