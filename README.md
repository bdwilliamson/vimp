## npvi

We have developed a parameter of interest for assessing variable importance nonparametrically. The parameter is 
\[T(P) = \int [E(Y | X) - E(Y | X_{(-j)})]^2 dP,\]
where $X_{(-j)}$ denotes the matrix of covariates with the $j$th column --- or a group of columns --- removed. Thus the parameter measures the "information loss" due to removing the covariate from the model.

We use a Generalized Newton-Raphson One-step estimator to get an estimate, and then use the influence function and results on statistical functionals to develop asymptotically valid confidence intervals.

This method works on low-dimensional and high-dimensional data. The current implementation allows use of \code{np}, \code{loess}, or \code{mgcv} to estimate the regression.

## Motivation

There are not very many methods for assessing variable importance, and there are even fewer that are easily interpretable. We came at this problem from the framework of Targeted Minimum Loss-based Estimation (TMLE), where one first pre-specifies a parameter of interest and then estimates it using properties of influence functions and statistical functionals. The rich theory on these topics yields asymptotically valid confidence intervals, and hence inference, which is notoriously difficult to do in high-dimensional problems. This package accompanies our theoretical results.

## License

GNU GPLv3
