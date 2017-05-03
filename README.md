## vimp

We have developed a parameter of interest for assessing variable importance nonparametrically. The parameter is 
\begin{equation} T(P) = \int [E(Y | X) - E(Y | X_{(-j)})]^2 dP,\end{equation}
where $X_{(-j)}$ denotes the matrix of covariates with the $j$th column --- or a group of columns --- removed. Thus the parameter measures the "information loss" due to removing the covariate from the model.

We use a Generalized Newton-Raphson One-step estimator to get an estimate, and then use the influence function and results on statistical functionals to develop asymptotically valid confidence intervals.

This method works on low-dimensional and high-dimensional data. The current implementation allows use of \code{np}, \code{loess}, or \code{mgcv} to estimate the regression.

## Motivation

There are not very many methods for assessing variable importance, and there are even fewer that are easily interpretable. In high dimensional settings, the variable importance measure is often inextricably linked with the regression technique. Thus we cannot compare variable importance obtained from two different methods, since these are fundamentally different measures. Our goal was to uncouple the estimation procedure from the variable importance measure, so that we could flexibly estimate a measure that retains its interpretation across estimation procedures. 

We came at this problem from the framework of Targeted Minimum Loss-based Estimation (TMLE), where one first pre-specifies a parameter of interest and then estimates it using properties of influence functions and statistical functionals. The rich theory on these topics yields asymptotically valid confidence intervals, which is notoriously difficult to do in high-dimensional problems. This package accompanies our theoretical results.

## License

GNU GPLv3
