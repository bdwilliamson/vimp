## vimp

We have developed a parameter of interest for assessing variable importance nonparametrically. For a true data generating mechanism $P_0$, we define the parameter 
\[\Psi_s(P_0) := \frac{\int \{E_{P_0}(Y \mid X = x) - E_{P_0}(Y \mid X_{(-s)} = x_{(-s)})\}^2 dP_0(x)}{var_{P_0}(Y)},\]
where for a vector $v$ and a set of indices $r$, $v_r$ denotes the elements of $v$ with index in $r$ and $v_{(-r)}$ denotes the elements of $v$ with index not in $r$. The parameter of interest defined above can then be interpreted as the additional proportion of the variability in the outcome explained by including $X_s$ in the conditional mean. This is a generalization of ANOVA-derived variable importance to a nonparametric model. 

The parameter of interest and methods we use to estimate it are explained in more detail in our [http://biostats.bepress.com/uwbiostat/paper422/ tech report].

This method works on low-dimensional and high-dimensional data. 

## Motivation

In a regression setting, it is often of interest to quantify the importance of various features in predicting the response. Commonly, the variable importance measure used is determined by the regression technique employed. For this reason, practitioners often only resort to one of a few regression techniques for which a variable importance measure is naturally defined. Unfortunately, these regression techniques are often sub-optimal for predicting the response. Additionally, because the variable importance measures native to different regression techniques generally have a different interpretation, comparisons across techniques can be difficult. This work studies a novel variable importance measure that can be used with any regression technique, and whose interpretation is agnostic to the technique used. Specifically, we propose a generalization of the ANOVA-derived variable importance measure. Our theoretical results suggest that we can use possibly-complex machine learning techniques to flexibly and efficiently estimate the variable importance of a single feature or group of features, as well as a valid confidence interval.

## License

GNU GPLv3
