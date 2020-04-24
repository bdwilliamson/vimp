#' vimp: Perform Inference on Algorithm-Agnostic Variable Importance
#'
#' In predictive modeling applications, it is often of interest to determine
#' the relative contribution of subsets of features in explaining an outcome;
#' this is often called variable importance. It is useful to consider variable importance
#' as a function of the unknown, underlying data-generating mechanism rather than
#' the specific algorithm used to fit the data; viewing variable importance
#' as a population parameter facilitates valid statistical inference, and estimates
#' of this variable importance help towards understanding the population of interest.
#' This package provides functions that compute algorithm-agnostic estimates of population variable importance,
#' along with asymptotically valid confidence intervals for the true importance and
#' hypothesis tests of the null hypothesis of zero importance.
#'
#' @section vimp Functions:
#' The functions vimp_accuracy(), vimp_auc(), vimp_deviance(), and vimp_rsquared() compute
#' point estimates, standard error estimates, confidence intervals, and p-values for null hypothesis testing
#' for variable importance defined using the difference in population classification accuracy,
#' area under the receiver operating characteristic curve (AUC), deviance, and R-squared, respectively.
#' Each of these functions is a wrapper around cv_vim(), a general function that computes cross-fitted estimates of variable importance.
#' The result of each of these functions is an object of class "vim" that contains the results.
#' 
#' The vim() function computes non-cross-fitted estimates of variable importance, and returns an object of class "vim".
#' When machine learning algorithms are used as part of the estimation procedure,
#' we recommend using cv_vim.
#' 
#' The function merge_vim() takes the output of multiple calls to vimp_regression(), and combines the results into a single vim object.
#' The function average_vim() combines independent estimates of the same variable importance parameter, e.g., 
#' from two independent splits of the data.
#' 
#' Other internal functions provide the point estimates, influence function estimates, and 
#' hypothesis tests necessary for a full variable importance analysis.
#' 
#' The function format() formats a vim object for printing; print() prints the results.
#'
#' @docType package
#' @name vimp
NULL
