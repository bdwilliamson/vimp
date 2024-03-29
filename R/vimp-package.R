#' vimp: Perform Inference on Algorithm-Agnostic Intrinsic Variable Importance
#'
#' A unified framework for valid statistical inference on algorithm-agnostic
#' measures of intrinsic variable importance. You provide the data, a method for
#' estimating the conditional mean of the outcome given the covariates,
#' choose a variable importance measure, and specify variable(s) of interest;
#' 'vimp' takes care of the rest.
#'
#' @section Author(s):
#' \bold{Maintainer}: Brian Williamson \url{https://bdwilliamson.github.io/}
#' \bold{Contributors}: Jean Feng \url{https://www.jeanfeng.com}, Charlie Wolock \url{https://cwolock.github.io/}
#'
#' Methodology authors:
#' \itemize{
#'   \item{Brian D. Williamson}
#'   \item{Jean Feng}
#'   \item{Peter B. Gilbert}
#'   \item{Noah R. Simon}
#'   \item{Marco Carone}
#' }
#'
#' @section See Also:
#' Manuscripts:
#' \itemize{
#'   \item{\doi{10.1111/biom.13392} (R-squared-based variable importance)}
#'   \item{\doi{10.1111/biom.13389} (Rejoinder to discussion on R-squared-based variable importance article)}
#'   \item{\url{http://proceedings.mlr.press/v119/williamson20a.html} (general Shapley-based variable importance)}
#'   \item{\doi{10.1080/01621459.2021.2003200} (general variable importance)}
#' }
#'
#' Other useful links:
#' \itemize{
#'   \item{\url{https://bdwilliamson.github.io/vimp/}}
#'   \item{\url{https://github.com/bdwilliamson/vimp}}
#'   \item{Report bugs at \url{https://github.com/bdwilliamson/vimp/issues}}
#' }
#'
#' @section Imports:
#' The packages that we import either make the internal code nice
#' (dplyr, magrittr, tibble, rlang, MASS, data.table), are directly relevant to estimating
#' the conditional mean (SuperLearner) or predictiveness measures (ROCR),
#' or are necessary for hypothesis testing (stats) or confidence intervals (boot, only for bootstrap intervals).
#'
#' We suggest several other packages: xgboost, ranger, gam, glmnet, polspline,
#' and quadprog allow a flexible library of candidate learners in the Super
#' Learner; ggplot2 and cowplot help with plotting variable
#' importance estimates; testthat, WeightedROC, cvAUC, and covr help with unit tests; and
#' knitr, rmarkdown, and tidyselect help with the vignettes and examples.
#'
#' @docType package
#' @name vimp
#' @keywords internal
"_PACKAGE"
