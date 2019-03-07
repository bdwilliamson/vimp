#' Estimate variable importance using a one-step estimator-based approach
#'
#' Compute nonparametric estimates of the variable importance parameter interpreted as the proportion of variability explained by including a group of covariates in the estimation technique.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
onestep_based_estimator <- function(full, reduced, y, type = "anova", na.rm = FALSE) {

  ## first calculate the naive
  if (type == "regression" | type == "anova") {
    naive <- mean((full - reduced) ^ 2, na.rm = na.rm)/mean((y - mean(y, na.rm = na.rm)) ^ 2, na.rm = na.rm)
  } else if (type == "deviance") { 
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    if (is.null(dim(full))) { # assume predicting y = 1
      full_mat <- cbind(1 - full, full)
      reduced_mat <- cbind(1 - reduced, reduced)
    } else if(dim(full)[2] < 2) {
        full_mat <- cbind(1 - full, full)
        reduced_mat <- cbind(1 - reduced, reduced)
    } else {
        full_mat <- full
        reduced_mat <- reduced
    }
    p <- apply(y_mult, 2, mean)
    naive_num <- 2*sum(diag(t(y_mult)%*%log(full_mat/reduced_mat)), na.rm = na.rm)/dim(y_mult)[1]
    naive_denom <- -1*sum(log(p))
    naive <- naive_num/naive_denom
  } else if (type == "r_squared"){
    denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    mse_full <- mean((y - full)^2, na.rm = na.rm)/denom
    mse_reduced <- mean((y - reduced)^2, na.rm = na.rm)/denom
    naive <- (1 - mse_full) - (1 - mse_reduced)
  } else if (type == "auc") {
    full_pred <- ROCR::prediction(predictions = full, labels = y)
    red_pred <- ROCR::prediction(predictions = reduced, labels = y)

    naive_auc_full <- unlist(ROCR::performance(prediction.obj = full_pred, measure = "auc", x.measure = "cutoff")@y.values)
    naive_auc_reduced <- unlist(ROCR::performance(prediction.obj = red_pred, measure = "auc", x.measure = "cutoff")@y.values)
    naive <- naive_auc_full - naive_auc_reduced
  } else if (type == "accuracy") {
    contrib_full <- mean((full > 1/2) != y)
    contrib_reduced <- mean((reduced > 1/2) != y)
    naive <- (1 - contrib_full) - (1 - contrib_reduced)
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }

  ## now add on the mean of the ic, only if type == "regression" or "anova"
  if (type == "regression" | type == "anova") {
    onestep <- naive + mean(vimp_update(full, reduced, y, type, na.rm = na.rm), na.rm = na.rm)  
  } else {
    onestep <- NA
  }  

  ## return
  ret <- c(onestep, naive)
  
  return(ret)
}
