#' Estimate a nonparametric risk functional
#'
#' Compute nonparametric estimates of the chosen risk functional.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated variable importance for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
risk_estimator <- function(fitted_values, y, type = "anova", na.rm = FALSE) {

  ## first calculate the naive
  if (type == "regression" | type == "anova") {
    est <- NA
  } else if (type == "deviance") { 
    if (is.null(dim(y))) { # assume that zero is in first column
        y_mult <- cbind(1 - y, y)
    } else {
        y_mult <- y
    }
    if (is.null(dim(fitted_values))) { # assume predicting y = 1
      fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else if(dim(fitted_values)[2] < 2) {
        fitted_mat <- cbind(1 - fitted_values, fitted_values)
    } else {
        fitted_mat <- fitted_values
    }
    p <- apply(y_mult, 2, mean)
    num <- 2*sum(diag(t(y_mult)%*%log(fitted_mat)), na.rm = na.rm)/dim(y_mult)[1]
    denom <- -1*sum(log(p))
    est <- num/denom
  } else if (type == "r_squared"){
    denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    mse <- mean((y - fitted_values)^2, na.rm = na.rm)/denom
    est <- (1 - mse)
  } else if (type == "auc") {
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)

    est <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)
  } else if (type == "accuracy") {
    est <- 1 - mean((fitted_values > 1/2) != y)
  } else if (type == "avg_value") {
    stop("We currently do not support the entered variable importance parameter.")
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }

  ## return
  return(est)
}
