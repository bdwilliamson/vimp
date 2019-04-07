#' Estimate the influence function for an estimator of risk
#'
#' Estimate the influence function for the given risk parameter.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param type which risk parameter are you estimating (defaults to \code{r_squared}, for the $R^2$)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return The influence function values for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
risk_update <- function(fitted_values, y, type = "r_squared", na.rm = FALSE) {
  
  ## calculate the necessary pieces for the influence curve
  if (type == "regression" | type == "anova") {
    ic_update <- NA
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
    d_risk <- 2*rowSums(y_mult*log(fitted_mat), na.rm = na.rm) - num
    ## influence function of the denominator
    d_denom <- rowSums(-1/p*((y_mult == 1) - p))
  } else if (type == "r_squared") {
    denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    mse <- mean((y - fitted_values)^2, na.rm = na.rm)/denom
    num <- mean((y - fitted_values)^2, na.rm = na.rm)
    d <- (y - fitted_values)^2 - mse
    d_denom <- (y - mean(y, na.rm = na.rm))^2 - denom
    d_risk <- (-1)*d 
  } else if (type == "auc") {
    if (!is.null(dim(fitted_values))) { # remove dimension if only dim 1
      if (dim(fitted_values)[2] == 1) {
        fitted_values <- as.vector(fitted_values)
        y <- as.vector(y)
      }
    }
    p_0 <- mean(y == 0)
    p_1 <- mean(y == 1)
    
    preds <- ROCR::prediction(predictions = fitted_values, labels = y)
    
    sens <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 0] < x)))
    spec <- unlist(lapply(as.list(fitted_values), function(x) mean(fitted_values[y == 1] > x)))
        
    contrib_1 <- (y == 1)/p_1*sens
    contrib_0 <- (y == 0)/p_0*spec
    naive_auc <- unlist(ROCR::performance(prediction.obj = preds, measure = "auc", x.measure = "cutoff")@y.values)
    d_risk <- contrib_1 + contrib_0 - ((y == 0)/p_0 + (y == 1)/p_1)*naive_auc
        
    ic_update <- d_risk
    
  } else if (type == "accuracy") {
    contrib <- mean((fitted_values > 1/2) != y)
    d_risk <- ((fitted_values > 1/2) != y) - contrib
    ic_update <- d_risk
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }
  
  ## influence curve
  if (type %in% c("r_squared", "deviance")) {
    ic_update <- d_risk/denom - num/(denom ^ 2)*d_denom    
  } else if (type %in% c("auc", "accuracy")) {
    # already computed it above
  } else {
    # already computed it above
  }
  
  
  return(ic_update)
}
