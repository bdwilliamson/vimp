#' Estimate the influence function for variable importance parameters
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return The influence function values for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_update <- function(full, reduced, y, type = "anova", na.rm = FALSE) {
  
  ## calculate the necessary pieces for the influence curve
  if (type == "regression" | type == "anova") {
    naive_num <- mean((full - reduced) ^ 2, na.rm = na.rm)
    naive_denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    d_s <- 2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - naive_num
    d_denom <- (y - mean(y, na.rm = na.rm))^2 - naive_denom
  } else if (type == "deviance") {
    if (is.null(dim(y))) {
      y_mult <- cbind(y, 1 - y)
    } else {
      y_mult <- y
    }
    if (is.null(dim(full))) {
      full_mat <- cbind(full, 1 - full)
      reduced_mat <- cbind(reduced, 1 - reduced)
    } else if(dim(full)[2] < 2) {
      full_mat <- cbind(full, 1 - full)
      reduced_mat <- cbind(reduced, 1 - reduced)
    } else {
      full_mat <- full
      reduced_mat <- reduced
    }
    p <- apply(y_mult, 2, mean)
    naive_num <- 2*sum(diag(t(y_mult)%*%log(full_mat/reduced_mat)), na.rm = na.rm)/dim(y_mult)[1]
    naive_denom <- -1*sum(log(p))
    d_s <- 2*rowSums(y_mult*log(full_mat/reduced_mat) - (full_mat - reduced_mat), na.rm = na.rm) - naive_num
    ## influence function of the denominator
    d_denom <- rowSums(-1/p*((y_mult == 1) - p))
  } else if (type == "r_squared") {
    naive_denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
    mse_full <- mean((y - full)^2, na.rm = na.rm)/naive_denom
    mse_reduced <- mean((y - reduced)^2, na.rm = na.rm)/naive_denom
    naive_num <- mse_reduced - mse_full
    
    d_s_full <- (y - full)^2 - mse_full
    d_s_reduced <- (y - reduced)^2 - mse_reduced
    d_denom <- (y - mean(y, na.rm = na.rm))^2 - naive_denom
    d_s <- (-1)*d_s_full - (-1)*d_s_reduced
  } else if (type == "auc") {
    p_0 <- mean(y == 0)
    p_1 <- mean(y == 1)
    
    full_pred <- ROCR::prediction(predictions = full, labels = y)
    red_pred <- ROCR::prediction(predictions = reduced, labels = y)
    
    sens_full <- unlist(lapply(as.list(full), function(x) mean(full[y == 0] < x)))
    spec_full <- unlist(lapply(as.list(full), function(x) mean(full[y == 1] > x)))
    
    sens_red <- unlist(lapply(as.list(reduced), function(x) mean(reduced[y == 0] < x)))
    spec_red <- unlist(lapply(as.list(reduced), function(x) mean(reduced[y == 1] > x)))
    
    contrib_1_full <- (y == 1)/p_1*sens_full
    contrib_0_full <- (y == 0)/p_0*spec_full
    naive_auc_full <- unlist(ROCR::performance(prediction.obj = full_pred, measure = "auc", x.measure = "cutoff")@y.values)
    d_s_full <- contrib_1_full + contrib_0_full - ((y == 0)/p_0 + (y == 1)/p_1)*naive_auc_full
    
    contrib_1_reduced <- (y == 1)/p_1*sens_red
    contrib_0_reduced <- (y == 0)/p_0*spec_red
    naive_auc_reduced <- unlist(ROCR::performance(prediction.obj = red_pred, measure = "auc", x.measure = "cutoff")@y.values)
    d_s_reduced <- contrib_1_reduced + contrib_0_reduced - ((y == 0)/p_0 + (y == 1)/p_1)*naive_auc_reduced
    
    ic_update <- d_s_full - d_s_reduced
    
  } else if (type == "accuracy") {
    contrib_full <- mean((full > 1/2) != y)
    contrib_reduced <- mean((reduced > 1/2) != y)
    d_s_full <- ((full > 1/2) != y) - contrib_full
    d_s_reduced <- ((reduced > 1/2) != y) - contrib_reduced
    ic_update <- d_s_reduced - d_s_full
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }
  
  ## influence curve
  if (type %in% c("regression", "anova", "r_squared", "deviance")) {
    ic_update <- d_s/naive_denom - naive_num/(naive_denom ^ 2)*d_denom    
  } else if (type %in% c("auc", "accuracy")) {
    # already computed it above
  } else {
    stop("We currently do not support the entered variable importance parameter.")
  }
  
  
  return(ic_update)
}
