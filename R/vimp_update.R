#' Estimate the influence function
#'
#' Compute the value of the influence function for the given group of left-out covariates.
#'
#' @param full fitted values from a regression of the outcome on the full set of covariates.
#' @param reduced fitted values from a regression either (1) of the outcome on the reduced set of covariates, or (2) of the fitted values from the full regression on the reduced set of covariates.
#' @param y the outcome.
#' @param type which parameter are you estimating (defaults to \code{regression}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return The influence function values for the given group of left-out covariates.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
vimp_update <- function(full, reduced, y, type = "regression", na.rm = FALSE) {

    ## calculate the necessary pieces for the influence curve
    if (type == "regression") {
        naive_num <- mean((full - reduced) ^ 2, na.rm = na.rm)
        naive_denom <- mean((y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        d_s <- 2*(y - full)*(full - reduced) + (full - reduced) ^ 2 - naive_num
        d_denom <- (y - mean(y, na.rm = na.rm))^2 - naive_denom
    } else if (type == "deviance") {
        p <- apply(y, 2, mean)
        naive_num <- 2*sum(diag(t(full)%*%log(full/reduced)))/dim(y)[1]
        naive_denom <- -1*sum(log(p))
        d_s <- 2*rowSums(y*log(full/reduced) - (full - reduced), na.rm = na.rm) - naive_num
        ## influence function of the denominator
        d_denom <- rowSums(-1/p*((y == 1) - p))
    } else {
        stop("We currently do not support the entered variable importance parameter.")
    }

    ## influence curve
    ic_update <- d_s/naive_denom - naive_num/(naive_denom ^ 2)*d_denom
  
    return(ic_update)
}
