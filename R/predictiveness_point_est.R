#' Estimate a nonparametric predictiveness functional
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
predictiveness_point_est <- function(fitted_values, y, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_deviance, measure_r_squared, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    ## compute plug-in point estimate
    if (!is.na(measure_func)) {
        point_est <- measure_func[[1]](fitted_values, y, weights, na.rm)$point_est
    } else { # if type is anova, no plug-in from predictiveness
        point_est <- NA
    }
    ## return it
    return(point_est)
}
