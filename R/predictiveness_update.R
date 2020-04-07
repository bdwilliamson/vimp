#' Estimate the influence function for an estimator of predictiveness
#'
#' Estimate the influence function for the given measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function.
#' @param y the outcome.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param type which risk parameter are you estimating (defaults to \code{r_squared}, for the $R^2$)?
#' @param na.rm logical; should NAs be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated influence function values for the given measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#'
#' @export
predictiveness_update <- function(fitted_values, y, weights = rep(1, length(y)), type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_deviance, measure_r_squared, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    ## calculate the necessary pieces for the influence curve
    if (!is.na(measure_func)) {
        ic <- measure_func[[1]](fitted_values, y, weights, na.rm)$ic
    } else { # if type is anova, no plug-in from predictiveness
        ic <- NA
    }
    return(ic)
}
