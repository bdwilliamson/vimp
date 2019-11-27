#' Estimate a nonparametric predictiveness functional using cross-validation
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param weights weights for the computed influence curve (e.g., inverse probability weights for coarsened-at-random settings)
#' @param folds the cross-validation folds
#' @param type which parameter are you estimating (defaults to \code{anova}, for ANOVA-based variable importance)?
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#'
#' @return The estimated measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
cv_predictiveness_point_est <- function(fitted_values, y, weights = rep(1, length(y)), folds, type = "r_squared", na.rm = FALSE) {

    ## get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_cross_entropy, measure_mse, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    ## compute plug-in point estimate
    if (!is.na(measure_func)) {
        V <- length(unique(folds))
        point_ests <- vector("numeric", V)
        for (v in 1:V) {
            point_ests[v] <- measure_func[[1]](fitted_values[[v]], y[folds == v], weights[folds == v], na.rm)$point_est
        }
        point_est <- mean(point_ests)
    } else { # if type is anova, no plug-in from predictiveness
        point_est <- point_ests <- NA
    }
    ## if full_type is "r_squared" or "deviance", post-hoc computing from "mse" or "cross_entropy"
    if (full_type == "r_squared") {
        denom_point_est <- mean(weights*(y - mean(y, na.rm = na.rm))^2, na.rm = na.rm)
        point_ests <- 1 - point_ests/denom_point_est
        point_est <- mean(point_ests)
    }
    if (full_type == "deviance") {
        if (is.null(dim(y))) { # assume that zero is in first column
            y_mult <- cbind(1 - y, y)
        } else if (dim(y)[2] == 1) {
            y_mult <- cbind(1 - y, y)
        } else {
            y_mult <- y
        }
        p <- apply(weights*y_mult, 2, mean, na.rm = na.rm)
        denom_point_est <- (-1)*sum(log(p))
        point_ests <- point_ests/denom_point_est
        point_est <- mean(point_ests)
    }
    ## return it
    return(list(point_est = point_est, all_ests = point_ests))
}
