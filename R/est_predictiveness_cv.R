#' Estimate a nonparametric predictiveness functional using cross-validation
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function; a list of length V, where each object is a set of predictions on the validation data.
#' @param y the outcome.
#' @param folds the cross-validation folds
#' @param x the covariates, only used if \code{ipc_weights} are entered (defaults to \code{NULL}).
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes unobserved).
#' @param ipc_weights weights for inverse probability of coarsening (e.g., inverse weights from a two-phase sample) weighted estimation.
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL", fit a SuperLearner to determine the correction to the efficient influence function
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values from a regression of the full-data EIF on the fully observed covariates/outcome; otherwise, not used.
#' @param na.rm logical; should NA's be removed in computation? (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return The estimated measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#' details on the mathematics behind this function and the definition of the parameter of interest.
#' @export
est_predictiveness_cv <- function(fitted_values, y, folds, type = "r_squared", x = NULL, C = rep(1, length(y)), ipc_weights = rep(1, length(y)), ipc_fit_type = "external", ipc_eif_preds = rep(1, length(y)), na.rm = FALSE, ...) {
    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse", "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop("We currently do not support the entered variable importance parameter.")
    measure_funcs <- c(measure_accuracy, measure_auc, measure_cross_entropy, measure_mse, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]

    # compute plug-in point estimate
    if (!is.na(measure_func)) {
        V <- length(unique(folds))
        measures <- vector("list", V)
        for (v in 1:V) {
            measures[[v]] <- measure_func[[1]](fitted_values = fitted_values[[v]], y = y[folds == v], x = x[folds == v, , drop =  FALSE], C = C[folds == v], ipc_weights = ipc_weights[folds == v], ipc_fit_type = ipc_fit_type, ipc_eif_preds = ipc_eif_preds[folds == v], na.rm = na.rm, ...)
        }
        point_ests <- sapply(1:V, function(v) measures[[v]]$point_est)
        point_est <- mean(point_ests)
        ipc_eif_preds <- sapply(1:V, function(v) measures[[v]]$ipc_eif_preds, simplify = FALSE)
    } else { # if type is anova, no plug-in from predictiveness
        point_est <- point_ests <- NA
    }
    # if full_type is "r_squared" or "deviance", post-hoc computing from "mse" or "cross_entropy"
    if (full_type == "r_squared") {
        obs_var <- mean(((y - mean(y[C == 1], na.rm = na.rm))^2)[C == 1], na.rm = na.rm)
        obs_var_eif <- ((y - mean(y[C == 1], na.rm = na.rm)) ^ 2)[C == 1] - obs_var
        ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_var_eif, X = x[C == 1, , drop = FALSE], ...)
        ipc_eif_preds <- predict(ipc_eif_mod)$pred
        var_eif <- (C / ipc_weights) * obs_var_eif + (C / ipc_weights - 1) * ipc_eif_preds
        denom_point_est <- mean((C / ipc_weights)*(y - mean(y, na.rm = na.rm))^2, na.rm = na.rm) + mean(var_eif)
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
        obs_p <- colMeans(y_mult[C == 1, ], na.rm = na.rm)
        obs_denom <- (-1)*sum(log(obs_p))
        obs_ic_denom <- rowSums(-1 / obs_p * ((y_mult[C == 1, ] == 1) - obs_p))
        ipc_eif_mod <- SuperLearner::SuperLearner(Y = obs_ic_denom, X = x[C == 1, , drop = FALSE], ...)
        ipc_eif_preds <- predict(ipc_eif_mod)$pred
        ic_denom <- (C / ipc_weights) * obs_ic_denom + (C / ipc_weights - 1) * ipc_eif_preds
        denom_point_est <- (-1) * sum(log(colMeans((C / ipc_weights) * y_mult, na.rm = na.rm))) + mean(ic_denom)
        point_ests <- point_ests/denom_point_est
        point_est <- mean(point_ests)
    }
    # return it
    return(list(point_est = point_est, all_ests = point_ests, ipc_eif_preds = ipc_eif_preds))
}
