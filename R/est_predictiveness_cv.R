#' Estimate a nonparametric predictiveness functional using cross-fitting
#'
#' Compute nonparametric estimates of the chosen measure of predictiveness.
#'
#' @param fitted_values fitted values from a regression function using the
#'   observed data; a list of length V, where each object is a set of
#'   predictions on the validation data, or a vector of the same length as \code{y}.
#' @param y the observed outcome.
#' @param full_y the observed outcome (from the entire dataset, for
#'   cross-fitted estimates).
#' @param folds the cross-validation folds for the observed data.
#' @param type which parameter are you estimating (defaults to \code{r_squared},
#'   for R-squared-based variable importance)?
#' @param C the indicator of coarsening (1 denotes observed, 0 denotes
#'   unobserved).
#' @param Z either \code{NULL} (if no coarsening) or a matrix-like object
#'   containing the fully observed data.
#' @param folds_Z either the cross-validation folds for the observed data
#'   (no coarsening) or a vector of folds for the fully observed data Z.
#' @param ipc_weights weights for inverse probability of coarsening (e.g.,
#'   inverse weights from a two-phase sample) weighted estimation. Assumed to be
#'   already inverted (i.e., ipc_weights = 1 / [estimated probability weights]).
#' @param ipc_fit_type if "external", then use \code{ipc_eif_preds}; if "SL",
#'   fit a SuperLearner to determine the correction to the efficient
#'   influence function.
#' @param ipc_eif_preds if \code{ipc_fit_type = "external"}, the fitted values
#'   from a regression of the full-data EIF on the fully observed
#'   covariates/outcome; otherwise, not used.
#' @param ipc_est_type IPC correction, either \code{"ipw"} (for classical
#'   inverse probability weighting) or \code{"aipw"} (for augmented inverse
#'   probability weighting; the default).
#' @param scale if doing an IPC correction, then the scale that the correction
#'   should be computed on (e.g., "identity"; or "logit" to logit-transform,
#'   apply the correction, and back-transform).
#' @param na.rm logical; should NA's be removed in computation?
#'   (defaults to \code{FALSE})
#' @param ... other arguments to SuperLearner, if \code{ipc_fit_type = "SL"}.
#'
#' @return The estimated measure of predictiveness.
#'
#' @details See the paper by Williamson, Gilbert, Simon, and Carone for more
#'   details on the mathematics behind this function and the definition of the
#'   parameter of interest.  If sample-splitting is also requested
#'    (recommended, since in this case inferences
#'    will be valid even if the variable has zero true importance), then the
#'    prediction functions are trained as if \eqn{2K}-fold cross-validation were run,
#'    but are evaluated on only \eqn{K} sets (independent between the full and
#'    reduced nuisance regression).
#' @export
est_predictiveness_cv <- function(fitted_values, y, full_y = NULL,
                                  folds,
                                  type = "r_squared",
                                  C = rep(1, length(y)), Z = NULL,
                                  folds_Z = folds,
                                  ipc_weights = rep(1, length(C)),
                                  ipc_fit_type = "external",
                                  ipc_eif_preds = rep(1, length(C)),
                                  ipc_est_type = "aipw", scale = "identity",
                                  na.rm = FALSE, ...) {
    # get the correct measure function; if not one of the supported ones, say so
    types <- c("accuracy", "auc", "deviance", "r_squared", "anova", "mse",
               "cross_entropy")
    full_type <- types[pmatch(type, types)]
    if (is.na(full_type)) stop(
        paste0("We currently do not support the entered variable importance ",
               "parameter.")
    )
    measure_funcs <- c(measure_accuracy, measure_auc, measure_cross_entropy,
                       measure_mse, NA, measure_mse, measure_cross_entropy)
    measure_func <- measure_funcs[pmatch(type, types)]
    if (is.list(fitted_values)) {
        fitted_values_vector <- vector("numeric", length = length(y))
        for (v in seq_len(length(unique(folds)))) {
          fitted_values_vector[folds == v] <- fitted_values[[v]]
        }
        fitted_values <- fitted_values_vector
    }

    # compute point estimate, EIF
    if (!is.na(measure_func)) {
        V <- length(unique(folds))
        max_nrow <- max(sapply(1:V, function(v) length(C[folds_Z == v])))
        ics <- vector("list", length = V)
        ic <- vector("numeric", length = length(C))
        measures <- vector("list", V)
        for (v in seq_len(V)) {
            measures[[v]] <- measure_func[[1]](
                fitted_values = fitted_values[folds == v], y = y[folds == v],
                full_y = full_y,
                C = C[folds_Z == v], Z = Z[folds_Z == v, , drop = FALSE],
                ipc_weights = ipc_weights[folds_Z == v],
                ipc_fit_type = ipc_fit_type,
                ipc_eif_preds = ipc_eif_preds[folds_Z == v],
                ipc_est_type = ipc_est_type, scale = scale, na.rm = na.rm, ...
            )
            ics[[v]] <- measures[[v]]$eif
            ic[folds_Z == v] <- measures[[v]]$eif
        }
        point_ests <- sapply(1:V, function(v) measures[[v]]$point_est)
        point_est <- mean(point_ests)
        ipc_eif_preds <- sapply(
            1:V, function(v) measures[[v]]$ipc_eif_preds, simplify = FALSE
        )
    } else { # if type is anova, no plug-in from predictiveness
        point_est <- point_ests <- NA
        ic <- rep(NA, length(y))
    }
    # check whether or not we need to do ipc weighting -- if y is fully observed,
    # i.e., is part of Z, then we don't
    do_ipcw <- as.numeric(!all(ipc_weights == 1))
    if (is.null(full_y)) {
        mn_y <- mean(y, na.rm = na.rm)
    } else {
        mn_y <- mean(full_y, na.rm = na.rm)
    }
    # if full_type is "r_squared" or "deviance", post-hoc computing from "mse"
    # or "cross_entropy"
    if (full_type == "r_squared") {
        var <- measure_mse(
            fitted_values = rep(mn_y, length(y)), y,
            C = switch(do_ipcw + 1, rep(1, length(C)), C), Z = Z,
            ipc_weights = ipc_weights,
            ipc_fit_type = switch(do_ipcw + 1, ipc_fit_type, "SL"), ipc_eif_preds,
            ipc_est_type = ipc_est_type, scale = "identity", na.rm = na.rm, ...
        )
        ic <- (-1) * as.vector(
            matrix(c(1 / var$point_est,
                     -point_est / (var$point_est ^ 2)),
                   nrow = 1) %*% t(cbind(ic, var$eif))
        )
        tmp_ics <- vector("list", length = V)
        for (v in 1:V) {
            tmp_ics[[v]] <- (-1) * as.vector(
                matrix(c(1 / var$point_est,
                         -point_ests[v] / (var$point_est ^ 2)),
                       nrow = 1) %*% t(cbind(ics[[v]],
                                             var$eif[folds_Z == v]))
            )
        }
        point_ests <- 1 - point_ests/var$point_est
        point_est <- mean(point_ests)
        ics <- tmp_ics
    }
    if (full_type == "deviance") {
        denom <- measure_cross_entropy(
            fitted_values = rep(mn_y, length(y)), y,
            C = switch(do_ipcw + 1, rep(1, length(C)), C), Z = Z,
            ipc_weights = ipc_weights,
            ipc_fit_type = switch(do_ipcw + 1, ipc_fit_type, "SL"),
            ipc_eif_preds, ipc_est_type = ipc_est_type,
            scale = "identity", na.rm = na.rm, ...
        )
        ic <- (-1) * as.vector(
            matrix(c(1 / denom$point_est,
                     -point_est / (denom$point_est ^ 2)),
                   nrow = 1) %*% t(cbind(ic, denom$eif))
        )
        tmp_ics <- vector("list", length = V)
        for (v in 1:V) {
            tmp_ics[[v]] <- (-1) * as.vector(
                matrix(c(1 / denom$point_est,
                         -point_ests[v] / (denom$point_est ^ 2)),
                       nrow = 1) %*% t(cbind(ics[[v]],
                                             denom$eif[folds_Z == v]))
            )
        }
        point_ests <- 1 - point_ests / denom$point_est
        point_est <- mean(point_ests)
        ics <- tmp_ics
    }
    # return it
    return(list(point_est = point_est, all_ests = point_ests, eif = ic,
                all_eifs = ics, ipc_eif_preds = ipc_eif_preds))
}
