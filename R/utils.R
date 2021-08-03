# Checkers ---------------------------------------------------------------------

#' Check inputs to a call to vim, cv_vim, or sp_vim
#'
#' @details Ensure that inputs to \code{vim}, \code{cv_vim}, and \code{sp_vim}
#'   follow the correct formats.
#'
#' @param Y the outcome
#' @param X the covariates
#' @param f1 estimator of the population-optimal prediction function
#'   using all covariates
#' @param f2 estimator of the population-optimal prediction function
#'   using the reduced set of covariates
#' @param indx the index or indices of the covariate(s) of interest
#'
#' @return None. Called for the side effect of stopping the algorithm if
#'   any inputs are in an unexpected format.
#' @export
check_inputs <- function(Y, X, f1, f2, indx) {
  if (is.null(f1) && is.null(Y)) {
    stop("You must enter either Y or fitted values for the full regression.")
  }
  if (is.null(f2) && is.null(X)) {
    stop("You must enter either X or fitted values for the reduced regression.")
  }
  # if indx is outside the range of X, stop and throw an error
  if (!is.null(X)) {
    if (any(indx > dim(X)[2])) {
      stop(paste0("One of the feature indices in 'indx' is larger than the ",
                  "total number of features in X. Please specify a new index ",
                  "subgroup in 'indx'."))
    }
  }
}
#' Check pre-computed fitted values for call to vim, cv_vim, or sp_vim
#'
#' @details Ensure that inputs to \code{vim}, \code{cv_vim}, and \code{sp_vim}
#'   follow the correct formats.
#'
#' @param Y the outcome
#' @param f1 estimator of the population-optimal prediction function
#'   using all covariates
#' @param f2 estimator of the population-optimal prediction function
#'   using the reduced set of covariates
#' @param cross_fitted_f1 cross-fitted estimator of the population-optimal
#'   prediction function using all covariates
#' @param cross_fitted_f2 cross-fitted estimator of the population-optimal
#'   prediction function using the reduced set of covariates
#' @param sample_splitting_folds the folds for sample-splitting (used for
#'   hypothesis testing)
#' @param cross_fitting_folds the folds for cross-fitting (used for point
#'   estimates of variable importance in \code{cv_vim} and \code{sp_vim})
#' @param cross_fitted_se logical; should cross-fitting be used to estimate
#'   standard errors?
#' @param V the number of cross-fitting folds
#' @param ss_V the number of folds for CV (if sample_splitting is TRUE)
#' @param cv a logical flag indicating whether or not to use cross-fitting
#'
#' @return None. Called for the side effect of stopping the algorithm if
#'   any inputs are in an unexpected format.
#' @export
check_fitted_values <- function(Y = NULL, f1 = NULL, f2 = NULL,
                                cross_fitted_f1 = NULL, cross_fitted_f2 = NULL,
                                sample_splitting_folds = NULL,
                                cross_fitting_folds = NULL, 
                                cross_fitted_se = TRUE, V = NULL, ss_V = NULL,
                                cv = FALSE) {
  if (is.null(Y)) stop("Y must be entered.")
  if (!cv) {
    if (length(f1) == 0 || length(f2) == 0) {
      stop("Fitted values must be entered if run_regression = FALSE.")
    }
    if (length(sample_splitting_folds) != length(Y)) {
      stop("The entered folds must be the same length as the outcome of interest.")
    }
  } else {
    if (is.null(cross_fitted_f1)) {
      stop(paste0("You must specify a list of predicted values from a ",
                  "regression of Y on X."))
    }
    if (is.null(cross_fitted_f2)) {
      stop(paste0("You must specify a list of predicted values from either ",
                  "(a) a regression of the fitted values from the Y on X ",
                  "regression on the reduced set of covariates, or (b)",
                  "a regression of Y on the reduced set of covariates."))
    }
    if (is.null(f1) & !cross_fitted_se) {
      stop(paste0("You must enter an estimator of the population-optimal predictor",
                  " using all covariates."))
    }
    if (is.null(f2) & !cross_fitted_se) {
      stop(paste0("You must enter an estimator of the population-optimal predictor",
                  " using the reduced set of covariates."))
    }
    if (length(sample_splitting_folds) != ss_V) {
      stop("The sample splitting folds must be the same length as the number of cross-fitting folds.")
    }
    if (is.null(cross_fitting_folds)) {
      stop("You must specify the folds that were used for cross-fitting.")
    }
    if (length(cross_fitted_f1) != V) {
      stop(paste0("The number of folds from the full regression must be the ",
                  "same length as the number of folds."))
    }
    if (length(cross_fitted_f2) != V) {
      stop(paste0("The number of folds from the reduced regression must be ",
                  "the same length as the number of folds."))
    }
  }
}

#' Create complete-case outcome, weights, and Z
#'
#' @param Y the outcome
#' @param C indicator of missing or observed
#' @param Z the covariates observed in phase 1 and 2 data
#' @param X all covariates
#' @param ipc_weights the weights
#'
#' @return a list, with the complete-case outcome, weights, and Z matrix
#' @export
create_z <- function(Y, C, Z, X, ipc_weights) {
  # set up internal data -- based on complete cases only
  Y_cc <- subset(Y, C == 1, drop = FALSE)
  weights_cc <- ipc_weights[C == 1]
  if (!all(C == 1) || !all(ipc_weights == 1)) {
    if (is.character(Z)) {
      tmp_Z <- Z[Z != "Y"]
      minus_X <- as.numeric(gsub("X", "", tmp_Z))
      # check to see if it is only part of X matrix
      if (any(sapply(seq_along(minus_X), function(j) length(minus_X[j]) > 0))) {
        if (any(grepl("Y", Z))) {
          Z_in <- as.data.frame(mget("Y"))
        } else {
          Z_in <- NULL
        }
        Z_in <- cbind.data.frame(Z_in, X[, minus_X])
      } else {
        Z_in <- as.data.frame(mget(Z))
      }
    } else {
      stop("Please enter a character vector corresponding to the names of the fully observed data.")
    }
  } else {
    Z_in <- NULL
  }
  list(Y = Y_cc, weights = weights_cc, Z = Z_in)
}

# ------------------------------------------------------------------------------

#' Obtain the type of VIM to estimate using partial matching
#'
#' @param type the partial string indicating the type of VIM
#'
#' @return the full string indicating the type of VIM
#' @export
get_full_type <- function(type) {
  types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
  full_type <- types[pmatch(type, types)]
  if (is.na(full_type)) {
    stop("We currently do not support the entered variable importance parameter.")
  }
  if (full_type == "anova" ) {
    message(paste0("Hypothesis testing is not available for type = 'anova'. ",
                   "If you want an R-squared-based hypothesis test, please enter ",
                   "type = 'r_squared'."))
  }
  full_type
}

#' Return an estimator on a different scale
#'
#' @details It may be of interest to return an estimate (or confidence interval)
#'   on a different scale than originally measured. For example, computing a
#'   confidence interval (CI) for a VIM value that lies in (0,1) on the logit scale
#'   ensures that the CI also lies in (0, 1).
#'
#' @param obs_est the observed VIM estimate
#' @param grad the estimated efficient influence function
#' @param scale the scale to compute on
#'
#' @return the scaled estimate
#' @export
scale_est <- function(obs_est = NULL, grad = NULL, scale = "identity") {
  if (scale == "logit") {
    this_grad <- 1 / (obs_est - obs_est ^ 2)
    est <- stats::plogis(stats::qlogis(obs_est) + this_grad * mean(grad))
  } else if (scale == "log") {
    this_grad <- 1 / obs_est
    est <- exp(log(obs_est) + this_grad * mean(grad))
  } else {
    est <- obs_est + mean(grad)
  }
  est
}

# ------------------------------------------------------------------------------

#' Create Folds for Cross-Fitting
#'
#' @param y the outcome
#' @param V the number of folds
#' @param stratified should the folds be stratified based on the outcome?
#' @param C a vector indicating whether or not the observation is fully observed;
#'    1 denotes yes, 0 denotes no
#' @param probs vector of proportions for each fold number
#' @return a vector of folds
#' @export
make_folds <- function(y, V = 2, stratified = FALSE,
                       C = NULL,
                       probs = rep(1/V, V)) {
  folds <- vector("numeric", length(y))
  if (length(unique(probs)) == 1) {
    if (stratified) {
      if (length(unique(C)) <= 1) {
        folds_1 <- sample(rep(seq_len(V), length = sum(y == 1)))
        folds_0 <- sample(rep(seq_len(V), length = sum(y == 0)))
        folds[y == 1] <- folds_1
        folds[y == 0] <- folds_0
      } else {
        folds_11 <- sample(rep(seq_len(V), length = sum(y == 1 & C == 1)))
        folds_10 <- sample(rep(seq_len(V), length = sum(y == 0 & C == 1)))
        folds_01 <- sample(rep(seq_len(V), length = sum(y == 1 & C == 0)))
        folds_00 <- sample(rep(seq_len(V), length = sum(y == 0 & C == 0)))
        folds[y == 1 & C == 1] <- folds_11
        folds[y == 0 & C == 1] <- folds_10
        folds[y == 1 & C == 0] <- folds_01
        folds[y == 0 & C == 0] <- folds_00
      }
    } else {
      folds <- sample(rep(seq_len(V), length = length(y)))
    }
  } else {
    if (stratified) {
      if (length(unique(C)) <= 1) {
        folds_1 <- rep(seq_len(V), probs * sum(y == 1))
        folds_1 <- c(folds_1, sample(seq_len(V), size = sum(y == 1) - length(folds_1),
                                     replace = TRUE, prob = probs))
        folds_0 <- rep(seq_len(V), probs * sum(y == 0))
        folds_0 <- c(folds_0, sample(seq_len(V), size = sum(y == 0) - length(folds_0),
                                     replace = TRUE, prob = probs))
        folds_1 <- sample(folds_1)
        folds_0 <- sample(folds_0)
        folds[y == 1] <- folds_1
        folds[y == 0] <- folds_0
      } else {
        folds_11 <- rep(seq_len(V), probs * sum(y == 1 & C == 1))
        folds_10 <- rep(seq_len(V), probs * sum(y == 1 & C == 0))
        folds_01 <- rep(seq_len(V), probs * sum(y == 0 & C == 1))
        folds_00 <- rep(seq_len(V), probs * sum(y == 0 & C == 0))
        folds_11 <- c(folds_11, sample(seq_len(V), size = sum(y == 1 & C == 1) -
                                         length(folds_11),
                                       replace = TRUE, prob = probs))
        folds_01 <- c(folds_01, sample(seq_len(V), size = sum(y == 0 & C == 1) -
                                         length(folds_01),
                                       replace = TRUE, prob = probs))
        folds_10 <- c(folds_10, sample(seq_len(V), size = sum(y == 1 & C == 0) -
                                         length(folds_10),
                                       replace = TRUE, prob = probs))
        folds_00 <- c(folds_00, sample(seq_len(V), size = sum(y == 0 & C == 0) -
                                         length(folds_00),
                                       replace = TRUE, prob = probs))
        folds_11 <- sample(folds_11)
        folds_01 <- sample(folds_01)
        folds_10 <- sample(folds_10)
        folds_00 <- sample(folds_00)
        folds[y == 1 & C == 1] <- folds_11
        folds[y == 0 & C == 1] <- folds_10
        folds[y == 1 & C == 0] <- folds_01
        folds[y == 0 & C == 0] <- folds_00
      }
    } else {
      these_probs <- round(probs * length(y))
      if (sum(these_probs) != length(y)) {
        these_probs[which.min(these_probs)] <- these_probs[which.min(these_probs)] - 1
      }
      folds <- sample(rep(seq_len(V), these_probs))
    }
  }
  return(folds)
}

#' Turn folds from 2K-fold cross-fitting into individual K-fold folds
#'
#' @param cross_fitting_folds the vector of cross-fitting folds
#' @param sample_splitting_folds the sample splitting folds
#' @param C vector of whether or not we measured the observation in phase 2
#'
#' @return the two sets of testing folds for K-fold cross-fitting
#' @export
make_kfold <- function(cross_fitting_folds,
                       sample_splitting_folds = rep(1, length(unique(cross_fitting_folds))),
                       C = rep(1, length(cross_fitting_folds))) {
  # get the folds for the full and reduced nuisance functions
  full_folds <- which(sample_splitting_folds == 1)
  redu_folds <- which(sample_splitting_folds == 2)
  sample_splitting_vec <- vector("numeric", length = length(cross_fitting_folds))
  sample_splitting_vec[cross_fitting_folds %in% full_folds] <- 1
  sample_splitting_vec[cross_fitting_folds %in% redu_folds] <- 2
  # create K-fold folds, i.e., 1:K for each
  full_cf_folds <- cross_fitting_folds[cross_fitting_folds %in% full_folds]
  redu_cf_folds <- cross_fitting_folds[cross_fitting_folds %in% redu_folds]
  unique_full <- sort(unique(full_cf_folds))
  unique_redu <- sort(unique(redu_cf_folds))
  K <- length(unique_full)
  folds_k <- seq_len(K)
  k_fold_full <- full_cf_folds
  k_fold_redu <- redu_cf_folds
  for (v in seq_len(K)) {
    k_fold_full <- replace(k_fold_full, full_cf_folds == unique_full[v], folds_k[v])
    k_fold_redu <- replace(k_fold_redu, redu_cf_folds == unique_redu[v], folds_k[v])
  }
  # return a list; the first four values are the cross-fitting folds,
  # while the last two values replicate the sample-splitting folds
  list(full = k_fold_full, reduced = k_fold_redu,
       sample_splitting_folds = sample_splitting_vec)
}

# For sp_vim -------------------------------------------------------------------
#' Run a Super Learner for the provided subset of features
#'
#' @param Y the outcome
#' @param X the covariates
#' @param V the number of folds
#' @param SL.library the library of candidate learners
#' @param univariate_SL.library the library of candidate learners for
#'          single-covariate regressions
#' @param s the subset of interest
#' @param cv_folds the CV folds
#' @param sample_splitting logical; should we use sample-splitting for
#'   predictiveness estimation?
#' @param ss_folds the sample-splitting folds; only used if
#'   \code{sample_splitting = TRUE}
#' @param split the split to use for sample-splitting; only used if 
#'   \code{sample_splitting = TRUE}
#' @param verbose should we print progress? defaults to FALSE
#' @param progress_bar the progress bar to print to (only if verbose = TRUE)
#' @param indx the index to pass to progress bar (only if verbose = TRUE)
#' @param weights weights to pass to estimation procedure
#' @param cross_fitted_se if \code{TRUE}, uses a cross-fitted estimator of
#'   the standard error; otherwise, uses the entire dataset
#' @param full should this be considered a "full" or "reduced" regression?
#'   If \code{NULL} (the default), this is determined automatically; a full
#'   regression corresponds to \code{s} being equal to the full covariate vector.
#'   For SPVIMs, can be entered manually.
#' @param ... other arguments to Super Learner
#'
#' @return a list of length V, with the results of predicting on the hold-out data for each v in 1 through V
#' @export
run_sl <- function(Y = NULL, X = NULL, V = 5, SL.library = "SL.glm",
                   univariate_SL.library = NULL, s = 1, cv_folds = NULL,
                   sample_splitting = TRUE, ss_folds = NULL, split = 1, verbose = FALSE,
                   progress_bar = NULL, indx = 1, weights = rep(1, nrow(X)),
                   cross_fitted_se = TRUE, full = NULL, ...) {
  # if verbose, print what we're doing and make sure that SL is verbose;
  # set up the argument list for the Super Learner / CV.SuperLearner
  arg_lst <- list(...)
  if (is.null(arg_lst$family)) {
    arg_lst$family <- switch(
      (length(unique(Y)) == 2) + 1, stats::gaussian(), stats::binomial()
    )
  }
  if (is.character(arg_lst$family)) {
    family <- get(arg_lst$family, mode = "function", envir = parent.frame())
    arg_lst$family <- family()
  }
  if ((arg_lst$family$family == "binomial") & (length(unique(Y)) > 2)) {
    arg_lst$family <- stats::gaussian()
  }
  if (verbose) {
    if (is.null(arg_lst$cvControl)) {
      arg_lst$cvControl <- list(verbose = TRUE)
    } else {
      arg_lst$cvControl$verbose <- TRUE
    }
  }
  arg_lst_bool <- is.null(arg_lst$cvControl) | 
    ifelse(!is.null(arg_lst$cvControl), (arg_lst$cvControl$V != V) & (cross_fitted_se), FALSE)
  if (arg_lst_bool) {
    arg_lst$cvControl <- list(V = ifelse(V == 1, 5, V))
  } 
  if (is.null(arg_lst$obsWeights)) {
    arg_lst$obsWeights <- weights
  }
  arg_lst_cv <- arg_lst
  # fit the super learner for a given set of variables
  red_X <- as.data.frame(X[, s, drop = FALSE])
  if (is.null(cv_folds)) {
    cv_folds <- make_folds(Y, V = V, stratified = (length(unique(Y)) == 2))
  }
  cf_folds_lst <- lapply(as.list(seq_len(V)), function(v) {
    which(cv_folds == v)
  })
  if (V > 1) {
    arg_lst_cv$cvControl$validRows <- cf_folds_lst  
  }
  this_sl_lib <- SL.library
  # if univariate regression (i.e., length(s) == 1) then check univariate_SL.library
  # if it exists, use it; otherwise, use the normal library
  if (length(s) == 1) {
    if (!is.null(univariate_SL.library)) {
      this_sl_lib <- univariate_SL.library
    } 
    requires_2d <- c("glmnet", "polymars")
    for (i in 1:length(requires_2d)) {
      if (any(grepl(requires_2d[i], this_sl_lib)) & (ncol(red_X) == 1)) {
        red_X <- cbind.data.frame(V0 = 0, red_X)
      }
    }
  }
  full_arg_lst_cv <- c(arg_lst_cv, list(
    Y = Y, X = red_X, SL.library = this_sl_lib
  ))
  # if dim(red_X) == 0, then return the mean
  if (ncol(red_X) == 0) {
    this_sl_lib <- eval(parse(text = "SL.mean"))
  } 
  # if a single learner, don't do inner CV
  if (!is.character(this_sl_lib) | length(this_sl_lib) == 1) {
    if (is.character(this_sl_lib)) {
      this_sl_lib <- eval(parse(text = this_sl_lib))
    }
    preds <- list()
    if (V == 1) {
      fit <- NA
      preds <- NA
    } else {
      pred_indx <- 1
      for (v in seq_len(V)) {
        train_v <- (cv_folds != v)
        test_v <- (cv_folds == v)
        if (ss_folds[v] == split | !sample_splitting) {
        fit <- this_sl_lib(Y = Y[train_v, , drop = FALSE],
                           X = red_X[train_v, , drop = FALSE],
                           newX = red_X[test_v, , drop = FALSE],
                           family = full_arg_lst_cv$family,
                           obsWeights = full_arg_lst_cv$obsWeights[train_v])
          preds[[pred_indx]] <- fit$pred  
          pred_indx <- pred_indx + 1
        } 
      }
    }
  } else if (V == 1) {
    # once again, don't do anything; will fit at end
    fit <- NA
    preds <- NA
  } else {
    # fit a cross-validated Super Learner
    fit <- do.call(SuperLearner::CV.SuperLearner, full_arg_lst_cv)
    # extract predictions on correct sampled-split folds
    if (is.null(full)) {
      all_equal_s <- all.equal(s, 1:ncol(X))
      is_full <- switch((sample_splitting) + 1, TRUE, 
                        !is.character(all_equal_s) & as.logical(all_equal_s))
    } else {
      is_full <- full
    }
    preds <- extract_sampled_split_predictions(
      cvsl_obj = fit, sample_splitting = sample_splitting, full = is_full,
      sample_splitting_folds = switch((sample_splitting) + 1, rep(1, V), ss_folds)
    )
  }
  # if cross_fitted_se, we're done; otherwise, re-fit to the entire dataset
  if (!cross_fitted_se) {
    # refit to the entire dataset
    if (!is.character(this_sl_lib)) {
      fit_se <- this_sl_lib(Y = Y[ss_folds == split, ], 
                            X = red_X[ss_folds == split, , drop = FALSE], 
                            newX = red_X[ss_folds == split, , drop = FALSE], 
                            family = arg_lst$family, 
                            obsWeights = arg_lst$obsWeights[ss_folds == split])
      preds_se <- fit_se$pred
      if (all(is.na(preds))) {
        preds <- preds_se
        fit <- fit_se
      }
    } else {
      arg_lst$obsWeights <- weights[ss_folds == split]
      fit_se <- do.call(
        SuperLearner::SuperLearner,
        args = c(arg_lst, list(
          Y = Y[ss_folds == split, ], X = red_X[ss_folds == split, , drop = FALSE], 
          SL.library = this_sl_lib
        ))
      )
      preds_se <- fit_se$SL.predict
      if (all(is.na(preds))) {
        fit <- fit_se
        preds <- preds_se
      }
    }
  } else {
    fit_se <- NA
    preds_se <- NA
  }
  if (verbose) {
    setTxtProgressBar(progress_bar, indx)
  }
  return(list(fit = fit, preds = preds, ss_folds = ss_folds,
              cv_folds = cv_folds, fit_non_cf_se = fit_se, preds_non_cf_se = preds_se))
}

# -------------------------------------
# release questions
# -------------------------------------
# @keywords internal
release_questions <- function() {
  c(
    "Have you run cran_prep <- rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = 'always'), show_status = FALSE)?",
    "Have you run devtools::check_win_devel() and devtools::check_win_release()?"
  )
}
