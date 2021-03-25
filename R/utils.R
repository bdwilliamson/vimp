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
#' @param X the covariates
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
#' @param V the number of cross-fitting folds
#' @param cv a logical flag indicating whether or not to use cross-fitting
#' 
#' @return None. Called for the side effect of stopping the algorithm if 
#'   any inputs are in an unexpected format.
check_fitted_values <- function(Y = NULL, f1 = NULL, f2 = NULL, 
                                cross_fitted_f1 = NULL, cross_fitted_f2 = NULL,
                                sample_splitting_folds = NULL, 
                                cross_fitting_folds = NULL, V = NULL, cv = FALSE) {
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
    if (is.null(f1)) {
      stop(paste0("You must enter an estimator of the population-optimal predictor",
                  " using all covariates."))
    }
    if (is.null(f2)) {
      stop(paste0("You must enter an estimator of the population-optimal predictor",
                  " using the reduced set of covariates."))
    }
    if (length(sample_splitting_folds) != length(Y)) {
      stop("The entered folds must be the same length as the outcome of interest.")
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

# ------------------------------------------------------------------------------

#' Obtain the type of VIM to estimate using partial matching
#' 
#' @param type the partial string indicating the type of VIM
#' 
#' @return the full string indicating the type of VIM
get_full_type <- function(type) {
  types <- c("accuracy", "auc", "deviance", "r_squared", "anova")
  full_type <- types[pmatch(type, types)]
  if (is.na(full_type)) {
    stop("We currently do not support the entered variable importance parameter.")
  }
  if (full_type == "anova" ) {
    warning(paste0("Hypothesis testing is not available for type = 'anova'. ",
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
#'
make_folds <- function(y, V = 2, stratified = FALSE, 
                       C = NULL,
                       probs = rep(1/V, V)) {
  folds <- vector("numeric", length(y))
  if (length(unique(probs)) == 1) {
    if (stratified) {
      if (length(unique(C)) == 1) {
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
      if (length(unique(C)) == 1) {
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

# For sp_vim -------------------------------------------------------------------
#' Run a Super Learner for the provided subset of features
#'
#' @param Y the outcome
#' @param X the covariates
#' @param V the number of folds
#' @param SL.library the library of candidate learners
#' @param s the subset of interest
#' @param cv_folds the CV folds
#' @param ss_folds the sample-splitting folds
#' @param verbose should we print progress? defaults to FALSE
#' @param progress_bar the progress bar to print to (only if verbose = TRUE)
#' @param indx the index to pass to progress bar (only if verbose = TRUE)
#' @param ... other arguments to Super Learner
#'
#' @return a list of length V, with the results of predicting on the hold-out data for each v in 1 through V
run_sl <- function(Y = NULL, X = NULL, V = 5, SL.library = "SL.glm", 
                   univariate_SL.library = "SL.glm", s = 1, cv_folds = NULL,
                   ss_folds = NULL, verbose = FALSE, progress_bar = NULL, 
                   indx = 1, weights = rep(1, nrow(X)), ...) {
  # if verbose, print what we're doing and make sure that SL is verbose
  L <- list(...)
  if (is.null(L$family)) {
    L$family <- gaussian()
  }
  if (verbose) {
    if (is.null(L$cvControl)) {
      L$cvControl <- list(verbose = TRUE)
    } else if (any(grepl("verbose", names(L$cvControl)))) {
      L$cvControl$verbose <- TRUE
    }
  }
  # fit the super learner on each full/reduced pair
  red_X <- as.data.frame(X[, s, drop = FALSE])
  if (is.null(L$obsWeights)) {
    L$obsWeights <- weights
  }
  if (is.null(cv_folds)) {
    cv_folds <- make_folds(Y, V = V, stratified = (length(unique(Y)) == 2))
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
  fhat_ful <- list()
  fhat_red <- list()
  for (v in 1:V) {
    train_v <- (cv_folds != v)
    test_v <- (cv_folds == v) & (ss_folds == 2)
    # fit super learner
    this_L <- L
    this_L$obsWeights <- L$obsWeights[train_v]
    new_arg_list <- c(list(
      Y = Y[train_v, , drop = FALSE], X = red_X[train_v, , drop = FALSE],
      SL.library = this_sl_lib
    ), this_L)
    if (!is.character(this_sl_lib)) { # only a single learner, so don't do CV
      fit <- this_sl_lib(Y = Y[train_v, , drop = FALSE], 
                         X = red_X[train_v, , drop = FALSE], 
                         newX = red_X[test_v, , drop = FALSE],
                         family = new_arg_list$family, 
                         obsWeights = new_arg_list$obsWeights)
      fhat_ful[[v]] <- fit$pred
    } else {
      fit <- do.call(SuperLearner::SuperLearner, new_arg_list)
      ## get predictions on the validation fold
      fhat_ful[[v]] <- SuperLearner::predict.SuperLearner(
        fit, newdata = red_X[test_v, , drop = FALSE], onlySL = TRUE
      )$pred
    }
  }
  # refit to the entire dataset
  if (!is.character(this_sl_lib)) {
    fit <- this_sl_lib(Y = Y, X = red_X, newX = red_X, family = L$family,
                       obsWeights = L$obsWeights)
  } else {
    fit <- do.call(
      SuperLearner::SuperLearner, 
      args = c(L, list(
        Y = Y, X = red_X, SL.library = this_sl_lib
      ))
    )
  }
  fitted <- fit$SL.predict
  if (verbose) {
    setTxtProgressBar(progress_bar, indx)
  }
  return(list(cf_preds_lst = fhat_ful, cf_folds = cv_folds, 
              ss_folds = ss_folds, fit = fit, fitted = fitted))
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
