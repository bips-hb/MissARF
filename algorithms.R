
library(data.table)
library(mice)
library(arf)
library(missRanger)
library(missForest)

# mice imputation ---------------------------------------------------------
impute_mice <- function(incomplete, m = 5, ...) {
  dat_imputed <- complete(mice(incomplete, m = m, print = FALSE, ...), "all")
  dat_imputed <- lapply(dat_imputed, as.data.table)
  dat_imputed
}
mice_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_mice(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_mice(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}

# arf imputation ----------------------------------------------------------
impute_arf <- function(incomplete, m = 5, min_node_size = 10,
                       num_trees = 10, finite_bounds = "no", epsilon = 1e-14, 
                       expectation = FALSE, ...) {
  # Multiple imputation
  arf <- adversarial_rf(incomplete, verbose = FALSE, parallel = FALSE, 
                        min_node_size = min_node_size, num_trees = num_trees, 
                        replace = FALSE)
  psi <- forde(arf, incomplete, parallel = FALSE, finite_bounds = finite_bounds, 
               epsilon = epsilon, alpha = 0)
  if (expectation) {
    dat_imputed <- expct(psi, evidence = incomplete, 
                         parallel = FALSE, stepsize = 100)
  } else {
    x_synth <- forge(psi, n_synth = m, evidence = incomplete, 
                     parallel = FALSE, stepsize = 100)
    x_synth[, idx := rep(1:m, nrow(incomplete))]
    dat_imputed <- split(x_synth, by = "idx")
    dat_imputed <- lapply(dat_imputed, function(x) x[, idx := NULL])
  }
  dat_imputed
}
arf_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_arf(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_arf(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}

# missRanger imputation ---------------------------------------------------
impute_missRanger <- function(incomplete, m = 5, num.trees = 100, pmm.k = 5, num.threads = 1, ...) {
  # Multiple imputation
  dat_imputed <- lapply(1:m, function(i) {
    missRanger(incomplete, verbose = 0, pmm.k = pmm.k, 
               num.trees = num.trees, num.threads = num.threads, ...)
  })
  dat_imputed
}
missRanger_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_missRanger(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_missRanger(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}

# missForest imputation ---------------------------------------------------
impute_missForest <- function(incomplete, m = 5, ntree = 100, ...) {
  # Multiple imputation
  dat_imputed <- lapply(1:m, function(i) {
    missForest(incomplete, ntree = ntree, parallelize = "no", ...)$ximp
  })
  
  dat_imputed
}
missForest_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_missForest(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_missForest(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}

# Median imputation -------------------------------------------------------
impute_median <- function(incomplete, m = 5) {
  dat_imputed <- copy(incomplete)
  
  # Factor columns
  factor_cols <- names(which(sapply(incomplete, is.factor)))
  if (length(factor_cols) > 0) {
    dat_imputed[ , (factor_cols) := missMethods::impute_mode(.SD), .SDcols = factor_cols]
  }
  
  # Numeric columns
  nonfactor_cols <- setdiff(colnames(incomplete), factor_cols)
  if (length(nonfactor_cols) > 0) {
    dat_imputed[ , (nonfactor_cols) := missMethods::impute_median(.SD), .SDcols = nonfactor_cols]
  }
  dat_imputed
}
median_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_median(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_median(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}

# Random imputation -------------------------------------------------------
impute_random <- function(incomplete, m = 5) {
  # Multiple imputation
  dat_imputed <- lapply(1:m, function(i) {
    dat_i <- copy(incomplete)
    cols <- colnames(incomplete)
    dat_i[ , (cols) := lapply(.SD, impute_col_random)]
    dat_i
  })
  dat_imputed
}
random_fun <- function(data, job, instance, eval, ...) {
  train_imputed <- impute_random(instance$train$incomplete, ...)
  if (!is.null(instance$test)) {
    test_imputed <- impute_random(instance$test$incomplete, ...)
  } else {
    test_imputed <- NULL
  }
  eval_fun(instance, train_imputed, test_imputed, eval)
}


