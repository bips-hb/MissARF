
library(data.table)
library(mice)
library(mlr3verse)

# Safer version of sample() -----------------------------------------------
resample <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

# which.max() with random at ties -----------------------------------------
which.max.random <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  which(rank(x, ties.method = "random", na.last = FALSE) == length(x))
}

# Mode function -----------------------------------------------------------
mymode <- function(x) {
  ux <- unique(x)
  ux[which.max.random(tabulate(match(x, ux)))]
}

# Mean for numerics, mode for factors -------------------------------------
meanmode <- function(x) {
  if (is.factor(x)) {
    mymode(x)
  } else {
    mean(x)
  }
}

# Impute a random value (from observed values) ----------------------------
impute_col_random <- function(x) {
  if (all(is.na(x))) {
    NA
  } else if (all(!is.na(x))) {
    x
  } else {
    x[is.na(x)] <- resample(x[!is.na(x)], sum(is.na(x)), replace = TRUE)
    x
  }
}

# Mean/mode of datasets ---------------------------------------------------
average_datasets <- function(dats) {
  if (is.list(dats) && !is.data.table(dats) && is.data.table(dats[[1]])) {
    n <- nrow(dats[[1]])
    m <- length(dats)
    dats <- rbindlist(dats)
    dats[, id := rep(seq(1, n), times = m)]
    dats <- dats[, lapply(.SD, meanmode), by=id]
    dats[, id := NULL]
  }
  dats
}

# Evaluate coverage -------------------------------------------------------
eval_coverage <- function(instance, dat_imputed) {
  # Linear or squared effect
  if (!is.null(instance$effect) && instance$effect == "squared") {
    frml <- paste0("y ~ ", paste0("I(", colnames(instance$complete)[-1], "^2)", collapse = " + "))
  } else {
    frml <- paste0("y ~ ", paste0(colnames(instance$complete)[-1], collapse = " + "))
  }
  
  # Model fit
  if (is.factor(instance$incomplete$y)) {
    fit <- lapply(dat_imputed, function(dat) glm(as.formula(frml), dat, family = "binomial"))
  } else {
    fit <- lapply(dat_imputed, function(dat) lm(as.formula(frml), dat))
  }
  
  # Pooling etc.
  tab <- summary(pool(fit), "all", conf.int = TRUE)
  res <- as.data.table(tab[-1, c("term", "estimate", "2.5 %", "97.5 %"), drop = FALSE])
  rownames(res) <- NULL
  colnames(res)[3:4] <- c("lower", "upper")
  res[, truth := instance$beta]
  res
}

# Evaluate NRMSE of the data ----------------------------------------------
eval_nrmse <- function(instance, dat_imputed) {
  # If multiple imputation, average (mean or mode)
  dat_imputed <- average_datasets(dat_imputed)
  
  # If still any missings (happens e.g. for mice with constant variables), median imputation
  if (any(is.na(dat_imputed))) {
    dat_imputed <- impute_median(dat_imputed)
  }
  
  # Recode integers
  idx_integer <- which(sapply(instance$complete, is.integer))
  if (sum(idx_integer) > 0L) {
    dat_imputed[, idx_integer] <- lapply(idx_integer, function(j) {
      if (is.numeric(dat_imputed[[j]])) {
        as.integer(round(dat_imputed[[j]]))
      } else {
        as.integer(as.character(dat_imputed[[j]]))
      }
    }) 
  }
  
  # Normalize
  pos = po("scale", center = TRUE, scale = TRUE)
  dat_complete_scaled <- pos$train(list(as_task_unsupervised(instance$complete)))[[1]]$data(cols = colnames(instance$incomplete))
  dat_imputed_scaled <- pos$predict(list(as_task_unsupervised(dat_imputed)))[[1]]$data(cols = colnames(instance$incomplete))
  
  # Convert to numeric
  idx_numeric <- sapply(instance$incomplete, is.numeric)
  x_na <- as.matrix(instance$incomplete[, idx_numeric, with = FALSE])
  x_imputed <- as.matrix(dat_imputed_scaled[, idx_numeric, with = FALSE])
  x_complete <- as.matrix(dat_complete_scaled[, idx_numeric, with = FALSE])
  idx_na <- is.na(x_na)
  
  # Compute RMSE
  sqrt(mean((x_imputed[idx_na] - x_complete[idx_na])^2))
}

# Evaluate prediction performance of logistic regression --------------------
eval_prediction <- function(instance, dat_imputed_train, dat_imputed_test) {
  # If multiple imputation, average (mean or mode)
  dat_imputed_train <- average_datasets(dat_imputed_train)
  dat_imputed_test <- average_datasets(dat_imputed_test)
  
  # If still any missings (happens e.g. for mice with constant variables), median imputation
  if (any(is.na(dat_imputed_train))) {
    dat_imputed_train <- impute_median(dat_imputed_train)
  }
  if (any(is.na(dat_imputed_test))) {
    dat_imputed_test <- impute_median(dat_imputed_test)
  }
  
  if (!is.null(instance$train$effect) && instance$train$effect == "squared") {
    frml <- paste0("y ~ ", paste0("I(", colnames(dat_imputed_train)[-1], "^2)", collapse = " + "))
  } else {
    frml <- paste0("y ~ ", paste0(colnames(dat_imputed_train)[-1], collapse = " + "))
  }
  
  # Fit/predict/evaluate model
  if (is.factor(dat_imputed_test$y)) {
    # Train/predict/evaluate model
    fit <- glm(as.formula(frml), dat_imputed_train, family = "binomial")
    pred <- predict(fit, dat_imputed_test, type = "response")
    mlr3measures::bbrier(dat_imputed_test$y, pred, positive = "1")
  } else {
    fit <- lm(as.formula(frml), dat_imputed_train)
    pred <- predict(fit, newdata = dat_imputed_test)
    mlr3measures::rmse(dat_imputed_test$y, pred)
  }
}

# Combined evaluation function --------------------------------------------
eval_fun <- function(instance, train_imputed, test_imputed, eval) {
  if (eval == "coverage") {
    eval_coverage(instance$train, train_imputed)
  } else if (eval == "nrmse") {
    eval_nrmse(instance$train, train_imputed)
  } else if (eval == "prediction") {
    eval_prediction(instance, train_imputed, test_imputed)
  } else {
    stop("Unknown evaluation.")
  }
}

