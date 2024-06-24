
library(mvtnorm)
library(data.table)
library(missMethods)
library(simstudy)

# Simulate data -----------------------------------------------------------
sim_data <- function(n, p, prop_mis = 0.1, pattern = "MCAR", 
                     cov_base = 0.5, outcome = "classif", beta_max = 0.5, 
                     dist = "normal", effect = "linear", ...) {
  
  beta0 <- 0
  beta <- seq(-beta_max, beta_max, length.out = p)

  # Simulate data
  sigma <- toeplitz(cov_base^(0:(p-1)))
  params2 <- NULL
  if (dist == "normal") {
    params1 <- rep(0, p)
    params2 <- rep(1, p)
  } else if (dist == "binary") {
    params1 <- rep(0.5, p)
  } else if (dist == "poisson") {
    params1 <- rep(2, p)
  } else if (dist == "gamma") {
    params1 <- rep(2, p)
    params2 <- rep(0.5, p)
  } else if (dist == "uniform") {
    params1 <- rep(-1, p)
    params2 <- rep(1, p)
  } else {
    stop("Unknown distribution.")
  }
  x <- genCorGen(n = n, nvars = p, corMatrix = sigma, 
                 params1 = params1, params2 = params2, 
                 dist = dist, wide = TRUE)
  x[, id := NULL]
  colnames(x) <- paste0('x', seq_len(p))
  
  # Simulate outcome
  if (effect == "linear") {
    lp <- as.matrix(x) %*% beta + beta0 
  } else if (effect == "squared") {
    lp <- as.matrix(x)^2 %*% beta + beta0
  } else {
    stop("Unknown effect.")
  } 
  if (outcome == "regr") {
    y <- lp[, 1] + rnorm(n)
  } else if (outcome == "classif") {
    y <- as.factor(rbinom(n, 1, plogis(lp[, 1])))
  } else {
    stop("Unknown outcome type.")
  }
  
  dat_complete <- data.table(y = y, x)
  
  # Simulate missings 
  feat_names <- setdiff(colnames(dat_complete), "y")
  cols_mis <- sample(feat_names, floor(length(feat_names) / 2))
  if (pattern == "MCAR") {
    dat_na <- delete_MCAR(dat_complete, p = prop_mis, cols_mis = cols_mis)
  } else if (pattern == "MAR") {
    cols_ctrl <- setdiff(feat_names, cols_mis)
    if (length(cols_ctrl) != length(cols_mis)) {
      # If uneven number features, remove one from cols_ctrl
      cols_ctrl <- cols_ctrl[-1]
    }
    dat_na <- delete_MAR_one_group(dat_complete, p = prop_mis, cols_mis = cols_mis, cols_ctrl = cols_ctrl)
  } else if (pattern == "MNAR") {
    dat_na <- delete_MNAR_one_group(dat_complete, p = prop_mis, cols_mis = cols_mis)
  } else {
    stop("Unknown pattern.")
  }
  
  # Return complete and incomplete data
  list(complete = dat_complete, 
       incomplete = dat_na, 
       beta = beta, 
       effect = effect)
}

sim_fun <- function(data, job, traintest = FALSE, ...) {
  train <- sim_data(...)
  if (traintest) {
    test <- sim_data(...)
  } else {
    test <- NULL
  }
  
  list(train = train, 
       test = test)
}

