
library(batchtools)
source("setup.R")

# Simulation study: NRMSE ---------------------------------------------
reg_name <- "logreg_nrmse"

set.seed(42)

# Problem args
missing_probs <- c(0.1, 0.2, 0.4)  
patterns <- c("MCAR", "MAR", "MNAR")
cov_base <- 0.5
beta_max <- 0.5
n <- c(1000, 10000)
p <- c(4, 10, 20)
dist <- c("normal", "uniform", "binary", "poisson", "gamma")
effect <- c("linear", "squared")
repls <- 1000

# Single imputation
m <- 1

# arf args
finite_bounds <- "local" 
num_trees <- 100
min_node_size <- 10
expectation <- TRUE

# mice args
method <- c("pmm", "rf")

# missRanger args
pmm.k <- c(0, 5)
num.trees <- 100

# missForest args
ntree <- 100

# Registry ----------------------------------------------------------------
if (!file.exists("registries")) dir.create("registries")
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE)
makeExperimentRegistry(file.dir = reg_dir, seed = 42, 
                       source = c("utils.R", "problems.R", "algorithms.R"))

# Problems -----------------------------------------------------------
addProblem(name = "sim_data", fun = sim_fun, seed = 43)

# Algorithms -----------------------------------------------------------
addAlgorithm(name = "mice", fun = mice_fun)
addAlgorithm(name = "arf", fun = arf_fun)
addAlgorithm(name = "missRanger", fun = missRanger_fun)
#addAlgorithm(name = "missForest", fun = missForest_fun)
addAlgorithm(name = "median", fun = median_fun)
addAlgorithm(name = "random", fun = random_fun)

# Experiments -----------------------------------------------------------
prob_design <- list(
  sim_data = expand.grid(
    traintest = FALSE,
    n = n, 
    p = p, 
    prop_mis = missing_probs,
    pattern = patterns, 
    cov_base = cov_base,
    outcome = "classif", 
    beta_max = beta_max, 
    dist = dist,
    effect = effect,
    stringsAsFactors = FALSE
  )
)

algo_design <- list(
  mice = expand.grid(m = m,
                     eval = "nrmse",
                     method = method, 
                     stringsAsFactors = FALSE),
  arf = expand.grid(m = m,
                    expectation = expectation,
                    eval = "nrmse",
                    finite_bounds = finite_bounds,
                    num_trees = num_trees,
                    min_node_size = min_node_size,
                    stringsAsFactors = FALSE), 
  missRanger = expand.grid(m = m,
                           eval = "nrmse",
                           pmm.k = pmm.k,
                           num.trees = num.trees,
                           stringsAsFactors = FALSE), 
  # missForest = expand.grid(m = m,
  #                          eval = "nrmse",
  #                          ntree = ntree,
  #                          stringsAsFactors = FALSE),
  median = expand.grid(eval = "nrmse",
                       stringsAsFactors = FALSE), 
  random = expand.grid(m = m,
                       eval = "nrmse",
                       stringsAsFactors = FALSE)
)

addExperiments(prob_design, algo_design, repls = repls)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
testJob(id = 1) 

# Submit -----------------------------------------------------------
ids <- findNotSubmitted()
ids[, chunk := chunk(job.id, n.chunks = 100)]
submitJobs(ids)
waitForJobs()

# Get/save results ---------------------------------------------------------
res <- ijoin(unwrap(getJobPars()), reduceResultsDataTable()[, as.data.table(result), by = job.id])
saveRDS(res, file.path(path, paste0(reg_name, ".rds")))
