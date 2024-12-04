
library(batchtools)
source("setup.R")

# Real data - diabetes: PRED ---------------------------------------------
reg_name <- "diabetes_pred"

set.seed(42)

# Problem args
missing_probs <- c(0.1, 0.2, 0.4)  
patterns <- c("MCAR", "MAR", "MNAR")
n <- 1000
repls <- 100 

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
addProblem(name = "real_data", fun = real_fun, seed = 43)

# Algorithms -----------------------------------------------------------
addAlgorithm(name = "mice", fun = mice_fun)
addAlgorithm(name = "arf", fun = arf_fun)
addAlgorithm(name = "missRanger", fun = missRanger_fun)
# addAlgorithm(name = "missForest", fun = missForest_fun)
addAlgorithm(name = "median", fun = median_fun)
addAlgorithm(name = "random", fun = random_fun)

# Experiments -----------------------------------------------------------
prob_design <- list(
  real_data = expand.grid(
    traintest = TRUE,
    n = n, 
    prop_mis = missing_probs,
    pattern = patterns, 
    stringsAsFactors = FALSE
  )
)

algo_design <- list(
  mice = expand.grid(m = m,
                     eval = "prediction",
                     method = method, 
                     stringsAsFactors = FALSE),
  arf = expand.grid(m = m,
                    expectation = expectation,
                    eval = "prediction",
                    finite_bounds = finite_bounds,
                    num_trees = num_trees,
                    min_node_size = min_node_size,
                    stringsAsFactors = FALSE), 
  missRanger = expand.grid(m = m,
                           eval = "prediction",
                           pmm.k = pmm.k,
                           num.trees = num.trees,
                           stringsAsFactors = FALSE), 
  # missForest = expand.grid(m = m,
  #                          eval = "prediction",
  #                          ntree = ntree,
  #                          stringsAsFactors = FALSE),
  median = expand.grid(eval = "prediction",
                       stringsAsFactors = FALSE), 
  random = expand.grid(m = m,
                       eval = "prediction",
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
