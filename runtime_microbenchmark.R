# Runtime comparison of imputation methods
# Run once for one thread and once for 16 threads, changing the relevant text passages.

library(doParallel)
library(microbenchmark)
source("setup.R")

set.seed(123)
#seed=123

## Set threads to 1 or 16
k=16 #1

# For e.g. XGBoost
Sys.setenv(OMP_NUM_THREADS = k)
Sys.setenv(OMP_THREAD_LIMIT = k)
# Unsure, MKL is an Intel-specific thing
Sys.setenv(MKL_NUM_THREADS = k)
# Package-specific settings
try(data.table::setDTthreads(k))
try(RhpcBLASctl::blas_set_num_threads(k))
try(RhpcBLASctl::omp_set_num_threads(k))

#for ranger
options(Ncpus = k)
#for arf
doParallel::registerDoParallel(cores = k)

if (k == 1) {
  parallel = FALSE
} else {
  parallel = TRUE
}

## Simulate data sets: normal distribution with linear effect with different n, p and prop_mis
#n=500
df_500_4_01 <- sim_data(n=500, p=4, prop_mis = 0.1, pattern = "MAR")$incomplete 
df_500_10_01 <- sim_data(n=500, p=10, prop_mis = 0.1, pattern = "MAR")$incomplete
df_500_20_01 <- sim_data(n=500, p=20, prop_mis = 0.1, pattern = "MAR")$incomplete

df_500_4_02 <- sim_data(n=500, p=4, prop_mis = 0.2, pattern = "MAR")$incomplete 
df_500_10_02 <- sim_data(n=500, p=10, prop_mis = 0.2, pattern = "MAR")$incomplete
df_500_20_02 <- sim_data(n=500, p=20, prop_mis = 0.2, pattern = "MAR")$incomplete

df_500_4_04 <- sim_data(n=500, p=4, prop_mis = 0.4, pattern = "MAR")$incomplete 
df_500_10_04 <- sim_data(n=500, p=10, prop_mis = 0.4, pattern = "MAR")$incomplete
df_500_20_04 <- sim_data(n=500, p=20, prop_mis = 0.4, pattern = "MAR")$incomplete

#n=1000
df_1000_4_01 <- sim_data(n=1000, p=4, prop_mis = 0.1, pattern = "MAR")$incomplete 
df_1000_10_01 <- sim_data(n=1000, p=10, prop_mis = 0.1, pattern = "MAR")$incomplete
df_1000_20_01 <- sim_data(n=1000, p=20, prop_mis = 0.1, pattern = "MAR")$incomplete

df_1000_4_02 <- sim_data(n=1000, p=4, prop_mis = 0.2, pattern = "MAR")$incomplete 
df_1000_10_02 <- sim_data(n=1000, p=10, prop_mis = 0.2, pattern = "MAR")$incomplete
df_1000_20_02 <- sim_data(n=1000, p=20, prop_mis = 0.2, pattern = "MAR")$incomplete

df_1000_4_04 <- sim_data(n=1000, p=4, prop_mis = 0.4, pattern = "MAR")$incomplete 
df_1000_10_04 <- sim_data(n=1000, p=10, prop_mis = 0.4, pattern = "MAR")$incomplete
df_1000_20_04 <- sim_data(n=1000, p=20, prop_mis = 0.4, pattern = "MAR")$incomplete

#n=10000
df_10000_4_01 <- sim_data(n=10000, p=4, prop_mis = 0.1, pattern = "MAR")$incomplete 
df_10000_10_01 <- sim_data(n=10000, p=10, prop_mis = 0.1, pattern = "MAR")$incomplete
df_10000_20_01 <- sim_data(n=10000, p=20, prop_mis = 0.1, pattern = "MAR")$incomplete

df_10000_4_02 <- sim_data(n=10000, p=4, prop_mis = 0.2, pattern = "MAR")$incomplete 
df_10000_10_02 <- sim_data(n=10000, p=10, prop_mis = 0.2, pattern = "MAR")$incomplete
df_10000_20_02 <- sim_data(n=10000, p=20, prop_mis = 0.2, pattern = "MAR")$incomplete

df_10000_4_04 <- sim_data(n=10000, p=4, prop_mis = 0.4, pattern = "MAR")$incomplete 
df_10000_10_04 <- sim_data(n=10000, p=10, prop_mis = 0.4, pattern = "MAR")$incomplete
df_10000_20_04 <- sim_data(n=10000, p=20, prop_mis = 0.4, pattern = "MAR")$incomplete

# Compare runtimes
runtime_k16 <- microbenchmark(
  #n=500
  
  ###p=4
  ##df_500_4_01
  #single
  median_sing_500_4_01 = impute_median(df_500_4_01, m=1),
  random_sing_500_4_01 = impute_random(df_500_4_01, m=1),
  mice_pmm_sing_500_4_01 = impute_mice(df_500_4_01, m=1),
  mice_rf_sing_500_4_01 = impute_mice(df_500_4_01, method="rf", m=1),
  arf_sing_500_4_01 = impute_arf(df_500_4_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_4_01 = impute_missRanger(df_500_4_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_4_01 = impute_missRanger(df_500_4_01, pmm.k = 5, m=1, num.threads = k),
  #multiple
  random_multi_500_4_01 = impute_random(df_500_4_01, m=20),
  mice_pmm_multi_500_4_01= impute_mice(df_500_4_01, m=20),
  mice_rf_multi_500_4_01= impute_mice(df_500_4_01, method="rf", m=20),
  arf_multi_500_4_01= impute_arf(df_500_4_01, m=20, parallel = parallel),
  missranger_multi_500_4_01= impute_missRanger(df_500_4_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_4_01= impute_missRanger(df_500_4_01, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_4_02
  #single
  median_sing_500_4_02 = impute_median(df_500_4_02, m=1),
  random_sing_500_4_02 = impute_random(df_500_4_02, m=1),
  mice_pmm_sing_500_4_02 = impute_mice(df_500_4_02, m=1),
  mice_rf_sing_500_4_02 = impute_mice(df_500_4_02, method="rf", m=1),
  arf_sing_500_4_02 = impute_arf(df_500_4_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_4_02 = impute_missRanger(df_500_4_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_4_02 = impute_missRanger(df_500_4_02, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_4_02 = impute_random(df_500_4_02, m=20),
  mice_pmm_multi_500_4_02= impute_mice(df_500_4_02, m=20),
  mice_rf_multi_500_4_02= impute_mice(df_500_4_02, method="rf", m=20),
  arf_multi_500_4_02= impute_arf(df_500_4_02, m=20, parallel = parallel),
  missranger_multi_500_4_02= impute_missRanger(df_500_4_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_4_02= impute_missRanger(df_500_4_02, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_4_04
  #single
  median_sing_500_4_04 = impute_median(df_500_4_04, m=1),
  random_sing_500_4_04 = impute_random(df_500_4_04, m=1),
  mice_pmm_sing_500_4_04 = impute_mice(df_500_4_04, m=1),
  mice_rf_sing_500_4_04 = impute_mice(df_500_4_04, method="rf", m=1),
  arf_sing_500_4_04 = impute_arf(df_500_4_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_4_04 = impute_missRanger(df_500_4_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_4_04 = impute_missRanger(df_500_4_04, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_4_04 = impute_random(df_500_4_04, m=20),
  mice_pmm_multi_500_4_04= impute_mice(df_500_4_04, m=20),
  mice_rf_multi_500_4_04= impute_mice(df_500_4_04, method="rf", m=20),
  arf_multi_500_4_04= impute_arf(df_500_4_04, m=20, parallel = parallel),
  missranger_multi_500_4_04= impute_missRanger(df_500_4_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_4_04= impute_missRanger(df_500_4_04, pmm.k = 5, m=20, num.threads = k),
  
  ###p=10
  ##df_500_10_01
  #single
  median_sing_500_10_01 = impute_median(df_500_10_01, m=1),
  random_sing_500_10_01 = impute_random(df_500_10_01, m=1),
  mice_pmm_sing_500_10_01 = impute_mice(df_500_10_01, m=1),
  mice_rf_sing_500_10_01 = impute_mice(df_500_10_01, method="rf", m=1),
  arf_sing_500_10_01 = impute_arf(df_500_10_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_10_01 = impute_missRanger(df_500_10_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_10_01 = impute_missRanger(df_500_10_01, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_10_01 = impute_random(df_500_10_01, m=20),
  mice_pmm_multi_500_10_01= impute_mice(df_500_10_01, m=20),
  mice_rf_multi_500_10_01= impute_mice(df_500_10_01, method="rf", m=20),
  arf_multi_500_10_01= impute_arf(df_500_10_01, m=20, parallel = parallel),
  missranger_multi_500_10_01= impute_missRanger(df_500_10_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_10_01= impute_missRanger(df_500_10_01, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_10_02
  #single
  median_sing_500_10_02 = impute_median(df_500_10_02, m=1),
  random_sing_500_10_02 = impute_random(df_500_10_02, m=1),
  mice_pmm_sing_500_10_02 = impute_mice(df_500_10_02, m=1),
  mice_rf_sing_500_10_02 = impute_mice(df_500_10_02, method="rf", m=1),
  arf_sing_500_10_02 = impute_arf(df_500_10_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_10_02 = impute_missRanger(df_500_10_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_10_02 = impute_missRanger(df_500_10_02, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_10_02 = impute_random(df_500_10_02, m=20),
  mice_pmm_multi_500_10_02= impute_mice(df_500_10_02, m=20),
  mice_rf_multi_500_10_02= impute_mice(df_500_10_02, method="rf", m=20),
  arf_multi_500_10_02= impute_arf(df_500_10_02, m=20, parallel = parallel),
  missranger_multi_500_10_02= impute_missRanger(df_500_10_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_10_02= impute_missRanger(df_500_10_02, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_10_04
  #single
  median_sing_500_10_04 = impute_median(df_500_10_04, m=1),
  random_sing_500_10_04 = impute_random(df_500_10_04, m=1),
  mice_pmm_sing_500_10_04 = impute_mice(df_500_10_04, m=1),
  mice_rf_sing_500_10_04 = impute_mice(df_500_10_04, method="rf", m=1),
  arf_sing_500_10_04 = impute_arf(df_500_10_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_10_04 = impute_missRanger(df_500_10_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_10_04 = impute_missRanger(df_500_10_04, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_10_04 = impute_random(df_500_10_04, m=20),
  mice_pmm_multi_500_10_04= impute_mice(df_500_10_04, m=20),
  mice_rf_multi_500_10_04= impute_mice(df_500_10_04, method="rf", m=20),
  arf_multi_500_10_04= impute_arf(df_500_10_04, m=20, parallel = parallel),
  missranger_multi_500_10_04= impute_missRanger(df_500_10_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_10_04= impute_missRanger(df_500_10_04, pmm.k = 5, m=20, num.threads = k),
  
  ###p=20
  ##df_500_20_01
  #single
  median_sing_500_20_01 = impute_median(df_500_20_01, m=1),
  random_sing_500_20_01 = impute_random(df_500_20_01, m=1),
  mice_pmm_sing_500_20_01 = impute_mice(df_500_20_01, m=1),
  mice_rf_sing_500_20_01 = impute_mice(df_500_20_01, method="rf", m=1),
  arf_sing_500_20_01 = impute_arf(df_500_20_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_20_01 = impute_missRanger(df_500_20_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_20_01 = impute_missRanger(df_500_20_01, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_20_01 = impute_random(df_500_20_01, m=20),
  mice_pmm_multi_500_20_01= impute_mice(df_500_20_01, m=20),
  mice_rf_multi_500_20_01= impute_mice(df_500_20_01, method="rf", m=20),
  arf_multi_500_20_01= impute_arf(df_500_20_01, m=20, parallel = parallel),
  missranger_multi_500_20_01= impute_missRanger(df_500_20_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_20_01= impute_missRanger(df_500_20_01, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_20_02
  #single
  median_sing_500_20_02 = impute_median(df_500_20_02, m=1),
  random_sing_500_20_02 = impute_random(df_500_20_02, m=1),
  mice_pmm_sing_500_20_02 = impute_mice(df_500_20_02, m=1),
  mice_rf_sing_500_20_02 = impute_mice(df_500_20_02, method="rf", m=1),
  arf_sing_500_20_02 = impute_arf(df_500_20_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_20_02 = impute_missRanger(df_500_20_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_20_02 = impute_missRanger(df_500_20_02, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_20_02 = impute_random(df_500_20_02, m=20),
  mice_pmm_multi_500_20_02= impute_mice(df_500_20_02, m=20),
  mice_rf_multi_500_20_02= impute_mice(df_500_20_02, method="rf", m=20),
  arf_multi_500_20_02= impute_arf(df_500_20_02, m=20, parallel = parallel),
  missranger_multi_500_20_02= impute_missRanger(df_500_20_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_20_02= impute_missRanger(df_500_20_02, pmm.k = 5, m=20, num.threads = k),
  
  ##df_500_20_04
  #single
  median_sing_500_20_04 = impute_median(df_500_20_04, m=1),
  random_sing_500_20_04 = impute_random(df_500_20_04, m=1),
  mice_pmm_sing_500_20_04 = impute_mice(df_500_20_04, m=1),
  mice_rf_sing_500_20_04 = impute_mice(df_500_20_04, method="rf", m=1),
  arf_sing_500_20_04 = impute_arf(df_500_20_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_500_20_04 = impute_missRanger(df_500_20_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_500_20_04 = impute_missRanger(df_500_20_04, pmm.k = 5, m=1, num.threads = k),
  
  #multiple
  random_multi_500_20_04 = impute_random(df_500_20_04, m=20),
  mice_pmm_multi_500_20_04= impute_mice(df_500_20_04, m=20),
  mice_rf_multi_500_20_04= impute_mice(df_500_20_04, method="rf", m=20),
  arf_multi_500_20_04= impute_arf(df_500_20_04, m=20, parallel = parallel),
  missranger_multi_500_20_04= impute_missRanger(df_500_20_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_500_20_04= impute_missRanger(df_500_20_04, pmm.k = 5, m=20, num.threads = k),
  
  ##############################################################################
  #n=1000
  ###p=4
  ##df_1000_4_01
  #single
  median_sing_1000_4_01 = impute_median(df_1000_4_01, m=1),
  random_sing_1000_4_01 = impute_random(df_1000_4_01, m=1),
  mice_pmm_sing_1000_4_01 = impute_mice(df_1000_4_01, m=1),
  mice_rf_sing_1000_4_01 = impute_mice(df_1000_4_01, method="rf", m=1),
  arf_sing_1000_4_01 = impute_arf(df_1000_4_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_4_01 = impute_missRanger(df_1000_4_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_4_01 = impute_missRanger(df_1000_4_01, pmm.k = 5, m=1, num.threads = k),
  #multiple
  random_multi_1000_4_01 = impute_random(df_1000_4_01, m=20),
  mice_pmm_multi_1000_4_01= impute_mice(df_1000_4_01, m=20),
  mice_rf_multi_1000_4_01= impute_mice(df_1000_4_01, method="rf", m=20),
  arf_multi_1000_4_01= impute_arf(df_1000_4_01, m=20, parallel = parallel),
  missranger_multi_1000_4_01= impute_missRanger(df_1000_4_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_4_01= impute_missRanger(df_1000_4_01, pmm.k = 5, m=20, num.threads = k),
  
  ##df_1000_4_02
  #single
  median_sing_1000_4_02 = impute_median(df_1000_4_02, m=1),
  random_sing_1000_4_02 = impute_random(df_1000_4_02, m=1),
  mice_pmm_sing_1000_4_02 = impute_mice(df_1000_4_02, m=1),
  mice_rf_sing_1000_4_02 = impute_mice(df_1000_4_02, method="rf", m=1),
  arf_sing_1000_4_02 = impute_arf(df_1000_4_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_4_02 = impute_missRanger(df_1000_4_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_4_02 = impute_missRanger(df_1000_4_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_4_02 = impute_random(df_1000_4_02, m=20),
  mice_pmm_multi_1000_4_02= impute_mice(df_1000_4_02, m=20),
  mice_rf_multi_1000_4_02= impute_mice(df_1000_4_02, method="rf", m=20),
  arf_multi_1000_4_02= impute_arf(df_1000_4_02, m=20, parallel = parallel),
  missranger_multi_1000_4_02= impute_missRanger(df_1000_4_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_4_02= impute_missRanger(df_1000_4_02, pmm.k = 5, m=20, num.threads = k),
  
  ##df_1000_4_04
  #single
  median_sing_1000_4_04 = impute_median(df_1000_4_04, m=1),
  random_sing_1000_4_04 = impute_random(df_1000_4_04, m=1),
  mice_pmm_sing_1000_4_04 = impute_mice(df_1000_4_04, m=1),
  mice_rf_sing_1000_4_04 = impute_mice(df_1000_4_04, method="rf", m=1),
  arf_sing_1000_4_04 = impute_arf(df_1000_4_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_4_04 = impute_missRanger(df_1000_4_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_4_04 = impute_missRanger(df_1000_4_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_4_04 = impute_random(df_1000_4_04, m=20),
  mice_pmm_multi_1000_4_04= impute_mice(df_1000_4_04, m=20),
  mice_rf_multi_1000_4_04= impute_mice(df_1000_4_04, method="rf", m=20),
  arf_multi_1000_4_04= impute_arf(df_1000_4_04, m=20, parallel = parallel),
  missranger_multi_1000_4_04= impute_missRanger(df_1000_4_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_4_04= impute_missRanger(df_1000_4_04, pmm.k = 5, m=20, num.threads = k),
  
  ###p=10
  ##df_1000_10_01
  #single
  median_sing_1000_10_01 = impute_median(df_1000_10_01, m=1),
  random_sing_1000_10_01 = impute_random(df_1000_10_01, m=1),
  mice_pmm_sing_1000_10_01 = impute_mice(df_1000_10_01, m=1),
  mice_rf_sing_1000_10_01 = impute_mice(df_1000_10_01, method="rf", m=1),
  arf_sing_1000_10_01 = impute_arf(df_1000_10_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_10_01 = impute_missRanger(df_1000_10_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_10_01 = impute_missRanger(df_1000_10_01, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_10_01 = impute_random(df_1000_10_01, m=20),
  mice_pmm_multi_1000_10_01= impute_mice(df_1000_10_01, m=20),
  mice_rf_multi_1000_10_01= impute_mice(df_1000_10_01, method="rf", m=20),
  arf_multi_1000_10_01= impute_arf(df_1000_10_01, m=20, parallel = parallel),
  missranger_multi_1000_10_01= impute_missRanger(df_1000_10_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_10_01= impute_missRanger(df_1000_10_01, pmm.k = 5, m=20, num.threads = k),
  
  ##df_1000_10_02
  #single
  median_sing_1000_10_02 = impute_median(df_1000_10_02, m=1),
  random_sing_1000_10_02 = impute_random(df_1000_10_02, m=1),
  mice_pmm_sing_1000_10_02 = impute_mice(df_1000_10_02, m=1),
  mice_rf_sing_1000_10_02 = impute_mice(df_1000_10_02, method="rf", m=1),
  arf_sing_1000_10_02 = impute_arf(df_1000_10_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_10_02 = impute_missRanger(df_1000_10_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_10_02 = impute_missRanger(df_1000_10_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_10_02 = impute_random(df_1000_10_02, m=20),
  mice_pmm_multi_1000_10_02= impute_mice(df_1000_10_02, m=20),
  mice_rf_multi_1000_10_02= impute_mice(df_1000_10_02, method="rf", m=20),
  arf_multi_1000_10_02= impute_arf(df_1000_10_02, m=20, parallel = parallel),
  missranger_multi_1000_10_02= impute_missRanger(df_1000_10_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_10_02= impute_missRanger(df_1000_10_02, pmm.k = 5, m=20, num.threads = k),
  
  ##df_1000_10_04
  #single
  median_sing_1000_10_04 = impute_median(df_1000_10_04, m=1),
  random_sing_1000_10_04 = impute_random(df_1000_10_04, m=1),
  mice_pmm_sing_1000_10_04 = impute_mice(df_1000_10_04, m=1),
  mice_rf_sing_1000_10_04 = impute_mice(df_1000_10_04, method="rf", m=1),
  arf_sing_1000_10_04 = impute_arf(df_1000_10_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_10_04 = impute_missRanger(df_1000_10_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_10_04 = impute_missRanger(df_1000_10_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_10_04 = impute_random(df_1000_10_04, m=20),
  mice_pmm_multi_1000_10_04= impute_mice(df_1000_10_04, m=20),
  mice_rf_multi_1000_10_04= impute_mice(df_1000_10_04, method="rf", m=20),
  arf_multi_1000_10_04= impute_arf(df_1000_10_04, m=20, parallel = parallel),
  missranger_multi_1000_10_04= impute_missRanger(df_1000_10_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_10_04= impute_missRanger(df_1000_10_04, pmm.k = 5, m=20, num.threads = k),

  ###p=20
  ##df_1000_20_01
  #single
  median_sing_1000_20_01 = impute_median(df_1000_20_01, m=1),
  random_sing_1000_20_01 = impute_random(df_1000_20_01, m=1),
  mice_pmm_sing_1000_20_01 = impute_mice(df_1000_20_01, m=1),
  mice_rf_sing_1000_20_01 = impute_mice(df_1000_20_01, method="rf", m=1),
  arf_sing_1000_20_01 = impute_arf(df_1000_20_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_20_01 = impute_missRanger(df_1000_20_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_20_01 = impute_missRanger(df_1000_20_01, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_20_01 = impute_random(df_1000_20_01, m=20),
  mice_pmm_multi_1000_20_01= impute_mice(df_1000_20_01, m=20),
  mice_rf_multi_1000_20_01= impute_mice(df_1000_20_01, method="rf", m=20),
  arf_multi_1000_20_01= impute_arf(df_1000_20_01, m=20, parallel = parallel),
  missranger_multi_1000_20_01= impute_missRanger(df_1000_20_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_20_01= impute_missRanger(df_1000_20_01, pmm.k = 5, m=20, num.threads = k),

  ##df_1000_20_02
  #single
  median_sing_1000_20_02 = impute_median(df_1000_20_02, m=1),
  random_sing_1000_20_02 = impute_random(df_1000_20_02, m=1),
  mice_pmm_sing_1000_20_02 = impute_mice(df_1000_20_02, m=1),
  mice_rf_sing_1000_20_02 = impute_mice(df_1000_20_02, method="rf", m=1),
  arf_sing_1000_20_02 = impute_arf(df_1000_20_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_20_02 = impute_missRanger(df_1000_20_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_20_02 = impute_missRanger(df_1000_20_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_20_02 = impute_random(df_1000_20_02, m=20),
  mice_pmm_multi_1000_20_02= impute_mice(df_1000_20_02, m=20),
  mice_rf_multi_1000_20_02= impute_mice(df_1000_20_02, method="rf", m=20),
  arf_multi_1000_20_02= impute_arf(df_1000_20_02, m=20, parallel = parallel),
  missranger_multi_1000_20_02= impute_missRanger(df_1000_20_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_20_02= impute_missRanger(df_1000_20_02, pmm.k = 5, m=20, num.threads = k),

  ##df_1000_20_04
  #single
  median_sing_1000_20_04 = impute_median(df_1000_20_04, m=1),
  random_sing_1000_20_04 = impute_random(df_1000_20_04, m=1),
  mice_pmm_sing_1000_20_04 = impute_mice(df_1000_20_04, m=1),
  mice_rf_sing_1000_20_04 = impute_mice(df_1000_20_04, method="rf", m=1),
  arf_sing_1000_20_04 = impute_arf(df_1000_20_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_1000_20_04 = impute_missRanger(df_1000_20_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_1000_20_04 = impute_missRanger(df_1000_20_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_1000_20_04 = impute_random(df_1000_20_04, m=20),
  mice_pmm_multi_1000_20_04= impute_mice(df_1000_20_04, m=20),
  mice_rf_multi_1000_20_04= impute_mice(df_1000_20_04, method="rf", m=20),
  arf_multi_1000_20_04= impute_arf(df_1000_20_04, m=20, parallel = parallel),
  missranger_multi_1000_20_04= impute_missRanger(df_1000_20_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_1000_20_04= impute_missRanger(df_1000_20_04, pmm.k = 5, m=20, num.threads = k),

  ##############################################################################
  #n=10000
  
  ###p=4
  ##df_10000_4_01
  #single
  median_sing_10000_4_01 = impute_median(df_10000_4_01, m=1),
  random_sing_10000_4_01 = impute_random(df_10000_4_01, m=1),
  mice_pmm_sing_10000_4_01 = impute_mice(df_10000_4_01, m=1),
  mice_rf_sing_10000_4_01 = impute_mice(df_10000_4_01, method="rf", m=1),
  arf_sing_10000_4_01 = impute_arf(df_10000_4_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_4_01 = impute_missRanger(df_10000_4_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_4_01 = impute_missRanger(df_10000_4_01, pmm.k = 5, m=1, num.threads = k),
  #multiple
  random_multi_10000_4_01 = impute_random(df_10000_4_01, m=20),
  mice_pmm_multi_10000_4_01= impute_mice(df_10000_4_01, m=20),
  mice_rf_multi_10000_4_01= impute_mice(df_10000_4_01, method="rf", m=20),
  arf_multi_10000_4_01= impute_arf(df_10000_4_01, m=20, parallel = parallel),
  missranger_multi_10000_4_01= impute_missRanger(df_10000_4_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_4_01= impute_missRanger(df_10000_4_01, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_4_02
  #single
  median_sing_10000_4_02 = impute_median(df_10000_4_02, m=1),
  random_sing_10000_4_02 = impute_random(df_10000_4_02, m=1),
  mice_pmm_sing_10000_4_02 = impute_mice(df_10000_4_02, m=1),
  mice_rf_sing_10000_4_02 = impute_mice(df_10000_4_02, method="rf", m=1),
  arf_sing_10000_4_02 = impute_arf(df_10000_4_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_4_02 = impute_missRanger(df_10000_4_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_4_02 = impute_missRanger(df_10000_4_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_4_02 = impute_random(df_10000_4_02, m=20),
  mice_pmm_multi_10000_4_02= impute_mice(df_10000_4_02, m=20),
  mice_rf_multi_10000_4_02= impute_mice(df_10000_4_02, method="rf", m=20),
  arf_multi_10000_4_02= impute_arf(df_10000_4_02, m=20, parallel = parallel),
  missranger_multi_10000_4_02= impute_missRanger(df_10000_4_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_4_02= impute_missRanger(df_10000_4_02, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_4_04
  #single
  median_sing_10000_4_04 = impute_median(df_10000_4_04, m=1),
  random_sing_10000_4_04 = impute_random(df_10000_4_04, m=1),
  mice_pmm_sing_10000_4_04 = impute_mice(df_10000_4_04, m=1),
  mice_rf_sing_10000_4_04 = impute_mice(df_10000_4_04, method="rf", m=1),
  arf_sing_10000_4_04 = impute_arf(df_10000_4_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_4_04 = impute_missRanger(df_10000_4_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_4_04 = impute_missRanger(df_10000_4_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_4_04 = impute_random(df_10000_4_04, m=20),
  mice_pmm_multi_10000_4_04= impute_mice(df_10000_4_04, m=20),
  mice_rf_multi_10000_4_04= impute_mice(df_10000_4_04, method="rf", m=20),
  arf_multi_10000_4_04= impute_arf(df_10000_4_04, m=20, parallel = parallel),
  missranger_multi_10000_4_04= impute_missRanger(df_10000_4_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_4_04= impute_missRanger(df_10000_4_04, pmm.k = 5, m=20, num.threads = k),

  ###p=10
  ##df_10000_10_01
  #single
  median_sing_10000_10_01 = impute_median(df_10000_10_01, m=1),
  random_sing_10000_10_01 = impute_random(df_10000_10_01, m=1),
  mice_pmm_sing_10000_10_01 = impute_mice(df_10000_10_01, m=1),
  mice_rf_sing_10000_10_01 = impute_mice(df_10000_10_01, method="rf", m=1),
  arf_sing_10000_10_01 = impute_arf(df_10000_10_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_10_01 = impute_missRanger(df_10000_10_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_10_01 = impute_missRanger(df_10000_10_01, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_10_01 = impute_random(df_10000_10_01, m=20),
  mice_pmm_multi_10000_10_01= impute_mice(df_10000_10_01, m=20),
  mice_rf_multi_10000_10_01= impute_mice(df_10000_10_01, method="rf", m=20),
  arf_multi_10000_10_01= impute_arf(df_10000_10_01, m=20, parallel = parallel),
  missranger_multi_10000_10_01= impute_missRanger(df_10000_10_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_10_01= impute_missRanger(df_10000_10_01, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_10_02
  #single
  median_sing_10000_10_02 = impute_median(df_10000_10_02, m=1),
  random_sing_10000_10_02 = impute_random(df_10000_10_02, m=1),
  mice_pmm_sing_10000_10_02 = impute_mice(df_10000_10_02, m=1),
  mice_rf_sing_10000_10_02 = impute_mice(df_10000_10_02, method="rf", m=1),
  arf_sing_10000_10_02 = impute_arf(df_10000_10_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_10_02 = impute_missRanger(df_10000_10_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_10_02 = impute_missRanger(df_10000_10_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_10_02 = impute_random(df_10000_10_02, m=20),
  mice_pmm_multi_10000_10_02= impute_mice(df_10000_10_02, m=20),
  mice_rf_multi_10000_10_02= impute_mice(df_10000_10_02, method="rf", m=20),
  arf_multi_10000_10_02= impute_arf(df_10000_10_02, m=20, parallel = parallel),
  missranger_multi_10000_10_02= impute_missRanger(df_10000_10_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_10_02= impute_missRanger(df_10000_10_02, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_10_04
  #single
  median_sing_10000_10_04 = impute_median(df_10000_10_04, m=1),
  random_sing_10000_10_04 = impute_random(df_10000_10_04, m=1),
  mice_pmm_sing_10000_10_04 = impute_mice(df_10000_10_04, m=1),
  mice_rf_sing_10000_10_04 = impute_mice(df_10000_10_04, method="rf", m=1),
  arf_sing_10000_10_04 = impute_arf(df_10000_10_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_10_04 = impute_missRanger(df_10000_10_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_10_04 = impute_missRanger(df_10000_10_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_10_04 = impute_random(df_10000_10_04, m=20),
  mice_pmm_multi_10000_10_04= impute_mice(df_10000_10_04, m=20),
  mice_rf_multi_10000_10_04= impute_mice(df_10000_10_04, method="rf", m=20),
  arf_multi_10000_10_04= impute_arf(df_10000_10_04, m=20, parallel = parallel),
  missranger_multi_10000_10_04= impute_missRanger(df_10000_10_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_10_04= impute_missRanger(df_10000_10_04, pmm.k = 5, m=20, num.threads = k),

  ###p=20
  ##df_10000_20_01
  #single
  median_sing_10000_20_01 = impute_median(df_10000_20_01, m=1),
  random_sing_10000_20_01 = impute_random(df_10000_20_01, m=1),
  mice_pmm_sing_10000_20_01 = impute_mice(df_10000_20_01, m=1),
  mice_rf_sing_10000_20_01 = impute_mice(df_10000_20_01, method="rf", m=1),
  arf_sing_10000_20_01 = impute_arf(df_10000_20_01, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_20_01 = impute_missRanger(df_10000_20_01, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_20_01 = impute_missRanger(df_10000_20_01, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_20_01 = impute_random(df_10000_20_01, m=20),
  mice_pmm_multi_10000_20_01= impute_mice(df_10000_20_01, m=20),
  mice_rf_multi_10000_20_01= impute_mice(df_10000_20_01, method="rf", m=20),
  arf_multi_10000_20_01= impute_arf(df_10000_20_01, m=20, parallel = parallel),
  missranger_multi_10000_20_01= impute_missRanger(df_10000_20_01, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_20_01= impute_missRanger(df_10000_20_01, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_20_02
  #single
  median_sing_10000_20_02 = impute_median(df_10000_20_02, m=1),
  random_sing_10000_20_02 = impute_random(df_10000_20_02, m=1),
  mice_pmm_sing_10000_20_02 = impute_mice(df_10000_20_02, m=1),
  mice_rf_sing_10000_20_02 = impute_mice(df_10000_20_02, method="rf", m=1),
  arf_sing_10000_20_02 = impute_arf(df_10000_20_02, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_20_02 = impute_missRanger(df_10000_20_02, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_20_02 = impute_missRanger(df_10000_20_02, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_20_02 = impute_random(df_10000_20_02, m=20),
  mice_pmm_multi_10000_20_02= impute_mice(df_10000_20_02, m=20),
  mice_rf_multi_10000_20_02= impute_mice(df_10000_20_02, method="rf", m=20),
  arf_multi_10000_20_02= impute_arf(df_10000_20_02, m=20, parallel = parallel),
  missranger_multi_10000_20_02= impute_missRanger(df_10000_20_02, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_20_02= impute_missRanger(df_10000_20_02, pmm.k = 5, m=20, num.threads = k),

  ##df_10000_20_04
  #single
  median_sing_10000_20_04 = impute_median(df_10000_20_04, m=1),
  random_sing_10000_20_04 = impute_random(df_10000_20_04, m=1),
  mice_pmm_sing_10000_20_04 = impute_mice(df_10000_20_04, m=1),
  mice_rf_sing_10000_20_04 = impute_mice(df_10000_20_04, method="rf", m=1),
  arf_sing_10000_20_04 = impute_arf(df_10000_20_04, expectation=TRUE, m=1, parallel = parallel),
  missranger_sing_10000_20_04 = impute_missRanger(df_10000_20_04, pmm.k = 1, m=1, num.threads = k),
  missranger_pmm_sing_10000_20_04 = impute_missRanger(df_10000_20_04, pmm.k = 5, m=1, num.threads = k),

  #multiple
  random_multi_10000_20_04 = impute_random(df_10000_20_04, m=20),
  mice_pmm_multi_10000_20_04= impute_mice(df_10000_20_04, m=20),
  mice_rf_multi_10000_20_04= impute_mice(df_10000_20_04, method="rf", m=20),
  arf_multi_10000_20_04= impute_arf(df_10000_20_04, m=20, parallel = parallel),
  missranger_multi_10000_20_04= impute_missRanger(df_10000_20_04, pmm.k = 1, m=20, num.threads = k),
  missranger_pmm_multi_10000_20_04= impute_missRanger(df_10000_20_04, pmm.k = 5, m=20, num.threads = k),

  times = 10)

runtime_k16

#save runtime
saveRDS(runtime_k16, file = "microbenchmark_results_k16.rds")
save(runtime_k16,file="runtime_k16.Rda")

#saveRDS(runtime_k16, file= file.path(path, "microbenchmark_results_k16.rds"))
