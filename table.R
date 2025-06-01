
library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
source("setup.R")

# Get results -------------------------------------------------------------
res_nrmse <- readRDS(file.path(path, paste0("logreg_nrmse", ".rds")))
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))
res_pred <- readRDS(file.path(path, paste0("logreg_pred", ".rds")))
res_runtime <- readRDS(file.path(path, paste0("microbenchmark_results_k1", ".rds"))) #"runtime_1thread", ".rds")))

# Evaluate NRMSE ----------------------------------------------------------------
res_nrmse[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(m), "", paste0("_", as.character(m))),
                              ifelse(is.na(expectation), "", ifelse(expectation, "_expct", "")),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_nrmse[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res_nrmse[, nrmse := V1]

# Rename methods
res_nrmse[, Method := factor(Method, 
                       levels = c("arf_1_expct_local_10_100", "missRanger_1_5", "missRanger_1_0", 
                                  "mice_rf_1", "mice_pmm_1", "median", "random_1"), 
                       labels = c("ARF", "MissForest PMM", "MissForest", 
                                  "MICE RF", "MICE PMM", "Median Imp.", "Random Imp."))]

# Evaluate Coverage ----------------------------------------------------------------
res_coverage[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_coverage[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

# Rename methods
res_coverage[, Method := factor(Method, 
                       levels = c("arf_local_10_100", "missRanger_5", "missRanger_0", 
                                  "mice_rf", "mice_pmm", "random"), 
                       labels = c("ARF", "MissForest PMM", "MissForest", 
                                  "MICE RF", "MICE PMM", "Random Imp."))]

eva <- res_coverage[, .(raw_bias = mean(estimate - truth), 
               percent_bias = 100 * abs(mean((estimate - truth) / truth)),
               relative_bias = mean((estimate - truth) / abs(truth)),
               relative_bias_median = median((estimate - truth) / abs(truth)),
               coverage_rate = mean(lower < truth & truth < upper),
               average_width = mean(upper - lower), 
               rmse = sqrt(mean((estimate - truth)^2))), 
           by = .(Method, n, p, prop_mis, pattern, dist, effect, term)] 
eva <- unique(eva)

# Evaluate pred ----------------------------------------------------------------
res_pred[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(m), "", paste0("_", as.character(m))),
                              ifelse(is.na(expectation), "", ifelse(expectation, "_expct", "")),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_pred[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res_pred[, perf := V1]

# Rename methods
res_pred[, Method := factor(Method, 
                       levels = c("arf_1_expct_local_10_100", "missRanger_1_5", "missRanger_1_0", 
                                  "mice_rf_1", "mice_pmm_1", "median", "random_1"), 
                       labels = c("ARF", "MissForest PMM", "MissForest", 
                                  "MICE RF", "MICE PMM", "Median Imp.", "Random Imp."))]

# Runtime -----------------------------------------------------------------
res_runtime <- as.data.table(res_runtime)
res_runtime[, seconds := time/1e9]

# Separate the information in the expr column
res_runtime[grep("mice", expr), expr := gsub("mice_", "mice-", expr)]
res_runtime[grep("missranger_pmm", expr), expr := gsub("missranger_pmm", "missranger-pmm", expr)]
res_runtime <- cbind(res_runtime, res_runtime[, tstrsplit(expr, split = "_", names = c("Method", "Imputation", "n", "p", "prop_mis"))])
res_runtime[, n := as.numeric(n)]
res_runtime[, p := as.numeric(p)]

# Rename methods
res_runtime[, Method := factor(Method, 
                            levels = c("arf", "missranger-pmm", "missranger", 
                                       "mice-rf", "mice-pmm", "median", "random"), 
                            labels = c("ARF", "MissForest PMM", "MissForest", 
                                       "MICE RF", "MICE PMM", "Median Imp.", "Random Imp."))]

# Mean over all settings
tab_runtime_single <- res_runtime[Imputation == "sing", .(runtime_single_mean = mean(seconds, na.rm = TRUE), runtime_single_sd = sd(seconds, na.rm = TRUE)), by = .(Method)]
tab_runtime_multi <- res_runtime[Imputation == "multi", .(runtime_multi_mean = mean(seconds, na.rm = TRUE), runtime_multi_sd = sd(seconds, na.rm = TRUE)), by = .(Method)]


# Table over all settings --------------------------------------------------------------------
tab_nrmse <- res_nrmse[ , .(nrmse_mean = mean(nrmse, na.rm = TRUE), nrmse_sd = sd(nrmse, na.rm = TRUE)), by = .(Method)]
tab_brier <- res_pred[ , .(brier_mean = mean(perf, na.rm = TRUE), brier_sd = sd(perf, na.rm = TRUE)), by = .(Method)]
tab_cvg <- eva[ , .(cvg_median = median(coverage_rate, na.rm = TRUE), cvg_iqr = IQR(coverage_rate, na.rm = TRUE)), by = .(Method)]
tab_aw <- eva[ , .(aw_median = median(average_width, na.rm = TRUE), aw_iqr = IQR(average_width, na.rm = TRUE)), by = .(Method)]
tab_rmse <- eva[ , .(rmse_median = median(rmse, na.rm = TRUE), rmse_iqr = IQR(rmse, na.rm = TRUE)), by = .(Method)]
#tab_rmse_mean <- eva[ , .(rmse_mean = mean(rmse, na.rm = TRUE), rmse_sd = sd(rmse, na.rm = TRUE)), by = .(Method)]
tab_runtime <- merge(tab_runtime_single, tab_runtime_multi, by = "Method", all = TRUE)

tab <- merge(tab_nrmse, tab_brier, by = "Method", all = TRUE)
tab <- merge(tab, tab_cvg, by = "Method", all = TRUE)
tab <- merge(tab, tab_aw, by = "Method", all = TRUE)
tab <- merge(tab, tab_rmse, by = "Method", all = TRUE)
#tab <- merge(tab, tab_rmse_mean, by = "Method", all = TRUE)
tab <- merge(tab, tab_runtime, by = "Method", all = TRUE)

tab[, NRMSE := sprintf("%.2f (%.2f)", nrmse_mean, nrmse_sd)]
tab[, Brier := sprintf("%.2f (%.2f)", brier_mean, brier_sd)]
#tab[, Runtime_single := sprintf("%.1f (%.1f)", runtime_single_mean, runtime_single_sd)]
tab[, Runtime_single := sprintf("%.1f", runtime_single_mean)]
tab[, Coverage := sprintf("%.1f (%.1f)", cvg_median*100, cvg_iqr*100)]
tab[, AvgWidth := sprintf("%.2f (%.2f)", aw_median, aw_iqr)]
#tab[, RMSE_mean := sprintf("%.2f (%.2f)", rmse_mean, rmse_sd)]
tab[, RMSE := sprintf("%.2f (%.2f)", rmse_median, rmse_iqr)]
#tab[, Runtime_multi := sprintf("%.1f (%.1f)", runtime_multi_mean, runtime_multi_sd)]
tab[, Runtime_multi := sprintf("%.1f", runtime_multi_mean)]


# Final latex table
kbl(tab[c(7, 6, 3, 2, 5, 4, 1), .(Method, NRMSE, Brier, Runtime_single, Coverage, AvgWidth, RMSE, Runtime_multi)], booktabs = TRUE, format = "latex")
