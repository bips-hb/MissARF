
library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
source("setup.R")

# Get results -------------------------------------------------------------
res_nrmse <- readRDS(file.path(path, paste0("logreg_nrmse", ".rds")))
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))
res_pred <- readRDS(file.path(path, paste0("logreg_pred", ".rds")))

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

# Table over all settings --------------------------------------------------------------------
tab_nrmse <- res_nrmse[ , .(nrmse_mean = mean(nrmse, na.rm = TRUE), nrmse_sd = sd(nrmse, na.rm = TRUE)), by = .(Method)]
tab_brier <- res_pred[ , .(brier_mean = mean(perf, na.rm = TRUE), brier_sd = sd(perf, na.rm = TRUE)), by = .(Method)]
tab_cvg <- eva[ , .(cvg_median = median(coverage_rate, na.rm = TRUE), cvg_iqr = IQR(coverage_rate, na.rm = TRUE)), by = .(Method)]
tab_aw <- eva[ , .(aw_median = median(average_width, na.rm = TRUE), aw_iqr = IQR(average_width, na.rm = TRUE)), by = .(Method)]

tab <- merge(tab_nrmse, tab_brier, by = "Method", all = TRUE)
tab <- merge(tab, tab_cvg, by = "Method", all = TRUE)
tab <- merge(tab, tab_aw, by = "Method", all = TRUE)

tab[, NRMSE := sprintf("%.3f (%.3f)", nrmse_mean, nrmse_sd)]
tab[, Brier := sprintf("%.3f (%.3f)", brier_mean, brier_sd)]
tab[, Coverage := sprintf("%.3f (%.3f)", cvg_median, cvg_iqr)]
tab[, AvgWidth := sprintf("%.3f (%.3f)", aw_median, aw_iqr)]

kbl(tab[c(1, 3, 2, 4:7), .(Method, NRMSE, Brier, Coverage, AvgWidth)], booktabs = TRUE, format = "latex")

# 
res_nrmse_mean <- res_nrmse[,  mean(nrmse, na.rm = TRUE), by = .(Method, pattern, n, p, prop_mis, dist, effect)]
res_nrmse_mean[ , .(nrmse_mean = mean(V1, na.rm = TRUE), nrmse_median = median(V1, na.rm = TRUE)), by = .(Method)][order(nrmse_median), ]


res_nrmse_mean <- res_nrmse[,  mean(nrmse, na.rm = TRUE), by = .(Method, pattern, prop_mis)]
res_nrmse_mean[ , .(nrmse_mean = mean(V1, na.rm = TRUE), nrmse_median = median(V1, na.rm = TRUE)), by = .(Method)][order(nrmse_median), ]
