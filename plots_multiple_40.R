# Plots of simulation study for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))
res_coverage_40 <- readRDS(file.path(path, paste0("logreg_coverage_m_40", ".rds")))

# Evaluate Coverage m=20 ----------------------------------------------------------------
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
                                levels = c("arf_local_10_100", "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", 
                                           "random"), 
                                labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                           "Random Imp."))]

eva <- res_coverage[, .(raw_bias = mean(estimate - truth), 
                        percent_bias = 100 * abs(mean((estimate - truth) / truth)),
                        relative_bias = mean((estimate - truth) / abs(truth)),
                        relative_bias_median = median((estimate - truth) / abs(truth)),
                        coverage_rate = mean(lower < truth & truth < upper),
                        average_width = mean(upper - lower), 
                        rmse = sqrt(mean((estimate - truth)^2))), 
                    by = .(Method, n, p, prop_mis, pattern, dist, effect, term)] 
eva <- unique(eva)


# Evaluate Coverage m=40 ----------------------------------------------------------------
res_coverage_40[, Method := factor(paste0(algorithm,
                                       ifelse(is.na(method), "", paste0("_", method)),
                                       ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                                       ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                                       ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                                       ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_coverage_40[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

# Rename methods
res_coverage_40[, Method := factor(Method, 
                                levels = c("arf_local_10_100", "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", 
                                           "random"), 
                                labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                           "Random Imp."))]

eva_40 <- res_coverage_40[, .(raw_bias = mean(estimate - truth), 
                        percent_bias = 100 * abs(mean((estimate - truth) / truth)),
                        relative_bias = mean((estimate - truth) / abs(truth)),
                        relative_bias_median = median((estimate - truth) / abs(truth)),
                        coverage_rate = mean(lower < truth & truth < upper),
                        average_width = mean(upper - lower), 
                        rmse = sqrt(mean((estimate - truth)^2))), 
                    by = .(Method, n, p, prop_mis, pattern, dist, effect, term)] 
eva_40 <- unique(eva_40)

# Plot Coverage ----------------------------------------------------------------
# Color methods
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


outline_colors <- c("MissARF" = "blue3", "MissForest" = "grey", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "darkgreen", "Random Imp." = "grey")

# Plot function for coverage rate
plot_cov <- function(nm=1000, pm=4, dm="normal", em="linear", mm=20,  patternm = "MAR" , prop_mism=0.4, subcap="a)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = Method, y = coverage_rate, fill = Method, color=Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = .1) +
    geom_hline(yintercept = 0.95, color = "red") +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
    coord_flip() + 
    xlab(NULL)+
    scale_fill_manual(values =c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = "none", color="none") +
    ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm))+ #ggtitle(paste(subcap,"n =", nm, ", p =", pm, ", dist =", dm, ", effect =", em))+
    ylab(paste("Coverage rate for m=", mm))
}

p_cov_20 <- plot_cov( mm=20, evam=eva)
p_cov_40 <- plot_cov(mm=40, evam=eva_40, subcap="b)")


# Plot Average Width ----------------------------------------------------------------
plot_aw <- function(nm=1000, pm=4, dm="normal", em="linear", mm=40,  patternm = "MAR" , prop_mism=0.4, subcap="c)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = Method, y = average_width, fill = Method, color=Method)) +
    facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = .1) +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+  
    coord_flip() + 
    xlab(NULL)+
    scale_fill_manual(values =c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = "none", color="none") +
    ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))+
    ylab(paste("Average CI width for m=", mm))
}

p_aw_20 <- plot_aw(mm=20, evam=eva)
p_aw_40 <- plot_aw(mm=40, evam=eva_40, subcap="d)")

# Plot rmse ----------------------------------------------------------------
plot_rmse <- function(nm=1000, pm=4, dm="normal", em="linear", mm=40, patternm = "MAR" , prop_mism=0.4, subcap="e)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = Method, y = rmse, fill = Method, color=Method)) +
    facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = .1) +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+  
    coord_flip() + 
    xlab(NULL)+
    scale_fill_manual(values =c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = "none", color="none") +
    ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm))+
    ylab(paste("Coefficient RMSE for m=", mm))
}

p_rmse_20 <- plot_rmse(mm=20, evam=eva)
p_rmse_40 <- plot_rmse(mm=40, evam=eva_40, subcap="f)")

# Plot combined and saved ----------------------------------------------------------------
bb <- (p_cov_20 | p_cov_40) / (p_aw_20 | p_aw_40) / (p_rmse_20  | p_rmse_40)

ggsave("plot_multiple_40.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 

