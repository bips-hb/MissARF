# Plots of simulation study for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))
#res_coverage_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_min_node_size", ".rds")))
res_coverage_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_num_trees_node_size_20", ".rds")))

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


# Evaluate Coverage different node sizes ----------------------------------------------------------------
res_coverage_node_sizes[, Method := factor(paste0(algorithm,
                                                  ifelse(is.na(method), "", paste0("_", method)),
                                                  ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                                                  ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                                                  ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                                                  ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_coverage_node_sizes[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

# Rename methods
res_coverage_node_sizes[, Method := factor(Method, 
                                           levels = c("arf_local_20_100",  "arf_local_20_325",  "arf_local_20_550",  "arf_local_20_775", "arf_local_20_1000",
                                           "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", "random"), 
                                           labels = c("MissARF_100", "MissARF_325", "MissARF_550", "MissARF_775", "MissARF_1000",   
                                                      "MICE RF", "MICE PMM","MissForest PMM", "MissForest", "Random Imp."))]

eva_node_sizes <- res_coverage_node_sizes[, .(raw_bias = mean(estimate - truth), 
                                              percent_bias = 100 * abs(mean((estimate - truth) / truth)),
                                              relative_bias = mean((estimate - truth) / abs(truth)),
                                              relative_bias_median = median((estimate - truth) / abs(truth)),
                                              coverage_rate = mean(lower < truth & truth < upper),
                                              average_width = mean(upper - lower), 
                                              rmse = sqrt(mean((estimate - truth)^2))), 
                                          by = .(Method, n, p, prop_mis, pattern, dist, effect, term)] 
eva_node_sizes <- unique(eva_node_sizes)

# Plot Coverage ----------------------------------------------------------------
# Color methods
method_colors <- c("MissARF_100"= "white", "MissARF_325"= "white", "MissARF_550"= "white", "MissARF_775"= "white", "MissARF_1000"= "white", 
                   "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


outline_colors <- c("MissARF_100"= "black", "MissARF_325"= "black", "MissARF_550"= "black", "MissARF_775"= "black", "MissARF_1000"= "black",
                    "MissForest" = "grey", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "darkgreen", "Random Imp." = "grey")

# Plot function for coverage rate
plot_cov <- function(nm=1000, pm=4, dm="normal", em="linear",  patternm = "MAR" , prop_mism=0.4, subcap="a)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = coverage_rate, y = Method , fill = Method, color=Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = .1) +
    geom_vline(xintercept = 0.95, color = "red")+
    #geom_hline(yintercept = 0.95, color = "red") +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
    coord_flip() + 
    xlab(NULL)+
    scale_fill_manual(values =c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = "none", color="none") +
    ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm))+ #ggtitle(paste(subcap,"n =", nm, ", p =", pm, ", dist =", dm, ", effect =", em))+
    ylab("Coverage rate for different number of trees for MissARF")
}

#p_cov_orig <- plot_cov(evam=eva)
p_cov <- plot_cov(evam=eva_node_sizes, subcap="b)")

# Plot Average Width ----------------------------------------------------------------
plot_aw <- function(nm=1000, pm=4, dm="normal", em="linear",  patternm = "MAR" , prop_mism=0.4, subcap="c)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = average_width, y =Method, fill = Method, color=Method)) +
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
    ylab("Average CI width for different number of trees for MissARF")
}

#p_aw_20 <- plot_aw(evam=eva)
p_aw<- plot_aw(evam=eva_node_sizes, subcap="d)")

# Plot rmse ----------------------------------------------------------------
plot_rmse <- function(nm=1000, pm=4, dm="normal", em="linear", patternm = "MAR" , prop_mism=0.4, subcap="e)", evam=eva){
  ggplot(evam[n == nm & p == pm & dist == dm & effect == em & pattern == patternm & prop_mis== prop_mism, ], aes(x = rmse, y =  Method , fill = Method, color=Method)) +
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
    ylab("Coefficient RMSE for different number of treesfor MissARF")
}

#p_rmse_20 <- plot_rmse(evam=eva)
p_rmse <- plot_rmse(evam=eva_node_sizes, subcap="f)")

# Plot combined and saved ----------------------------------------------------------------
#bb <- (p_cov_orig | p_cov) / (p_aw_20 | p_aw) / (p_rmse_20  | p_rmse)
#bb <-  p_cov / p_aw / p_rmse
bb <- (plot_spacer() | p_cov) / (plot_spacer() | p_aw) / (plot_spacer()  | p_rmse)



ggsave("plot_num_trees.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 

#median value of "MissARF_3"
median(eva_node_sizes[Method == "MissARF_17" & n == 1000 & p == 4 & dist == "normal" & effect == "linear" & pattern == "MAR" & prop_mis== 0.4, coverage_rate])
