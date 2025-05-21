# Plots of simulation study for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))
#res_coverage_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_min_node_size", ".rds"))) #logreg_coverage_min_node_size_2_30
res_coverage_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_min_node_size_100_500", ".rds")))

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
 #levels = c("arf_local_02_100", "arf_local_13_100",  "arf_local_24_100",  "arf_local_35_100",  "arf_local_46_100",  "arf_local_56_100",  "arf_local_67_100",  "arf_local_78_100",  "arf_local_89_100" , "arf_local_100_100", 
#            "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", "random"), 
 #levels = c("arf_local_02_100", "arf_local_03_100", "arf_local_04_100", "arf_local_05_100", "arf_local_06_100", "arf_local_08_100", "arf_local_09_100", "arf_local_11_100", "arf_local_14_100", "arf_local_17_100", "arf_local_20_100", "arf_local_25_100", "arf_local_30_100",
levels=c("arf_local_100_100", "arf_local_144_100", "arf_local_189_100", "arf_local_233_100", "arf_local_278_100", "arf_local_322_100", "arf_local_367_100", "arf_local_411_100", "arf_local_456_100", "arf_local_500_100",   
          "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", "random"), 
 #labels = #c("MissARF_2", "MissARF_13", "MissARF_24", "MissARF_35", "MissARF_46", "MissARF_56", "MissARF_67","MissARF_78", "MissARF_89", "MissARF_100",    
 #labels = c("MissARF_2", "MissARF_3", "MissARF_4", "MissARF_5", "MissARF_6", "MissARF_8", "MissARF_9","MissARF_11", "MissARF_14", "MissARF_17", "MissARF_20", "MissARF_25", "MissARF_30",
labels = c("MissARF_100", "MissARF_144", "MissARF_189", "MissARF_233", "MissARF_278", "MissARF_322", "MissARF_367", "MissARF_411", "MissARF_456", "MissARF_500",  
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
#method_colors <- c("MissARF_2"= "white", "MissARF_13"= "white", "MissARF_24"= "#4472C4", "MissARF_35"= "white", "MissARF_46"= "white", "MissARF_56"= "white", "MissARF_67"= "white","MissARF_78"= "white", "MissARF_89"= "white", "MissARF_100"= "white",
#method_colors <- c("MissARF_2"= "white", "MissARF_3"= "white", "MissARF_4"= "white", "MissARF_5"= "white", "MissARF_6"= "white", "MissARF_8"= "white", "MissARF_9"= "white","MissARF_11"= "white", "MissARF_14"= "white", "MissARF_17"= "white", "MissARF_20"= "white", "MissARF_25"= "white", "MissARF_30"= "white",  
method_colors <- c("MissARF_100"= "white", "MissARF_144"= "white", "MissARF_189"= "white", "MissARF_233"= "white", "MissARF_278"= "white", "MissARF_322"= "white", "MissARF_367"= "white", "MissARF_411"= "white", "MissARF_456"= "white", "MissARF_500"= "white", 
                  "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


#outline_colors <- c("MissARF_2"= "black", "MissARF_13"= "black", "MissARF_24"= "blue3", "MissARF_35"= "black", "MissARF_46"= "black", "MissARF_56"= "black", "MissARF_67"= "black","MissARF_78"= "black", "MissARF_89"= "black", "MissARF_100"= "black",
#outline_colors <- c("MissARF_2"= "black", "MissARF_3"= "black", "MissARF_4"= "black", "MissARF_5"= "black", "MissARF_6"= "black", "MissARF_8"= "black", "MissARF_9"= "black","MissARF_11"= "black", "MissARF_14"= "black", "MissARF_17"= "black", "MissARF_20"= "black", "MissARF_25"= "black", "MissARF_30"= "black",  
outline_colors <- c("MissARF_100"= "black", "MissARF_144"= "black", "MissARF_189"= "black", "MissARF_233"= "black", "MissARF_278"= "black", "MissARF_322"= "black", "MissARF_367"= "black", "MissARF_411"= "black", "MissARF_456"= "black", "MissARF_500"= "black", 
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
    ylab("Coverage rate for different min node sizes for MissARF")
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
    ylab("Average CI width for different min node sizes for MissARF")
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
    ylab("Coefficient RMSE for different min node sizes for MissARF")
}

#p_rmse_20 <- plot_rmse(evam=eva)
p_rmse <- plot_rmse(evam=eva_node_sizes, subcap="f)")

# Plot combined and saved ----------------------------------------------------------------
#bb <- (p_cov_orig | p_cov) / (p_aw_20 | p_aw) / (p_rmse_20  | p_rmse)
#bb <-  p_cov / p_aw / p_rmse
bb <- (plot_spacer() | p_cov) / (plot_spacer() | p_aw) / (plot_spacer()  | p_rmse)



ggsave("plot_min_node_100_500.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 

#median value of "MissARF_3"
median(eva_node_sizes[Method == "MissARF_17" & n == 1000 & p == 4 & dist == "normal" & effect == "linear" & pattern == "MAR" & prop_mis== 0.4, coverage_rate])
