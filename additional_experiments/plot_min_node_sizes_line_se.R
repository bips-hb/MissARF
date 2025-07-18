# Plots of simulation study for the paper
# Different number of min node sizes for a fixed case
# From simulation skript: sim_logreg_coverage_min_node_sizes.R

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_coverage_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_min_node_size_log_2_400", ".rds"))) 

# Evaluate Coverage different node sizes ----------------------------------------------------------------

res_coverage_node_sizes[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

eva_node_sizes <- res_coverage_node_sizes[, .(raw_bias = mean(estimate - truth), 
                                              percent_bias = 100 * abs(mean((estimate - truth) / truth)),
                                              relative_bias = mean((estimate - truth) / abs(truth)),
                                              relative_bias_median = median((estimate - truth) / abs(truth)),
                                              coverage_rate = mean(lower < truth & truth < upper),
                                              average_width = mean(upper - lower), 
                                              rmse = sqrt(mean((estimate - truth)^2))), 
                                          by = .(algorithm, min_node_size, n, p, prop_mis, pattern, dist, effect, term)] 
                                          
eva_node_sizes <- unique(eva_node_sizes)

p_cov <- ggplot(eva_node_sizes, aes(x=min_node_size,y=coverage_rate)) +
  stat_smooth(method="loess", span=0.1, se=TRUE,  alpha=0.3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = c(seq(0,400,by=20)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("a)")+ #dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("Coverage rate")+
  xlab("Minimum node sizes")# of MissARF")

p_aw <- ggplot(eva_node_sizes, aes(x=min_node_size,y=average_width)) +
  stat_smooth(method="loess", span=0.1, se=TRUE,  alpha=0.3) +
  scale_x_continuous(breaks = c(seq(0,400,by=20)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("b) ")+ #dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("Average CI width")+
  xlab("Minimum node sizes")#ylab(NULL) #xlab("Minimum node sizes of MissARF")

p_rmse <- ggplot(eva_node_sizes, aes(x=min_node_size,y=rmse)) +
  stat_smooth(method="loess", span=0.1, se=TRUE,  alpha=0.3) +
  scale_x_continuous(breaks =c(seq(0,400,by=20)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("c) ") +#dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("Coefficient RMSE")+
  xlab("Minimum node sizes")#xlab(NULL)#("Minimum node sizes of MissARF")

bb <- (p_cov |p_aw) / ( p_rmse |plot_spacer() )

ggsave("supplement_plots/plot_min_node_sizes.pdf", plot = bb, width = 210, height = 173.33, units = "mm", scale = 1.5) 

## checking values ----------------------------------------------------------------
median(eva_node_sizes[min_node_size== 31 & n == 1000 & p == 4 & dist == "normal" & effect == "linear" & pattern == "MAR" & prop_mis== 0.4, coverage_rate])
median(eva_node_sizes[min_node_size== 31 , coverage_rate])

medians <- eva_node_sizes[algorithm == "arf", .(
  median_coverage = median(coverage_rate)
), by = min_node_size]


best_node <- medians[which.max(median_coverage)]$min_node_size

#mean
means <- eva_node_sizes[algorithm == "arf", .(
  mean_coverage = mean(coverage_rate)
), by = min_node_size]

best_node <- means[which.max(mean_coverage)]$min_node_size
best_node

mean(eva_node_sizes[min_node_size== 31 , coverage_rate])
mean(eva_node_sizes[min_node_size== 10 , coverage_rate])

