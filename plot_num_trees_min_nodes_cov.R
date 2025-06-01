# Plots of simulation study for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_cov_node_sizes <- readRDS(file.path(path, paste0("logreg_coverage_num_trees_10_160_node_sizes_10_20", ".rds"))) 

# Evaluate Coverage ----------------------------------------------------------------
res_cov_node_sizes[, Method := factor(paste0(algorithm,
                                       ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size)))
))]
res_cov_node_sizes[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

eva_node_sizes <- res_cov_node_sizes[, .(raw_bias = mean(estimate - truth), 
                        percent_bias = 100 * abs(mean((estimate - truth) / truth)),
                        relative_bias = mean((estimate - truth) / abs(truth)),
                        relative_bias_median = median((estimate - truth) / abs(truth)),
                        coverage_rate = mean(lower < truth & truth < upper),
                        average_width = mean(upper - lower), 
                        rmse = sqrt(mean((estimate - truth)^2))), 
                    by = .(Method, num_trees, n, p, prop_mis, pattern, dist, effect, term)] 
eva_node_sizes <- unique(eva_node_sizes)

table(eva_node_sizes$Method)
table(eva_node_sizes$num_trees)



# Evaluate Cov for different number of trees ----------------------------------------------------------------

cov <- ggplot(eva_node_sizes, aes(x=num_trees,y=coverage_rate, colour=Method)) +
  stat_smooth(method="loess", span= 0.3, se=TRUE, alpha=0.3) +
  scale_x_continuous(breaks = c(seq(10,160,by=30)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
  #  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("a) ")+ #dist=normal, effect = linear, n=1000, p=4, pattern = MAR, mis.= 0.4")+ #dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("Coverage")+
  xlab("Number of trees")# of MissARF")

aw <- ggplot(eva_node_sizes, aes(x=num_trees,y=average_width, colour=Method)) +
  stat_smooth(method="loess", span= 0.3, se=TRUE, alpha=0.3) +
  scale_x_continuous(breaks = c(seq(10,160,by=30)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
  #  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("b) ")+ # dist=normal, effect = linear, n=1000, p=4, pattern = MAR, mis.= 0.4")+ 
  ylab("Average CI width")+
  xlab("Number of trees")

rmse <- ggplot(eva_node_sizes, aes(x=num_trees,y=rmse, colour=Method)) +
  stat_smooth(method="loess", span= 0.3, se=TRUE, alpha=0.3) +
  scale_x_continuous(breaks = c(seq(10,160,by=30)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
  #  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("c) ")+ # dist=normal, effect = linear, n=1000, p=4, pattern = MAR, mis.= 0.4")+ 
  ylab("Coef. RMSE")+
  xlab("Number of trees")

bb <- (cov | aw)/(rmse|plot_spacer()) 

ggsave("supplement_plots/plot_cov_num_trees.pdf", plot = bb, width = 210, height = 173.33, units = "mm", scale = 1.5)
