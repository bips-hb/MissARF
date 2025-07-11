# Plots of simulation study for the paper
# Plot results of sim_logreg_nrmse_num_trees.R of different number of trees 10-160 and min node sizes 10, 20 & 30 of NRMSE


library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
#source("setup.R")
path <- file.path("/opt/projects/imputation_benchmark/")
# Get results -------------------------------------------------------------
res_nrmse_node_sizes <- readRDS(file.path(path, paste0("logreg_nrmse_num_trees", ".rds"))) 
res_nrmse_node_sizes_20_30 <- readRDS(file.path(path, paste0("logreg_nrmse_num_trees_20_30", ".rds"))) 

d_10 <- res_nrmse_node_sizes[num_trees <190,]

#combine d_10 with res_nrmse_node_sizes_20_30
res_nrmse_node_sizes_20_30 <- rbind(res_nrmse_node_sizes_20_30, d_10)

res_nrmse_node_sizes_20_30[, Method := factor(paste0(algorithm,
                                                     ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size)))
))]
res_nrmse_node_sizes_20_30[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res_nrmse_node_sizes_20_30[, nrmse := V1]

table(res_nrmse_node_sizes_20_30$Method)


# Evaluate NRMSE for different number of trees ----------------------------------------------------------------

res_nrmse_node_sizes[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res_nrmse_node_sizes[, nrmse := V1]

#------------------------------------------------------------------
res_nrmse_node_sizes_20_30[, Method := factor(paste0(algorithm,
                                    #ifelse(is.na(method), "", paste0("_", method)),
                                    #ifelse(is.na(m), "", paste0("_", as.character(m))),
                                    #ifelse(is.na(expectation), "", ifelse(expectation, "_expct", "")),
                                    #ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                                    #ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                                    ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size)))#,
                                    #ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res_nrmse_node_sizes_20_30[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res_nrmse_node_sizes_20_30[, nrmse := V1]

table(res_nrmse_node_sizes_20_30$Method)

# min node size=10,20,30 and number of trees 10-160
bb<-ggplot(res_nrmse_node_sizes_20_30, aes(x=num_trees,y=nrmse, colour=Method)) +
  stat_smooth(method="loess", span= 0.2, se=TRUE, alpha=0.3) +
  scale_x_continuous(breaks = c(seq(10,160,by=30)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
  #  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  #ggtitle("dist=normal, effect = linear, n=1000, p=20, pattern = MAR, mis.= 0.4")+ #dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("Data NRMSE")+
  xlab("Number of trees")# of MissARF")

ggsave("supplement_plots/plot_nrmse_num_trees.pdf", plot = bb, width = 105, height = 86.67, units = "mm", scale = 1.5)


#----------------------------------------------------------------
#min node size 10 over all number of trees 
ggplot(res_nrmse_node_sizes, aes(x=num_trees,y=nrmse)) +
  stat_smooth(method="loess", span=0.2, se=TRUE,  alpha=0.3) +
  scale_x_continuous(breaks = c(0, seq(10,1000,by=30)))+#pretty(eva_node_sizes$min_node_size, n = 25))+
#  scale_y_continuous(breaks = c(seq(0.5, 0.9, by = 0.1), seq(0.91, 1, by = 0.02)))+
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("a) dist=normal, effect = linear, n=1000, p=20")+ #dist = normal, effect = linear, n = 1000, p = 4, pattern = MAR, mis.= 0.4")+ 
  ylab("NRMSE")+
  xlab("Number of trees")# of MissARF")

#--------------------------------------------------------------------------------
#d_30 <- res_nrmse_node_sizes_20_30[Method=="arf_30",]

d_20 <- res_nrmse_node_sizes_20_30[min_node_size==20,]
d_30 <- res_nrmse_node_sizes_20_30[min_node_size==30,]
d_10 <- res_nrmse_node_sizes[num_trees <190,]
#plot individually
ggplot(d_10, aes(x=num_trees,y=nrmse)) +
  stat_smooth(method="loess", span= 0.2, se=TRUE, alpha=0.3) +
  theme_bw(base_size = 14) + 
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("a) dist=normal, effect = linear, n=1000, p=20, pattern = MAR, mis.= 0.4")+ 
  ylab("NRMSE")+
  xlab("Number of trees")




