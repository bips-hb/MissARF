# Plots of real data example for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_nrmse <- readRDS(file.path(path, paste0("diabetes_nrmse", ".rds")))
res<- readRDS(file.path(path, paste0("diabetes_coverage", ".rds")))

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

# Rename Methods
res_nrmse[, Method := factor(Method, 
                       levels = c("arf_1_expct_local_10_100", "mice_rf_1", "mice_pmm_1", "missRanger_1_5", "missRanger_1_0", 
                                  "median", "random_1"), 
                       labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                  "Median Imp.", "Random Imp."))]

# Evaluate Coverage ----------------------------------------------------------------
res[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
# Rename
res[, Method := factor(Method, 
                       levels = c("arf_local_10_100", "mice_rf", "mice_pmm","missRanger_5", "missRanger_0", 
                                  "median", "random"), 
                       labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                  "Median Imp.", "Random Imp."))]

eva <- res[, .(raw_bias = mean(estimate - truth), 
               percent_bias = 100 * abs(mean((estimate - truth) / truth, na.rm = TRUE)),
               relative_bias = mean((estimate - truth) / abs(truth), na.rm = TRUE),
               relative_bias_median = median((estimate - truth) / abs(truth), na.rm = TRUE),
               coverage_rate = mean(lower < truth & truth < upper, na.rm = TRUE),
               average_width = mean(upper - lower, na.rm = TRUE), 
               rmse = sqrt(mean((estimate - truth)^2, na.rm = TRUE))), 
           by = .(Method, n, prop_mis, pattern, term)] 
eva <- unique(eva)


# Plot NRMSE --------------------------------------------------------------------

# Color Methods
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "#DD7907", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "white", "Median Imp." = "white", "Random Imp." = "white")

outline_colors <- c("MissARF" = "blue3", "MissForest" = "orange4", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "black", "Median Imp." = "black", "Random Imp." = "black")

# Plot function for NRMSE 
plot_function <- function(res, bestMethod, ylab=NULL, propMis = 0.4, xlab=NULL, patternLabel=FALSE, ticks=TRUE,limit, title=FALSE){
  # Filter the data for the "best method" and calculate the median NRMSE for each pattern
  medians <- res %>%
    filter(Method == bestMethod, prop_mis==propMis) %>%
    group_by(pattern) %>%
    summarize(median_nrmse = median(nrmse))
  
  if(bestMethod == "MissARF"){
    col<-"blue3"
  }else{
    col<-"orange4"
  }
  data <- res %>%
    filter(prop_mis == propMis)
  p<- ggplot(data, aes(x = Method, y = nrmse, fill = Method, color = Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = 0.1)+  
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
    coord_flip() + 
    scale_fill_manual(values = c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = "none", color = "none") + 
    ylab(ylab) + 
    xlab(xlab) + 
    scale_y_continuous(limits = limit) + 
    geom_hline(data = medians, aes(yintercept = median_nrmse), linetype = "dashed", color = col)
  if(title){
    p<-p+ggtitle("a) NRMSE - Diabetes dataset")
  }
  if(!patternLabel){
    p<-p+theme(strip.text.x = element_blank()) 
  }
  if(ticks){
    p<-p+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  return(p)
}

p4 <- plot_function(res_nrmse, bestMethod = "MissForest", , propMis = 0.1, patternLabel=TRUE,limit=c(0.5, 2.5), title = TRUE)
p10 <- plot_function(res_nrmse, bestMethod = "MissForest", , propMis = 0.2, limit=c(0.5, 2.5)) 
p20 <- plot_function(res_nrmse, bestMethod = "MissForest", ylab="Data NRMSE", ticks = FALSE,limit=c(0.5, 2.5))

# Combine the plots
p_nrmse <- wrap_plots(p4, p10, p20, ncol = 1)

# Plot Coverage --------------------------------------------------------------------
# Color methods
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


outline_colors <- c("MissARF" = "blue3", "MissForest" = "black", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "darkgreen", "Random Imp." = "black")

# Plot coverage
plot_cov <- lapply(res[, unique(n)], function(nm) {
ggplot(eva[n == nm, ], aes(x = Method, y = coverage_rate, fill = Method, color=Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
    geom_boxplot(outlier.size = .1) +
    geom_hline(yintercept = 0.95, color = "red") +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+   
    ggtitle("b) Coverage rate - Diabetes dataset")+
    coord_flip() + 
    ylab("Coverage rate")+
    xlab(NULL)+
    scale_fill_manual(values =c(method_colors)) +
    scale_colour_manual(values = c(outline_colors)) +
    guides(fill = FALSE, color=FALSE) 
})
names(plot_cov) <- res[, unique(n)]
p_cov <- wrap_plots(plot_cov, ncol = 1)

# Combine and save the plots
bb <- p_nrmse | p_cov

ggsave("plot_real.pdf", plot = bb, width = 210, height = 86.66, units = "mm", scale = 1.5)

