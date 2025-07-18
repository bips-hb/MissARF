# Plots of simulation study for the paper

library(data.table)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(dplyr)
source("setup.R")

# Get results -------------------------------------------------------------
res_nrmse <- readRDS(file.path(path, paste0("logreg_nrmse", ".rds")))
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
                             levels = c("arf_1_expct_local_10_100", "mice_rf_1", "mice_pmm_1", "missRanger_1_5", "missRanger_1_0", 
                                        "median", "random_1"), 
                             labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                        "Median Imp.", "Random Imp."))]

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
                            levels = c("arf_1_expct_local_10_100", "mice_rf_1", "mice_pmm_1", "missRanger_1_5", "missRanger_1_0", 
                                       "median", "random_1"), 
                            labels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                       "Median Imp.", "Random Imp."))]

# Plot NRMSE ----------------------------------------------------------------
# Color Methods
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "#DD7907", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "white", "Median Imp." = "white", "Random Imp." = "white")

outline_colors <- c("MissARF" = "blue3", "MissForest" = "orange4", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "black", "Median Imp." = "black", "Random Imp." = "black")

# Plotting function
plot_function <- function(res, nm, pm, dm, em, bestMethod, ylab=NULL, propMis = 0.4, xlab=NULL, patternLabel=FALSE, ticks=TRUE,limit, title=FALSE, subcap="a)"){
  # Filter the data for the "best method" and calculate the median NRMSE for each pattern
  medians <- res %>%
    filter(Method == bestMethod, n == nm, p == pm, dist == dm, effect == em, prop_mis==propMis) %>%
    group_by(pattern) %>%
    summarize(median_nrmse = median(nrmse))
  
  # Create the overall title
  overall_title <-  paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", mis.=", propMis )
  
  # Color hline in plot                       
  if(bestMethod == "MissARF"){
    col<-"blue3"
  }else{
    col<-"orange4"
  }
  
  # Filter the data for the given parameters
  data <- res %>%
    filter(n == nm, p == pm, dist == dm, effect == em, prop_mis == propMis)
  
  # Create the plot
  p<- ggplot(data, aes(x = Method, y = nrmse, fill = Method, color = Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.4'=paste("p =",pm))))+ 
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
    p<-p+ggtitle(overall_title)
  }
  if(!patternLabel){
    p<-p+theme(strip.text.x = element_blank()) 
  }
  if(ticks){
    p<-p+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  return(p)
}

#normal linear 500----------------------------------------------------------------
nm <- 500 
dm <- "normal" 
em<- "linear"

# Generate the individual plots
p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.8, 1.75), title = TRUE, subcap="b)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.8, 1.75)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Data NRMSE", ticks = FALSE,limit=c(0.8, 1.75))

# Combine the plots
p_nrmse_norm_500 <- wrap_plots(p4, p10, p20, ncol = 1)

#normal linear 1000 ------------------------------------------------------------
nm <- 1000 

p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.8, 1.75), title = TRUE, subcap="a)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.8, 1.75)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Data NRMSE", ticks = FALSE,limit=c(0.8, 1.75))

p_nrmse_norm_1000 <- wrap_plots(p4, p10, p20, ncol = 1)


#binary squared 1000----------------------------------------------------------------
dm <- "binary"
em<- "squared"
# Generate the individual plots
p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.9, 2.0), title = TRUE, subcap="c)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissARF",limit=c(0.9, 2.0)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissARF", ylab="Data NRMSE", ticks = FALSE,limit=c(0.9, 2.0))

# Combine the plots 
p_nrmse_binary_1000 <- wrap_plots(p4, p10, p20, ncol = 1)

#ninary squared 500----------------------------------------------------------------
nm <- 500

p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.9, 2.0), title = TRUE, subcap="d)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.9, 2.0)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Data NRMSE", ticks = FALSE,limit=c(0.9, 2.0))

# Combine the plots 
p_nrmse_binary_500 <- wrap_plots(p4, p10, p20, ncol = 1)


# Plot Brier Score ----------------------------------------------------------------
# Plotting function
plot_pred <- function(res, nm, pm, dm, em, bestMethod, ylab=NULL, propMis = 0.4, xlab=NULL, patternLabel=FALSE, ticks=TRUE,limit, title=FALSE, subcap="e)"){
  # Filter the data for the "best method" and calculate the median perf for each pattern
  medians <- res %>%
    filter(Method == bestMethod, n == nm, p == pm, dist == dm, effect == em, prop_mis==propMis) %>%
    group_by(pattern) %>%
    summarize(median_perf = median(perf))
  
  # Create the overall title
  overall_title <-  paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", mis.=", propMis )
  
  # Color hline in plot                       
  if(bestMethod == "MissARF"){
    col<-"blue3"
  }else{
    col<-"orange4"
  }
  
  # Filter the data for the given parameters
  data <- res %>%
    filter(n == nm, p == pm, dist == dm, effect == em, prop_mis == propMis)
  
  # Create the plot
  p<- ggplot(data, aes(x = Method, y = perf, fill = Method, color = Method)) +
    facet_grid(prop_mis ~ pattern, labeller = labeller(prop_mis =c('0.4'=paste("p =",pm))))+ 
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
    geom_hline(data = medians, aes(yintercept = median_perf), linetype = "dashed", color = col)
  if(title){
    p<-p+ggtitle(overall_title)
  }
  if(!patternLabel){
    p<-p+theme(strip.text.x = element_blank()) 
  }
  if(ticks){
    p<-p+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  return(p)
}

#normal linear
nm <- 1000
dm <- "normal"
em<- "linear"

p4 <- plot_pred(res_pred,nm, pm = 4, dm, em, bestMethod = "MissForest", patternLabel=TRUE,limit=c(0.13, 0.3), title = TRUE, subcap="e)")
p10 <- plot_pred(res_pred,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.13, 0.3)) 
p20 <- plot_pred(res_pred,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Brier Score", ticks = FALSE,limit=c(0.13, 0.3))

pp_brier_normal <- wrap_plots(p4, p10, p20, ncol = 1)
pp_brier_normal

#binary squared
dm <- "binary"
em<- "squared"

p4 <- plot_pred(res_pred,nm, pm = 4, dm, em, bestMethod = "MissForest", patternLabel=TRUE,limit=c(0.18, 0.35), title = TRUE, subcap="f)")
p10 <- plot_pred(res_pred,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.18, 0.35)) 
p20 <- plot_pred(res_pred,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Brier Score", ticks = FALSE,limit=c(0.18, 0.35))

pp_brier_binary <- wrap_plots(p4, p10, p20, ncol = 1)
pp_brier_binary

# Plot combined and saved ----------------------------------------------------------------
#bb <- (p_nrmse_norm_500 | p_nrmse_binary_500) / (p_nrmse_norm_1000 | p_nrmse_binary_1000) / (pp_brier_normal | pp_brier_binary)
bb<- (p_nrmse_norm_1000 |p_nrmse_norm_500 ) / (p_nrmse_binary_1000 | p_nrmse_binary_500) / (pp_brier_normal | pp_brier_binary)

ggsave("plot_paper_nrmse_brier.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 
