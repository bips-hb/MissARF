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
res_coverage <- readRDS(file.path(path, paste0("logreg_coverage", ".rds")))

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

#normal ----------------------------------------------------------------
nm <- 10000 
dm <- "normal" 
em<- "linear"

# Generate the individual plots
p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.8, 1.75), title = TRUE, subcap="a)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissForest",limit=c(0.8, 1.75)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissForest", ylab="Data NRMSE", ticks = FALSE,limit=c(0.8, 1.75))

# Combine the plots
p_nrmse_norm <- wrap_plots(p4, p10, p20, ncol = 1)

#binary ----------------------------------------------------------------
dm <- "binary"
# Generate the individual plots
p4 <- plot_function(res_nrmse,nm, pm = 4, dm, em, bestMethod = "MissARF", patternLabel=TRUE,limit=c(0.9, 2.0), title = TRUE, subcap="b)")
p10 <- plot_function(res_nrmse,nm, pm = 10, dm, em, bestMethod = "MissARF",limit=c(0.9, 2.0)) 
p20 <- plot_function(res_nrmse,nm, pm = 20, dm, em, bestMethod = "MissARF", ylab="Data NRMSE", ticks = FALSE,limit=c(0.9, 2.0))

# Combine the plots 
p_nrmse_bin <- wrap_plots(p4, p10, p20, ncol = 1)

# Plot Brier Score ----------------------------------------------------------------
# Plotting function
plot_pred <- function(res, nm, pm, dm, em, bestMethod, ylab=NULL, propMis = 0.4, xlab=NULL, patternLabel=FALSE, ticks=TRUE , title = FALSE, subcap="c)"){
  # Filter the data for "best method" and calculate the median perf for each pattern
  medians <- res %>%
    filter(Method == bestMethod, n == nm, p == pm, dist == dm, effect == em, prop_mis==propMis) %>%
    group_by(pattern) %>%
    summarize(median_perf = median(perf))
  
  overall_title <- paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ) 
  
  if(bestMethod == "MissARF"){
    col<-"blue3"
  }else{
    col<-"orange4"
  }
  data <- res %>%
    filter(n == nm, p == pm, dist == dm, effect == em, prop_mis == propMis)
  p<- ggplot(data, aes(x = Method, y = perf, fill = Method, color = Method)) +
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
    geom_hline(data = medians, aes(yintercept = median_perf), linetype = "dashed", color = col)+
    scale_y_continuous(limits = c(0.07, 0.3)) 
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

nm <- 1000 
dm <- "gamma"
em<- "squared"

# Generate the individual plots
p4 <- plot_pred(res_pred,nm, pm = 4, dm, em,propMis = 0.1, bestMethod = "MissForest", patternLabel=TRUE, title = TRUE)
p10 <- plot_pred(res_pred,nm, pm = 4, dm, em, propMis = 0.2,bestMethod = "MissForest")
p20 <- plot_pred(res_pred,nm, pm = 4, dm, em,propMis = 0.4, bestMethod = "MissARF", ylab="Brier Score",ticks=FALSE)

pp_brier <- wrap_plots(p4, p10, p20, ncol = 1)

# Plot Coverage ----------------------------------------------------------------
# Color methods
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


outline_colors <- c("MissARF" = "blue3", "MissForest" = "grey", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "darkgreen", "Random Imp." = "grey")

# Plot function for coverage rate
plot_cov <- function(nm, pm, dm, em, subcap="d)"){
  ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = coverage_rate, fill = Method, color=Method)) +
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
    ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))+ #ggtitle(paste(subcap,"n =", nm, ", p =", pm, ", dist =", dm, ", effect =", em))+
    ylab("Coverage rate")
}

nm <- 10000 
pm <- 10 
dm <- "normal" 
em <- "linear"

p_cov_1 <- plot_cov(nm, pm, dm, em)

em <- "squared"
p_cov_2 <- plot_cov(nm, pm, dm, em, "f)")

# Plot Average Width ----------------------------------------------------------------
plot_aw <- function(nm, pm, dm, em, subcap="e)"){
  ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = average_width, fill = Method, color=Method)) +
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
    ylab("Average CI width")
}

em <- "linear"
p_aw <- plot_aw(nm, pm, dm, em)


# Plot combined and saved ----------------------------------------------------------------
bb <- (p_nrmse_norm | p_nrmse_bin) / (pp_brier | p_cov_1) / (p_aw  | p_cov_2)

ggsave("plot_sim_all.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 

