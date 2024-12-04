# All plots for the supplement of the results for the simulated data

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

# Colour methods nrmse/pred-------------------------------------------------------------------------------
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "#DD7907", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "white", "Median Imp." = "white", "Random Imp." = "white")

outline_colors <- c("MissARF" = "blue3", "MissForest" = "orange4", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "black", "Median Imp." = "black", "Random Imp." = "black")

# Plot Brier Score --------------------------------------------------------------------
plot_pred <- lapply(res_pred[, unique(effect)], function(em) {
  pp <- lapply(res_pred[, unique(dist)], function(dm) {
    lapply(res_pred[, unique(p)], function(pm) {
      nm <- 1000
      if(pm == 4){
        subcap <- "a)"
      }else if(pm == 10){
        subcap <- "c)"
      }else{
        subcap <- "e)"
      }
      p1 <- ggplot(res_pred[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = perf, fill = Method, color=Method)) +
        facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
        geom_boxplot(outlier.size = .1) +
        theme_bw(base_size = 14) + 
        theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
        coord_flip() + 
        scale_fill_manual(values =c(method_colors)) +
        scale_colour_manual(values = c(outline_colors)) +
        guides(fill = "none", color = "none")+ 
        ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))+
        ylab("Brier score") + 
        xlab(NULL)
      
      nm <- 10000
      if(pm == 4){
        subcap <- "b)"
      }else if(pm == 10){
        subcap <- "d)"
      }else{
        subcap <- "f)"
      }
      p2 <- ggplot(res_pred[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = perf, fill = Method, color=Method)) +
        facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
        geom_boxplot(outlier.size = .1) +
        theme_bw(base_size = 14) + 
        theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
        coord_flip() + 
        scale_fill_manual(values =c(method_colors)) +
        scale_colour_manual(values = c(outline_colors)) +
        guides(fill = "none", color = "none")+ 
        ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))+
        ylab("Brier score") + 
        xlab(NULL)
      
      (p1 + p2)
    })
  })
  names(pp) <- res_pred[, unique(dist)]
  pp
})
names(plot_pred) <- res_pred[, unique(effect)]

invisible(lapply(1:length(plot_pred), function(i) {
  lapply(1:length(plot_pred[[i]]), function(j) {
    pp <- patchwork::wrap_plots(plot_pred[[i]][[j]], ncol = 1)
    ggsave(file.path("supplement_plots/results_pred", paste0("logreg_pred", "_", names(plot_pred)[i], "_", names(plot_pred[[i]])[j], ".pdf")), plot = pp , width = 210, height = 260, units = "mm", scale = 1.5) 
  })
}))

# Plot NRMSE --------------------------------------------------------------------
plot_nrmse <- lapply(res_nrmse[, unique(effect)], function(em) {
  pp <- lapply(res_nrmse[, unique(dist)], function(dm) {
    lapply(res_nrmse[, unique(p)], function(pm) {
      nm <- 1000
      if(pm == 4){
        subcap <- "a)"
      }else if(pm == 10){
        subcap <- "c)"
      }else{
        subcap <- "e)"
      }
      p1 <- ggplot(res_nrmse[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = nrmse, fill = Method, color=Method)) +
        facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
        geom_boxplot(outlier.size = .1) +
        theme_bw(base_size = 14) + 
        theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+  
        coord_flip() + 
        ylab("Data NRMSE") + 
        xlab(NULL)+
        scale_fill_manual(values =c(method_colors)) +
        scale_colour_manual(values = c(outline_colors)) +
        guides(fill = "none", color = "none")+ 
        ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))

      nm <- 10000
      if(pm == 4){
        subcap <- "b)"
      }else if(pm == 10){
        subcap <- "d)"
      }else{
        subcap <- "f)"
      }
      p2 <- ggplot(res_nrmse[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = nrmse, fill = Method, color=Method)) +
        facet_grid(prop_mis ~ pattern, scales = "free", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) +
        geom_boxplot(outlier.size = .1) +
        theme_bw(base_size = 14) + 
        theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+  
        coord_flip() + 
        ylab("Data NRMSE") + 
        xlab(NULL)+
        scale_fill_manual(values =c(method_colors)) +
        scale_colour_manual(values = c(outline_colors)) +
        guides(fill = "none", color = "none")+ 
        ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", nm, ", p =", pm ))
      
      (p1 + p2)
    })
  })
  names(pp) <- res_nrmse[, unique(dist)]
  pp
})
names(plot_nrmse) <- res_nrmse[, unique(effect)]

invisible(lapply(1:length(plot_nrmse), function(i) {
  lapply(1:length(plot_nrmse[[i]]), function(j) {
    pp <- patchwork::wrap_plots(plot_nrmse[[i]][[j]], ncol = 1)
    ggsave(file.path("supplement_plots/results_nrmse", paste0("logreg_nrmse", "_", names(plot_nrmse)[i], "_", names(plot_nrmse[[i]])[j], ".pdf")), plot = pp , width = 210, height = 260, units = "mm", scale = 1.5) 
  })
}))


# Colour methods for coverage-------------------------------------------------------------------------------
method_colors <- c("MissARF" = "#4472C4", "MissForest" = "white", "MissForest PMM" = "white", 
                   "MICE RF" = "white", "MICE PMM" = "#1d9053", "Random Imp." = "white")


outline_colors <- c("MissARF" = "blue3", "MissForest" = "black", "MissForest PMM" = "black", 
                    "MICE RF" = "black", "MICE PMM" = "darkgreen", "Random Imp." = "black")

# Plot coverage --------------------------------------------------------------------
plot_cov <- lapply(res_coverage[, unique(effect)], function(em) {
  pp <- lapply(res_coverage[, unique(dist)], function(dm) {
      lapply(eva[, unique(p)], function(pm) {
        if(pm == 4){
          subcap <- "a)"
        }else if(pm == 10){
          subcap <- "c)"
        }else{
          subcap <- "e)"
        }
        p1 <- ggplot(eva[n == 1000 & p == pm & dist == dm & effect == em, ], aes(x = Method, y = coverage_rate, fill = Method, color=Method)) +
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
          ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", 1000, ", p =", pm ))+ 
          ylab("Coverage rate")
          
        if(pm == 4){
          subcap <- "b)"
        }else if(pm == 10){
          subcap <- "d)"
        }else{
          subcap <- "f)"
        }
        p2 <- ggplot(eva[n == 10000 & p == pm & dist == dm & effect == em, ], aes(x = Method, y = coverage_rate, fill = Method, color=Method)) +
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
          ggtitle(paste(subcap,"dist =", dm, ", effect =", em, ", n =", 10000, ", p =", pm ))+ 
          ylab("Coverage rate")
        
        (p1 + p2)
      })
    })
  names(pp) <- res_coverage[, unique(dist)]
  pp
})
names(plot_cov) <- res_coverage[, unique(effect)]

invisible(lapply(1:length(plot_cov), function(i) {
  lapply(1:length(plot_cov[[i]]), function(j) {
      pp <- patchwork::wrap_plots(plot_cov[[i]][[j]], ncol = 1)
      ggsave(file.path("supplement_plots/results_cov", paste0("logreg_coverage", "_", names(plot_cov)[i], "_", names(plot_cov[[i]])[j], ".pdf")), plot = pp , width = 210, height = 260, units = "mm", scale = 1.5) 
  })
}))

#Plot AW -----------------------------------------------------------------------
plot_aw <- lapply(res_coverage[, unique(effect)], function(em) {
  pp <- lapply(res_coverage[, unique(dist)], function(dm) {
    lapply(eva[, unique(p)], function(pm) {
      nm <- 1000
      if(pm == 4){
        subcap <- "a)"
      }else if(pm == 10){
        subcap <- "c)"
      }else{
        subcap <- "e)"
      }
      p1 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = average_width, fill = Method, color=Method)) +
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
      
      nm <- 10000
      if(pm == 4){
        subcap <- "b)"
      }else if(pm == 10){
        subcap <- "d)"
      }else{
        subcap <- "f)"
      }
      p2 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = average_width, fill = Method, color=Method)) +
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
      
      (p1 + p2)
    })
  })
  names(pp) <- res_coverage[, unique(dist)]
  pp
})
names(plot_aw) <- res_coverage[, unique(effect)]

invisible(lapply(1:length(plot_aw), function(i) {
  lapply(1:length(plot_aw[[i]]), function(j) {
    pp <- patchwork::wrap_plots(plot_aw[[i]][[j]], ncol = 1)
    ggsave(file.path("supplement_plots/results_aw", paste0("logreg_aw", "_", names(plot_aw)[i], "_", names(plot_aw[[i]])[j], ".pdf")), plot = pp , width = 210, height = 260, units = "mm", scale = 1.5) 
  })
}))


#Plot RMSE -----------------------------------------------------------------------
plot_rmse <- lapply(res_coverage[, unique(effect)], function(em) {
  pp <- lapply(res_coverage[, unique(dist)], function(dm) {
    lapply(eva[, unique(p)], function(pm) {
      nm <- 1000
      if(pm == 4){
        subcap <- "a)"
      }else if(pm == 10){
        subcap <- "c)"
      }else{
        subcap <- "e)"
      }
      p1 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = rmse, fill = Method, color=Method)) +
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
        ylab("Coefficient RMSE")
      
      nm <- 10000
      if(pm == 4){
        subcap <- "b)"
      }else if(pm == 10){
        subcap <- "d)"
      }else{
        subcap <- "f)"
      }
      p2 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = rmse, fill = Method, color=Method)) +
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
        ylab("Coefficient RMSE")
      
      (p1 + p2)
    })
  })
  names(pp) <- res_coverage[, unique(dist)]
  pp
})
names(plot_rmse) <- res_coverage[, unique(effect)]

invisible(lapply(1:length(plot_rmse), function(i) {
  lapply(1:length(plot_rmse[[i]]), function(j) {
    pp <- patchwork::wrap_plots(plot_rmse[[i]][[j]], ncol = 1)
    ggsave(file.path("supplement_plots/results_rmse", paste0("logreg_rmse", "_", names(plot_rmse)[i], "_", names(plot_rmse[[i]])[j], ".pdf")), plot = pp , width = 210, height = 260, units = "mm", scale = 1.5) 
  })
}))
