
library(data.table)
library(ggplot2)
library(patchwork)
source("setup.R")

reg_name <- "logreg_coverage"

# Get results -------------------------------------------------------------
res <- readRDS(file.path(path, paste0(reg_name, ".rds")))

# Evaluate ----------------------------------------------------------------
res[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]

# Rename methods
res[, Method := factor(Method, 
                       levels = c("arf_local_10_100", "missRanger_5", "missRanger_0", 
                                  "mice_rf", "mice_pmm", "random"), 
                       labels = c("ARF", "MissForest PMM", "MissForest", 
                                  "MICE RF", "MICE PMM", "Random Imp."))]

eva <- res[, .(raw_bias = mean(estimate - truth), 
               percent_bias = 100 * abs(mean((estimate - truth) / truth)),
               relative_bias = mean((estimate - truth) / abs(truth)),
               relative_bias_median = median((estimate - truth) / abs(truth)),
               coverage_rate = mean(lower < truth & truth < upper),
               average_width = mean(upper - lower), 
               rmse = sqrt(mean((estimate - truth)^2))), 
           by = .(Method, n, p, prop_mis, pattern, dist, effect, term)] 
eva <- unique(eva)

# Plot --------------------------------------------------------------------
plots <- lapply(res[, unique(effect)], function(em) {
  pp <- lapply(res[, unique(dist)], function(dm) {
    pp <- lapply(res[, unique(n)], function(nm) {
      lapply(eva[, unique(p)], function(pm) {
        p1 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = rmse)) +
          facet_grid(prop_mis ~ pattern, scales = "free") +
          geom_boxplot(outlier.size = .1) +
          theme_bw() + 
          coord_flip() + 
          ylab("Coefficient RMSE") +
          ggtitle(paste("n =", nm, ", p =", pm, ", dist =", dm, ", effect =", em))
        
        p2 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = coverage_rate)) +
          facet_grid(prop_mis ~ pattern) +
          geom_boxplot(outlier.size = .1) +
          geom_hline(yintercept = 0.95, color = "red") +
          theme_bw() + 
          coord_flip() + 
          ylab("Coverage rate")
        
        p3 <- ggplot(eva[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = average_width)) +
          facet_grid(prop_mis ~ pattern, scales = "free") +
          geom_boxplot(outlier.size = .1) +
          theme_bw() + 
          coord_flip() + 
          ylab("Average CI width")
        
        (p1 + p2 + p3)
      })
    })
    names(pp) <- res[, unique(n)]
    pp
  })
  names(pp) <- res[, unique(dist)]
  pp
})
names(plots) <- res[, unique(effect)]

invisible(lapply(1:length(plots), function(i) {
  lapply(1:length(plots[[i]]), function(j) {
    lapply(1:length(plots[[i]][[j]]), function(k) {
      pp <- patchwork::wrap_plots(plots[[i]][[j]][[k]], ncol = 1)
      ggsave(file.path("results", paste0(reg_name, "_", names(plots)[i], "_", names(plots[[i]])[j], "_", names(plots[[i]][[j]])[k], ".pdf")), plot = pp, width = 30, height = 30)
    })
  })
}))

