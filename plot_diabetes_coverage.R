
library(data.table)
library(ggplot2)
library(patchwork)
source("setup.R")

reg_name <- "diabetes_coverage"

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

eva <- res[, .(raw_bias = mean(estimate - truth), 
               percent_bias = 100 * abs(mean((estimate - truth) / truth)),
               relative_bias = mean((estimate - truth) / abs(truth)),
               relative_bias_median = median((estimate - truth) / abs(truth)),
               coverage_rate = mean(lower < truth & truth < upper),
               average_width = mean(upper - lower), 
               rmse = sqrt(mean((estimate - truth)^2))), 
           by = .(Method, n, prop_mis, pattern, term)] 
eva <- unique(eva)

# Plot --------------------------------------------------------------------
plots <- lapply(res[, unique(n)], function(nm) {
  p1 <- ggplot(eva[n == nm, ], aes(x = Method, y = relative_bias, fill = term)) +
    facet_grid(prop_mis ~ pattern, scales = "free") +
    geom_bar(stat = "identity", position = "dodge") +
    theme_bw() + 
    coord_flip() + 
    ggtitle(paste("n =", nm))
  
  p2 <- ggplot(eva[n == nm, ], aes(x = Method, y = coverage_rate)) +
    facet_grid(prop_mis ~ pattern) +
    geom_boxplot() +
    geom_hline(yintercept = 0.95, color = "red") +
    theme_bw() + 
    coord_flip()
  
  p3 <- ggplot(eva[n == nm, ], aes(x = Method, y = average_width)) +
    facet_grid(prop_mis ~ pattern, scales = "free") +
    geom_boxplot(outlier.size = .1) +
    theme_bw() + 
    coord_flip()
  
  (p1 + p2 + p3)
})
names(plots) <- res[, unique(n)]

pp <- patchwork::wrap_plots(plots, ncol = 1)
ggsave(file.path("results", paste0(reg_name, ".pdf")), plot = pp, width = 30, height = 10)

