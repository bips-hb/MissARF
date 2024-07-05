
library(data.table)
library(ggplot2)
library(patchwork)
source("setup.R")

reg_name <- "diabetes_pred"

# Get results -------------------------------------------------------------
res <- readRDS(file.path(path, paste0(reg_name, ".rds")))

# Evaluate ----------------------------------------------------------------
res[, Method := factor(paste0(algorithm,
                              ifelse(is.na(method), "", paste0("_", method)),
                              ifelse(is.na(m), "", paste0("_", as.character(m))),
                              ifelse(is.na(expectation), "", ifelse(expectation, "_expct", "")),
                              ifelse(is.na(pmm.k), "", paste0("_", as.character(pmm.k))),
                              ifelse(is.na(finite_bounds), "", paste0("_", finite_bounds)),
                              ifelse(is.na(min_node_size), "", paste0("_", sprintf("%02d", min_node_size))),
                              ifelse(is.na(num_trees), "", paste0("_", sprintf("%03d", num_trees)))
))]
res[, pattern := factor(pattern, levels = c("MCAR", "MAR", "MNAR"))]
res[, perf := V1]

# Plot --------------------------------------------------------------------
plots <- lapply(res[, unique(n)], function(nm) {
  ggplot(res[n == nm, ], aes(x = Method, y = perf)) +
    facet_grid(prop_mis ~ pattern) +
    geom_boxplot() +
    theme_bw() + 
    ylab("Brier score") + 
    coord_flip() + 
    ggtitle(paste("n =", nm))
})
names(plots) <- res[, unique(n)]

pp <- patchwork::wrap_plots(plots, ncol = 1)
ggsave(file.path("results", paste0(reg_name, ".pdf")), plot = pp, width = 10, height = 20)