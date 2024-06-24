
library(data.table)
library(ggplot2)
library(patchwork)

reg_name <- "logreg_nrmse"

# Get results -------------------------------------------------------------
res <- readRDS(paste0(reg_name, ".Rds"))

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
res[, nrmse := V1]

# Plot --------------------------------------------------------------------
plots <- lapply(res[, unique(effect)], function(em) {
  pp <- lapply(res[, unique(dist)], function(dm) {
    pp <- lapply(res[, unique(n)], function(nm) {
      lapply(res[, unique(p)], function(pm) {
        ggplot(res[n == nm & p == pm & dist == dm & effect == em, ], aes(x = Method, y = nrmse)) +
          facet_grid(prop_mis ~ pattern) +
          geom_boxplot() +
          theme_bw() + 
          coord_flip() + 
          ggtitle(paste("n =", nm, ", p =", pm, ", dist =", dm, ", effect =", em))
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
      ggsave(paste0(reg_name, "_", names(plots)[i], "_", names(plots[[i]])[j], "_", names(plots[[i]][[j]])[k], ".pdf"), plot = pp, width = 10, height = 20)
    })
  })
}))
