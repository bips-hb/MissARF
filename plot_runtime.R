# For Supplement: 
# 1) Plots of the runtime over p and miss_prop. 
# 2) Mean runtime for each method (for Table).
# Note: run the script once for results for thread=1 and once for thread=16.

library(dplyr)
library(data.table)
library(ggplot2)
library(microbenchmark)
library(patchwork)
library(dplyr)
library(knitr)

#Get runtime results
#loaded_results <- readRDS("microbenchmark_results_med_rand_k1.rds")
#loaded_results1 <- readRDS("microbenchmark_results_k1.rds")
runtime_k16 <- readRDS("microbenchmark_results_k16.rds")
runtime_k16_arf <- readRDS("microbenchmark_results_k16_missARF.rds")

#mice_pmm_1 <- readRDS("runtime_k1_mice_pmm.rds")
#mice_pmm_16 <- readRDS("runtime_k16_mice_pmm.rds")

#threads = 16
#runtime_k16 <- runtime_k16 %>%
#  filter(!grepl("mice_pmm", expr))

#runtime <- rbind(runtime_k16, mice_pmm_16)

runtime_k16 <- runtime_k16 %>%
  filter(!grepl("arf", expr))

runtime <- rbind(runtime_k16, runtime_k16_arf)
#runtime <- runtime_k16 #res_runtime_16# #runtime_k16

##thread=1
#runtime <- rbind(loaded_results, loaded_results1)
#runtime <- runtime %>%
#    filter(!grepl("mice_pmm", expr))
#runtime <- rbind(runtime, mice_pmm_1)

## 1) Plot the runtime over p and miss_prop -----------------------------------
plot_fct <- function(filter = "sing_1000_", ts = 1e9, x_labs="Time (sec)", title="NA", k = "k16"){
  runtime_filtered <- runtime %>%
    filter(grepl(filter, as.character(runtime$expr))) %>%
    mutate(
      expr = sub("\\..*$", "", expr),
      time_ms = time/ts,  
      p = sub(".*_([0-9]+)_([0-9]+)$", "\\1", expr),  
      prop_mis = sub(".*_([0-9]+)$", "\\1", expr),  

      # Rename method names 
      method = case_when(
        grepl("arf", expr) ~ "MissARF", 
        grepl("mice_pmm", expr) ~ "MICE PMM",      
        grepl("mice_rf", expr) ~ "MICE RF",  
        grepl("median", expr) ~ "Median Imp.", 
        grepl("missranger_pmm", expr) ~ "MissForest PMM",  
        grepl("missranger_(multi|sing)", expr) ~ "MissForest", 
        grepl("random",expr) ~ "Random Imp.", 
        TRUE ~ "Other" 
      )
    ) %>%
    mutate(
      prop_mis = factor(prop_mis, levels = c("01", "02", "04"), labels = c("0.1", "0.2", "0.4")),  
      p = factor(p, levels = c("4", "10", "20"), labels=c("p=4", "p=10", "p=20")),  
      method = factor(method, levels = c("MissARF", "MICE RF", "MICE PMM","MissForest PMM", "MissForest", 
                                         "Median Imp.", "Random Imp."))  
    )
  
    p <-runtime_filtered %>%
    ggplot(aes(x = time_ms , y =method ))+
    geom_boxplot() +
    theme_bw(base_size = 14) + 
    theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1))+ 
    labs(title = title, 
         x = x_labs, 
         y = NULL)+
    facet_grid(prop_mis ~ p, scales = "free_x", labeller = labeller(prop_mis =c('0.1'=paste("mis. = 0.1"), '0.2'=paste("mis. = 0.2"), '0.4'=paste("mis. = 0.4")))) 
  
  return(p)
}
  
#thread = 16  
p0 <- plot_fct(filter = "sing_500_", ts = 1e9, title="Single imputation, n=500, 16 threads")
p1 <- plot_fct(filter = "sing_1000_", ts = 1e9, title="Single imputation, n=1000, 16 threads")
p2<-plot_fct(filter = "sing_10000_", ts = 1e9, x_labs="Time (sec)", title="Single imputation, n=10000, 16 threads")
#plot_fct(filter = "multi_10000_", ts = 1e9, x_labs="Time (s)", title="Runtime of mutliple imputation and n=10000 with 16 threads.")
p3<-plot_fct(filter = "multi_500_", ts = 1e9, x_labs="Time (sec)", title="Mutliple imputation, n=500, 16 threads")
p4<-plot_fct(filter = "multi_1000_", ts = 1e9, x_labs="Time (sec)", title="Mutliple imputation, n=1000, 16 threads")
p5<-plot_fct(filter = "multi_10000_", ts = 6e10, x_labs="Time (min)", title="Mutliple imputation, n=10000, 16 threads")

##thread=1
#p1 <-plot_fct(filter = "sing_1000_", ts = 1e9, x_labs="Time (sec)", title="a) Single imputation, n=1000, 1 thread", k = "k1")
#p2<-plot_fct(filter = "sing_10000_", ts = 1e9, x_labs="Time (sec)", title="b) Single imputation, n=10000, 1 thread", k = "k1")
##plot_fct(filter = "multi_10000_", ts = 1e9, x_labs="Time (s)", title="Runtime of mutliple imputation and n=10000 with 1 threads.", k = "k1")
#p3<-plot_fct(filter = "multi_1000_", ts = 1e9, x_labs="Time (sec)", title="c) Mutliple imputation, n=1000, 1 thread", k = "k1")
#p4<-plot_fct(filter = "multi_10000_", ts = 6e10, x_labs="Time (min)", title="d) Mutliple imputation, n=10000, 1 thread", k = "k1")

bb <- (p0 | p3) / (p1 | p4) / (p2 | p5) 
#thread=1
#ggsave("supplement_plots/runtime_thread1.pdf", plot = bb, width = 210, height = 173.33, units = "mm", scale = 1.5) 
#thread=16
ggsave("supplement_plots/runtime_thread16.pdf", plot = bb, width = 210, height = 260, units = "mm", scale = 1.5) 

## 2) Caculate the mean runtime for each method --------------------------------

mean_method_fct <- function(filter = "sing_", ts = 1e9){
  runtime_filtered <- runtime %>%
    filter(grepl(filter, as.character(runtime$expr))) %>%
    mutate(
      time_ms = time/ts, #/1e9, #/ 6e+7, # 1e6,  
      p = sub(".*_([0-9]+)_([0-9]+)$", "\\1", expr),  
      prop_mis = sub(".*_([0-9]+)$", "\\1", expr),  
      
      # Rename method names
      method = case_when(
        grepl("arf", expr) ~ "MissARF", 
        grepl("mice_pmm", expr) ~ "MICE PMM",      
        grepl("mice_rf", expr) ~ "MICE RF",  
        grepl("median", expr) ~ "Median Imp.", 
        grepl("missranger_pmm", expr) ~ "MissForest PMM",  
        grepl("missranger_(multi|sing)", expr) ~ "MissForest", 
        grepl("random",expr) ~ "Random Imp.", 
        TRUE ~ "Other" 
      )
    ) %>%
    mutate(
      prop_mis = factor(prop_mis, levels = c("01", "02", "04"), labels = c("0.1", "0.2", "0.4")),  
      p = factor(p, levels = c("4", "10", "20"), labels=c("p=4", "p=10", "p=20")),  
      method = factor(method, levels = c("Random Imp.", "Median Imp.", "MissForest", "MissForest PMM",  "MICE PMM", 
                                         "MICE RF", "MissARF"))  
    )

  mean_times_all_methods <- runtime_filtered %>%
    group_by(method) %>%                
    summarize(mean_time_ms = mean(time_ms)) 
  
  return(mean_times_all_methods)
}

sing_500 <- mean_method_fct(filter = "sing_500_", ts = 1e9)
multi_500 <- mean_method_fct(filter = "multi_500_", ts = 1e9)
sing_1000 <- mean_method_fct(filter = "sing_1000_", ts = 1e9)
multi_1000 <- mean_method_fct(filter = "multi_1000_", ts = 1e9)
sing_10000 <- mean_method_fct(filter = "sing_10000_", ts = 1e9)
multi_10000 <- mean_method_fct(filter = "multi_10000_", ts = 1e9)
mean_method_fct(filter = "sing_", ts = 1e9)
mean_method_fct(filter = "multi_", ts = 1e9)
mean_method_fct(filter = "", ts = 1e9)

#Table -------------------------------------------------------------------------
# Combine and round values
table_df <- sing_500 %>%
  rename(Single_500 = mean_time_ms) %>%
  left_join(multi_500 %>% rename(Multiple_500 = mean_time_ms), by = "method") %>%
  left_join(sing_1000 %>% rename(Single_1000 = mean_time_ms), by = "method") %>%
  left_join(multi_1000 %>% rename(Multiple_1000 = mean_time_ms), by = "method") %>%
  left_join(sing_10000 %>% rename(Single_10000 = mean_time_ms), by = "method") %>%
  left_join(multi_10000 %>% rename(Multiple_10000 = mean_time_ms), by = "method") %>%
  mutate(across(-method, ~ round(.x, 2)))  # round all numeric columns to 2 digits

# Print LaTeX rows only
kable(
  table_df,
  format = "latex",
  align = "l|ll|ll|ll",
  col.names = c(
    "Method", 
    "Single", "Multiple", 
    "Single", "Multiple", 
    "Single", "Multiple"
  ),
  booktabs = FALSE
)

