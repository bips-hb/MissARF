# Imputation benchmarks
This repository contains the code for the simulation study and real data example 
in the paper "Missing value imputation with adversarial random forests -- MissARF" 
by P. Golchian, J. Kapar, D. Watson, M. Wright. It compares MissARF with other 
state-of-the-art imputation algorithms.

**Note that a HPC cluster is required to run the benchmark experiments in a reasonable amount of time.**

Setup for benchmark
* setup.R - Required packages and scripts
* problems.R - Setup data 
* algorithms.R - Imputation algorithms
* batchtools.conf.R - Cluster setup
* utils.R - utility functions for e.g. evaluation metrics, imputations, and averaging datasets

To run simulation study, run: 
* sim_logreg_nrmse.R - NRMSE simulation (difference to fully observed data)
* sim_logreg_pred.R - Prediction simulation (Logistic regression on imputed data)
* sim_logreg_coverage.R - Coverage simulation (Confidence interval coverage of GLM coefficients)

To run real data example, run:
* real_diabetes_nrmse.R - NRMSE real data example
* real_diabetes_pred.R - Prediction real data example
* real_diabetes_coverage.R - Coverage real data example

For runtime comparison, run:
* runtime_microbenchmark.R - Runtime comparison

Aggregated simulation results and runtime:
* table.R - Aggregated simulation results and runtime comparison

Plot results:
* plot_paper_nrmse_brier.R - Plot simulation results of settin I of NRMSE and Brier score (shown in paper)
* plot_paper_sim_settingII.R - Plot simulation results of setting II of Coverage, CI width, RMSE (shown in paper)
* plots_paper_real.R - Plot real data results (shown in paper)
* plots_supplement.R - Plot all simulation results (supplement)
* plots_supplement_realdata.R - Plot real data results (supplement)
* plot_runtimes.R - Plot runtime comparison, calculate mean runtime shown in table (supplement)

Further experiments with plots:
* sim_logreg_coverage_min_node_sizes.R - Simulation with varying minimum node sizes 2-400 for MissARF (setting II)
* plot_min_node_sizes_line_se.R - Plots results of sim_logreg_coverage_min_node_sizes.R 

* sim_logreg_coverage_num_trees.R - Simulation of different number of trees 10-160 and min node sizes 10, 20 & 30 (setting II)
* plot_num_trees_min_nodes_cov.R - Plots results of sim_logreg_coverage_num_trees.R
* sim_logreg_nrmse_num_trees.R - Simulation of different number of trees 10-160 and min node sizes 10, 20 & 30 for NRMSE 
* plot_num_trees_nrmse_line.R - Plot results of sim_logreg_nrmse_num_trees.R of different number of trees 10-160 and min node sizes 10, 20 & 30 of NRMSE

* sim_logreg_coverage_m_40.R - Simulation with m=40 multiple imputation
* plots_multiple_40.R - Plots results of sim_logreg_coverage_m_40.R and compares it with m=20


