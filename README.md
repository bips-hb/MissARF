## Imputation benchmark for comparing MissARF with other imputation algorithms
This repository contains the code for the simulation study and real data example 
in the paper "Missing value imputation with adversarial random forests -- MissARF" 
by P. Golchian, J. Kapar, D. Watson, M. Wright

# Imputation benchmarks
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
* plots_paper_sim.R - Plot simulation results shown in paper
* plots_paper_real.R - Plot real data results shown in paper
* plots_supplement.R - Plot all simulation results (supplement)
* plots_supplement_realdata.R - Plot real data results shown in supplement
* plot_runtime.R - Plot runtime comparison, calculate mean runtime shown in table (supplement)

