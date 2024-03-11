rm(list = ls())

library(ramcmc)
library(deSolve)
library(tidyverse)

runs <- read_csv("runs.csv")
run_ns <- 1:nrow(runs)

for(run_n in run_ns){
  # Model configuration
  config <- list()
  config$species <- runs[run_n,]$species
  config$model_name <- runs[run_n,]$model_name
  config$run_name <- runs[run_n,]$run_name
  
  # Data and params
  source("data/data_config.R")
  config$fixed_param$I0 <- runs[run_n,]$I0
  config$fixed_param$T_NT <- runs[run_n,]$T_NT
  config$fixed_param$T_HS <- runs[run_n,]$T_HS
  
  # MCMC configuration
  config$n_iterations <- 100000
  config$adapt_shape_stop <- 20000
  
  # MCMC launch
  source("mcmc/launch_mcmc.R")
}
