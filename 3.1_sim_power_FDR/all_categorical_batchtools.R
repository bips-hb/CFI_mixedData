# This script is for the simulation of all categorical variables in Section 3.1
# We consider DGPs where the data is all binary (2 level categoricals) and all categoricals (4 level categoricals)

library(batchtools)
library(reshape2)
library(ggplot2)
library(reticulate) 
# Note:need to unload package MASS and load dplyr in case FI_comparison was run previously to avoid MASS masking dplyr select()
use_condaenv(condaenv = 'knockoffenv', required = TRUE)

set.seed(2022)
# setwd("./sim_power_FDR")
# Registry ----------------------------------------------------------------
reg_name <- "sim_batchtools"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(file.dir = reg_dir, conf.file = "config.R")
#getDefaultRegistry()

# Problems -----------------------------------------------------------
source("problems.R") # DGP

DAG_cat_dat <- function(job, data, n,...) {
  generated_dat <-  DAG_cat(n = n, ...)
  train_instances <- sample(1:nrow(generated_dat), n*0.66, replace = F)
  list(train = generated_dat[train_instances,], 
       test = generated_dat[-train_instances,])
}

addProblem(name = "DAG_cat_data", fun = DAG_cat_dat, seed = 1)
# Algorithms -----------------------------------------------------------
source("algorithms.R") # Algorithms
addAlgorithm(name = "sequential", fun = wrapper_sequential)
addAlgorithm(name = "gaussian", fun = wrapper_gaussian)
addAlgorithm(name = "deep", fun = wrapper_deep)

# Parameters --------------------------------------------------------------

repls <- 500
space <- c("Hi")
summarizeExperiments()

# add experiments for appendix ------------------------------------------
# Parameters --------------------------------------------------------------

n_app <- c(100,250,500,750,1000)
binary_var_app <- list(c("X1","X2", "X3", "X4"))
categories_app <- c(2,4)
outcome_y_app <-  list("regr")
effect_sizes_app <- list(
  c("x1x4" = 0.9, "x2x3" = 0.9, "x3y" = 0.9, "x4y" = 0.9),
  c("x1x4" = 0.5, "x2x3" = 0.5, "x3y" = 0.5, "x4y" = 0.5))
learners_app <- list(
  "ranger", 
  "regression"
)
# Experiments -----------------------------------------------------------
prob_design_app <- list(
  DAG_cat_data = rbind(expand.grid(n = n_app,
                                   cat_variables = binary_var_app,
                                   num_levels = categories_app,
                                   effect_size = effect_sizes_app,
                                   outcome = outcome_y_app))
)

algo_design_app <- list(
  sequential = expand.grid(learner = learners_app, space=space),
  gaussian = expand.grid(learner = learners_app,space=space),
  deep = expand.grid(learner = learners_app,space=space)
)
addExperiments(prob_design_app, algo_design_app, repls = repls, combine = "crossprod")
summarizeExperiments()

# Test jobs -----------------------------------------------------------
# testJob(id = 2)

# Submit -----------------------------------------------------------
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE, 
                              ncpus = 1, memory = 6000, walltime = 10*24*3600, 
                              max.concurrent.jobs = 40))
} else {
  submitJobs()
}
waitForJobs()
# Get results -------------------------------------------------------------
res <-  flatten(ijoin(reduceResultsDataTable(), getJobPars()))

write.csv(apply(res,2,as.character), file = "/home/blesch/knockoffs/sim_power_FDR/res_all_cat_082022.csv")


