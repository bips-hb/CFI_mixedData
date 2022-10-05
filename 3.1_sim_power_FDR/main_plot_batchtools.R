library(batchtools)
library(reshape2)
library(ggplot2)
library(reticulate)
use_condaenv(condaenv = 'knockoffenv', required = TRUE)
# Note:need to unload package MASS and load dplyr in case FI_comparison was run previously to avoid MASS masking dplyr select()
set.seed(2022)

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
n_main <- c(500,750, 1000, 2000,3000,5000,7000)
binary_var_main <- list(c("X1", "X3"))
categories_main <- c(10)
outcome_y_main <-  list("regr")
effect_sizes_main <- list(
  c("x1x4" = 0.5, "x2x3" = 0.5, "x3y" = 0.5, "x4y" = 0.5))
learners_main <- list("ranger")
space <- c("Hi")
# Experiments -----------------------------------------------------------
prob_design <- list(
  DAG_cat_data = rbind(expand.grid(n = n_main,
                                   cat_variables = binary_var_main,
                                   num_levels = categories_main,
                                   effect_size = effect_sizes_main,
                                   outcome = outcome_y_main))
)

algo_design <- list(
  sequential = expand.grid(learner = learners_main, space=space),
  gaussian = expand.grid(learner = learners_main,space=space),
  deep = expand.grid(learner = learners_main,space=space)
)

addExperiments(prob_design, algo_design, repls = repls, combine = "crossprod")
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

write.csv(apply(res,2,as.character), file = "/home/blesch/knockoffs/sim_power_FDR/res_0922022.csv")


