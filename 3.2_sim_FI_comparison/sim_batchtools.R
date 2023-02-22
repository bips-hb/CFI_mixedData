library(batchtools)
library(reshape2)
library(ggplot2)
library(mvtnorm)
library(fastDummies)
library(dplyr)
#library(reticulate)
set.seed(2022)
# setwd("/home/blesch/knockoffs/sim_FI_comparison")
# Registry ----------------------------------------------------------------
reg_name <- "sim_batchtools"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(file.dir = reg_dir, conf.file = "config.R")


# Problems -----------------------------------------------------------
source("problems.R") # DGP

my_dgp <- function(job, data, n, p, ...) {
  generated_dat <-  dgp(n = n, p = p, ...)
  train_instances <- sample(1:nrow(generated_dat), n*0.66, replace = F)
  list(train = generated_dat[train_instances,], 
       test = generated_dat[-train_instances,])
}

addProblem(name = "dgp", fun = my_dgp, seed = 1)

# Algorithms -----------------------------------------------------------
source("algorithms.R") # Algorithms

addAlgorithm(name = "boruta", fun = boruta) ## added 0123
addAlgorithm(name = "cpi_gauss", fun = cpi_gauss) ## added 0123
addAlgorithm(name = "cpi_deep", fun = cpi_gauss) ## added 0123
addAlgorithm(name = "cpi_seq", fun = cpi_seq)
addAlgorithm(name = "loco", fun = loco)
addAlgorithm(name = "cond_subgroup", fun = cond_subgroup)
addAlgorithm(name = "pfi", fun = pfi)
addAlgorithm(name = "sage", fun = SAGE)

# Parameters --------------------------------------------------------------

repls <-  500
n <- c(300,500, 1000, 2000, 2500,3000)
p <- c(12)
num_cat <- c(2,5)
error_level <- c(2)
cov_base <- c(0.5, 0.8)
outcome_y <-  list( "regr", "classif")
learners <- list("ranger",  "nnet",  "regression") #
space <- c("Hi")

# Experiments -----------------------------------------------------------
prob_design <- list(
  dgp = rbind(expand.grid(n = n,
                          p = p,
                          num_cat = num_cat,
                          error_level = error_level,
                          cov_base = cov_base,
                          outcome = outcome_y))
)
algo_design <- list(
   boruta = data.frame("learner" = "boruta", "space" = space), ## added 0123
   cpi_gauss = expand.grid(learner = learners, space=space), ## added 0123
   cpi_deep = expand.grid(learner = learners, space=space)#, ## added 0123
#   cpi_seq = expand.grid(learner = learners, space=space),
#   pfi = expand.grid(learner = learners, space=space),
#   loco = expand.grid(learner = learners,space=space),
#   cond_subgroup = expand.grid(learner = learners,space=space),
#   sage = expand.grid(learner = learners,space=space)
)

addExperiments(prob_design, algo_design, repls = repls)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
#testJob(id = 59999)

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
reduceResultsList()
#res <-  flatten(ijoin(reduceResultsDataTable(), getJobPars()))
resi = unwrap(ijoin(reduceResultsDataTable(24001:168000), getJobPars(24001:168000))) # for CPIgauss and CPI deep results 
#resi = unwrap(ijoin(reduceResultsDataTable(), getJobPars())) # when running other (res_0808) results
#resi = unwrap(ijoin(reduceResultsDataTable(id = 1:24000), getJobPars(1:24000))) # for botura results
#unlist(getJobPars(id = 41)) # example
#
#write.csv(apply(resi,2,as.character), file = "/home/blesch/knockoffs/sim_FI_comparison/res_0808.csv", row.names = F)
write.csv(apply(resi,2,as.character), file = "/home/blesch/knockoffs/sim_FI_comparison/res_0123.csv", row.names = F)

# plot an overview of results
# res_plot <- resi %>% dplyr::select(-job.id) %>% dplyr::select(-space) %>% group_by(problem, algorithm, n, p, learner, outcome, error_level, cov_base, num_cat) %>% summarise_all(mean)
# res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n", "p", "learner", "outcome", "num_cat", "cov_base", "error_level"), value.name = "rejection_rate")
# #res_plot$binary_variables <- do.call(rbind, lapply(res_plot$binary_variables, function(x){paste(x, sep="", collapse="")})) 
# ggplot(res_plot, aes(x = n, y = rejection_rate, col = algorithm )) +
#   facet_grid( variable ~ p  + learner + error_level + cov_base + outcome)+
#   # facet_grid(algorithm ~ p)+
#   geom_line()

