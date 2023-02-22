library(batchtools)
library(reshape2)
library(ggplot2)
library(mvtnorm)
library(fastDummies)
library(dplyr)
#library(reticulate)
set.seed(2022)
# setwd("./knockoffs/sim_FI_comparison")
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
source("algorithms.R") # helper functions

validate_model = function(data, job, instance, learner, dummy_enc, ...){
  if (dummy_enc == TRUE){
    print("dummy encode data")
    helper = make_mlr3_dummy(instance = instance, learner = learner)
    test_dat = helper$test
      }
  else{
    helper = make_mlr3(instance = instance, learner = learner)
    test_dat = instance$test
  }
  # train model
  fit <- helper$learner$train(helper$task)
#  pred <-  Predictor$new(fit, data = test_dat, y = "y")
  # predict test data -> validate model
  if (helper$measure$id == "regr.mse"){
    measure = list("rsquared" = mlr3::msr("regr.rsq"))
    } else{
    measure = list("accuracy" = mlr3::msr("classif.acc"))
    }
  res = list(fit$predict_newdata(newdata = test_dat)$score(measures = measure[[1]]))
  names(res) = names(measure[1])
  return(res)
}

 addAlgorithm(name = "validation", fun = validate_model)

# Parameters --------------------------------------------------------------

repls <-  500
n <- c(300,500, 1000, 2000, 2500,3000)
p <- c(12)
num_cat <- c(2,5)
error_level <- c(2)
cov_base <- c(0.5, 0.8)
outcome_y <-  list( "regr", "classif")
learners <- list("ranger",  "nnet",  "regression") #
dum = c(TRUE, FALSE)

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
  validation = expand.grid(learner = learners, dummy=dum)
)

addExperiments(prob_design, algo_design, repls = repls)
summarizeExperiments()

# Test jobs -----------------------------------------------------------
testJob(id = 1)

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

resi = unwrap(ijoin(reduceResultsDataTable(), getJobPars())) 
write.csv(apply(resi,2,as.character), file = "./validate_models.csv", row.names = F)


#############################################################################
# plot results of model validation -----------------------------------------
#############################################################################
library(cowplot)
df =  read.csv("validate_models.csv") 
df = df %>%
  select(-job.id) %>%
  group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base, dummy) %>%
  summarise_all(mean)
df_regr <- df %>% filter(outcome == "regr") %>% select(-accuracy) # %>% melt(., id.vars = c("problem", "algorithm", "n", "p", "error_level","cov_base", "num_cat", "learner", "dummy", "outcome"), value.name = "rsquared")
ggplot(df_regr, aes(x = n, y = rsquared, col = cov_base)) + geom_line()+
  facet_grid(  num_cat+cov_base+error_level+ dummy ~ learner)+
  geom_hline(yintercept = 2/3, col = "black", lty = 2)


df2 = df %>% group_by(n, learner, outcome) %>% mutate(mean_rsq = mean(rsquared), sd_rsq = sd(rsquared),
                                             mean_acc = mean(accuracy), sd_acc = sd(accuracy))
p1 = ggplot(df2 %>% filter(outcome == "regr"), aes(x = n, y = mean_rsq, col = learner)) + 
  geom_line()+ 
  geom_ribbon(aes(ymin = mean_rsq - sd_rsq, ymax = mean_rsq + sd_rsq, fill = learner), alpha= 0.1, linetype = 0)+
  theme_minimal()+
  geom_hline(yintercept = 2/3, col = "black", lty = 2)+
  scale_y_continuous(limits = c(-0.15,0.75))+
  scale_x_continuous(limits = c(300,3000), breaks = c(300,1000,2000,3000))+
  theme(legend.position="none")+
  scale_color_manual(values = c( "steelblue3", "seagreen3","tomato1"))+
  scale_fill_manual(values = c( "steelblue3", "seagreen3","tomato1"))+
  ylab("R squared")+
  ggtitle("Model Validation: Regression Tasks")

p2 = ggplot(df2 %>% filter(outcome == "classif"), aes(x = n, y = mean_acc, lty = learner)) + 
  geom_line(aes(col = learner),show.legend = FALSE)+ 
  geom_ribbon(aes(ymin = mean_acc - sd_acc, ymax = mean_acc + sd_acc, fill = learner), alpha= 0.1, linetype = 0,show.legend = FALSE)+
  theme_minimal()+
  geom_hline(aes(yintercept = 0.8, lty="oracle"), col = "black")+
  scale_y_continuous(limits = c(0.5,0.83))+ 
  scale_x_continuous(limits = c(300,3000), breaks = c(300,1000,2000,3000))+
  scale_color_manual(values =c( "steelblue3", "seagreen3","tomato1"))+
  scale_fill_manual(values =c( "steelblue3", "seagreen3","tomato1"))+
  scale_linetype_manual(name = "Prediction Model", values = c("oracle"= 2,"nnet" =1, "ranger" = 1,"regression" = 1),
                        guide = guide_legend(override.aes = list(color = c("black","steelblue3", "seagreen3","tomato1"))))+
  ylab("accuracy")+
  ggtitle("Model Validation: Classification Tasks")

plot_grid(p1, p2, ncol = 2, rel_widths = c(.43, .58),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(0.02, 0.02) )

ggsave("validate_models_3.2.png", width = 10, height = 5.7)

