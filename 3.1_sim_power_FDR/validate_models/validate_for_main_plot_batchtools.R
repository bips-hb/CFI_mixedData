library(batchtools)
library(reshape2)
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(Metrics)

set.seed(2022)
# setwd("./sim_power_FDR")
# Registry ----------------------------------------------------------------
reg_name <- "sim_batchtools"
reg_dir <- file.path("registries", reg_name)
unlink(reg_dir, recursive = TRUE) # comment this line out when running final version on cluster
makeExperimentRegistry(file.dir = reg_dir, conf.file = "config.R")
#getDefaultRegistry()

# Problems -----------------------------------------------------------
source("problems_val.R") # DGP

DAG_cat_dat <- function(job, data, n,...) {
  generated_dat <-  DAG_cat(n = n, ...)
  train_instances <- sample(1:nrow(generated_dat), n*0.66, replace = F)
  list(train = generated_dat[train_instances,], 
       test = generated_dat[-train_instances,])
}

addProblem(name = "DAG_cat_data", fun = DAG_cat_dat, seed = 1)
# Algorithms -----------------------------------------------------------
# copy helper functions from "../sim_FI_comparison/algorithms.R"

make_mlr3 <- function(instance, learner){
  if (is.double(instance$train$y)){ 
    if (learner == "regression"){ 
      # print("regression learner used")
      my_learner <- lrn("regr.lm", predict_type = "response")
    }
    else {
      my_learner <- lrn(paste0("regr.", learner), predict_type = "response")}
    if (learner == "nnet"){my_learner$param_set$values = list(size = 20, decay = 0.1)}
    my_task <- as_task_regr(x = instance$train, target = "y") 
    my_measure <- msr("regr.mse")
    my_loss <- Metrics::mse
    my_loss_instance <-  Metrics::se
    my_loss_subgroup <-Metrics::mse
    # print("TEST PRINT numeric instance")
  }
  else if (is.factor(instance$train$y)){ 
    # print("TEST PRINT integer/ classification instance")
    if (learner == "regression"){
      #  print("regression learner used")
      my_learner <- lrn("classif.log_reg", predict_type = "prob")} # need to respell for regression because learner name does not match actual learner name
    else{
      my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")} # , importance = "permutation"
    if (learner == "nnet"){my_learner$param_set$values = list(size = 20, decay = 0.1)}
    # temp = instance$train %>% mutate(y = as.numeric(as.character(y)))
    my_task <-  as_task_classif(x = instance$train, target = "y") 
    my_measure <- msr("classif.logloss")
    my_loss_instance <- Metrics::ll
    my_loss <-Metrics::logLoss
    
  } else {
    print(str(instance$train))
    print("please specify prediction type" )}
  return(list("learner" = my_learner, "task" = my_task, "measure" = my_measure, "loss" = my_loss))
}
make_mlr3_dummy <- function(instance, learner){
  print("dummy encode dataset")
  # dummy encode instance
  dummy_dat = lapply(instance, function(x){dummy_cols(x[,!names(x)%in%c("y")], remove_selected_columns = TRUE, remove_first_dummy = TRUE)}) 
  dummy_dat$train$y = instance$train$y
  dummy_dat$test$y = instance$test$y
  
  # nnet does not support integer encoded dummies -> to numeric
  if (learner == "nnet"){
    dummy_dat$train =  dummy_dat$train %>% dplyr::mutate_if(is.integer,as.numeric)
    dummy_dat$test =  dummy_dat$test %>% dplyr::mutate_if(is.integer,as.numeric)
  }
  # now define task ON DUMMY DATA
  # need to define task here because of reordering in mlr3 $feature_names()
  
  if (is.double(dummy_dat$train$y)){ 
    print("TEST PRINT numeric instance")
    if (learner == "regression"){ learner = "lm"}
    my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
    if (learner == "nnet"){my_learner$param_set$values = list(size = 20, decay = 0.1)}
    # define task, measure, loss
    my_task <- as_task_regr(x = dummy_dat$train, target = "y") 
    my_measure <- msr("regr.mse")
    my_loss <- Metrics::mse
    my_loss_instance <-  Metrics::se
    my_loss_subgroup <-Metrics::mse
  }
  else if (is.factor(dummy_dat$train$y)){ 
    print("TEST PRINT integer/ classification instance")
    if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
    my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
    if (learner == "nnet"){my_learner$param_set$values = list(size = 20, decay = 0.1)}
    # define task, measure, loss
    my_task <-  as_task_classif(x = dummy_dat$train, target = "y") 
    my_measure <- msr("classif.logloss")
    my_loss_instance <- Metrics::ll
    my_loss <-Metrics::logLoss
    
    
  } else {print("please specify prediction type" )}
  # define groups of dummys that encode the same variable
  vars = unique(gsub("_.*", "",my_task$feature_names))
  level_cols <- lapply(vars, FUN = function(x){which(x == gsub("_.*", "",my_task$feature_names))})
  grps = mapply(function(vars,level_cols) { level_cols }, vars, level_cols, SIMPLIFY = FALSE,USE.NAMES = TRUE)
  # print(grps)
  
  return(list("learner" = my_learner, "task" = my_task, "measure" = my_measure, "loss" = my_loss,  "test" = dummy_dat$test))
}

validate_model = function(data, job, instance, learner, dummy_enc, ...){
  if(is.integer(instance$train$y)){
    prob = instance$test$prob
    instance = lapply(instance,function(x){ x %>% select(-prob) %>%mutate_if(is.integer,as.factor)})
  }
  if (dummy_enc == TRUE){
    print("dummy encode data")
    stopifnot(sum(sapply(instance$test[,!names(instance$test)%in%c("y")], is.factor)) > 0)
    helper = make_mlr3_dummy(instance = instance, learner = learner)
    test_dat = helper$test
  }
  else{
    helper = make_mlr3(instance = instance, learner = learner)
    test_dat = instance$test
  }
  # train model
  fit <- helper$learner$train(helper$task)
  # predict test data -> validate model
  if (helper$measure$id == "regr.mse"){
    measure = list("rsquared" = mlr3::msr("regr.rsq"))
    snr = var(test_dat$y) - 1 # that is because var(epsilon) = 1
    opt = snr / (snr + 1)
  } else{
    measure = list("accuracy" = mlr3::msr("classif.acc"))
    opt =  mean(pmax(prob, 1-prob))
  }
  res = list(fit$predict_newdata(newdata = test_dat)$score(measures = measure[[1]]))
  names(res) = names(measure[1])
  res$opt = opt
  return(res)
}

addAlgorithm(name = "validation", fun = validate_model)
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
dum = c(TRUE, FALSE)
# Experiments -----------------------------------------------------------
prob_design <- list(
  DAG_cat_data = rbind(expand.grid(n = n_main,
                                   cat_variables = binary_var_main,
                                   num_levels = categories_main,
                                   effect_size = effect_sizes_main,
                                   outcome = outcome_y_main))
)


algo_design <- list(
  validation = expand.grid(learner = learners_main, dummy_enc=dum)
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

write.csv(apply(res,2,as.character), file = "./validate_models/validate_for_main_plot_batchtools.csv")

