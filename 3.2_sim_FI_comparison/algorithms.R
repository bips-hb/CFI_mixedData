# script to define various functions to compare: CPI + seq KO, LOCO, Cond subgroup
library(dplyr)
library(seqknockoff)
library(cpi)
library(mlr3)
library(mlr3learners)
library(fastDummies)
library(knockoff)
library(cs)
library(iml)
library(pROC)
library(Boruta)
#use_condaenv("renv_sage")
#Sys.setenv(RETICULATE_PYTHON = "/home/blesch/miniconda3/envs/renv_sage/bin/python.exe")
#library(reticulate)
#reticulate::use_miniconda("senv", required = T)
#reticulate::use_virtualenv("senv", required = T)
#reticulate::use_condaenv("senv", required = T)
#Sys.unsetenv("RETICULATE_PYTHON")
#use_virtualenv("venv", required = TRUE)  
#library(mlr3keras)

# for deep knockoffs generation:
#use_condaenv("knockoffenv_local") # switch to knockoffenv for workstation setup 
#use_condaenv("knockoffenv") # switch to knockoffenv for workstation setup 
library(reticulate)
source_python('../sim_power_FDR/deep_knockoffs.py', envir = globalenv())
#RETICULATE_PYTHON  = "/home/blesch/.local/share/r-miniconda/envs/mlr3keras/bin/python3.8"
# define loss function
my_logloss <- function(actual, predicted){
  if(is.factor(actual)){
    actual =  as.numeric(as.character(actual))
  }
  return(Metrics::logLoss(actual = actual, predicted = predicted)*(-1)) # !!!! CAUTION -1 justified by trial and error only
  # check up mix up in cond subgroup for non -1 multiplication!!! 
}


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
    my_loss_subgroup <-my_logloss
  } else {
    print(str(instance$train))
    print("please specify prediction type" )}
   return(list("learner" = my_learner, "task" = my_task, "measure" = my_measure, "loss" = my_loss, "loss_instance"= my_loss_instance,
               "loss_subgroup" = my_loss_subgroup,
               "true_importances" =data.frame("Variable" = colnames(instance$train)[!names(instance$test) %in% c("y")], 
                                              "true" = ifelse(grepl("_relevant",colnames(instance$train)[!names(instance$test) %in% c("y")]), 1,0))))
  }

cpi_seq <- function(data, job, instance, learner, ...){
  my_mlr3 <- make_mlr3(instance = instance, learner = learner)
  CPI_r <- cpi(task = my_mlr3$task,
               learner = my_mlr3$learner,
               resampling = "none",
               log = FALSE, 
               test_data = instance$test,
               knockoff_fun = seqknockoff::knockoffs_seq,
               test = "t")
  res = merge(x = my_mlr3$true_importances, y = CPI_r, by.x = "Variable", by.y = "Variable")
  auc <- pROC::auc(res$true, res$CPI)
  CPI_r$p.value[CPI_r$CPI == 0] <- 1.0 # if CPI == 0.000, p.value should be set to 1
  CPI_r$rejected <- (CPI_r$p.value < 0.05)*1
  CPI_r = CPI_r %>% dplyr::select(Variable, rejected, p.value,CPI) %>% as.data.frame()
  CPI_r$rank = rank(-CPI_r$CPI)
  CPI_r$top_p = CPI_r$rank <= length(CPI_r$rank) / 2
  res_list = split(CPI_r$top_p*1, CPI_r$Variable)
  res_list[['auc']] = auc
  return(res_list)
}
#true_importances = data.frame("Variable" = colnames(instance$test)[-1], "true" = c(rep(c(0,1),6)))
loco <- function(data,job,  instance, learner, ...){
  # full model 
  full_model <- make_mlr3(instance = instance, learner = learner)
  full_fit <- full_model$learner$train(full_model$task)
  full_pred <-  full_model$learner$predict_newdata(newdata = instance$test)
  full_loss <-  full_pred$score(measures = full_model$measure)
  # leave one out
  # modify task:
  loco_res <-  sapply(2:ncol(instance$train), function(i) {
    red_model <- make_mlr3(instance = lapply(instance, function(x){x[,-i]}), learner = learner)
    red_fit <- red_model$learner$train(red_model$task)
    red_pred <- red_model$learner$predict_newdata(newdata = instance$test[,-i])                 
    red_loss <- red_pred$score(measures = red_model$measure)
    delta <- red_loss - full_loss
    list(i = i, delta = delta, Variable =  colnames(instance$train)[i]) # CAUTION -- might double check reordering in mlr3 of feature names if not X1--X2--X3 order
  })
  res = as.data.frame(t(loco_res))
  res = merge(x = full_model$true_importances, y = res, by.x = "Variable", by.y = "Variable")
  auc <- pROC::auc(res$true, as.numeric(res$delta))
  res$rank = rank(-unlist(res$delta)) # Double check ranking -- lower / lower rank
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, unlist(res$Variable))
  res_list[['auc']] = auc
  return(res_list)
}

cond_subgroup <- function(data, job, instance, learner, ...){
  model <- make_mlr3(instance = instance, learner = learner)
  fit <- model$learner$train(model$task)
  #pred <-  model$learner$predict_newdata(newdata = instance$test)
  pred = Predictor$new(fit, data = instance$test, y = "y")
  # 1. fit tree
  feature_names = setdiff(colnames(instance$train), "y")
  my_ctrl = partykit::ctree_control(maxdepth = 3, minbucket = 30)
  my_conds = fit_conditionals(data = instance$train[, feature_names], ctrl = my_ctrl, type = "trtf")
  # 2. caclulate cPFI 
 # res = grouped_pfi(pred = pred, loss = model$loss_instance , conds = my_conds, repetitions = 5) # it's not the instance wise loss but just for 
  res = grouped_pfi(pred = pred, loss = model$loss_subgroup , conds = my_conds, repetitions = 5) #special loss for cond subgroup approach -- double check! 
  # double checking the reverse issue with cond subgroupt in classification using AUC
  res = merge(x = model$true_importances, y = res, by.x = "Variable", by.y = "feature")
  auc <- pROC::auc(res$true, res$importance)
  res$rank = rank(-res$importance)
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, res$Variable)
  res_list[['auc']] = auc
  return(res_list)
}

pfi <- function(data,job,  instance, learner, ...){
  model <- make_mlr3(instance = instance, learner = learner)
  fit <- model$learner$train(model$task)
  if (is.factor(instance$train$y)){
    num_y = as.numeric(as.character(instance$test$y))
    pred = iml::Predictor$new(fit, data = instance$test[,-num_y], y =num_y, type = "prob", class = "1") # need to pass y as numeric, see reported bug https://github.com/christophM/iml/issues/134
  } else if (is.double(instance$train$y)){ 
    pred = iml::Predictor$new(fit, data = instance$test, y = "y")
  }
  res = iml::FeatureImp$new(predictor = pred, loss = model$loss, compare = "difference", n.repetitions = 5)$results 
  res = merge(x = model$true_importances, y = res, by.x = "Variable", by.y = "feature")
  auc <- pROC::auc(res$true, res$importance)
  res$rank = rank(-res$importance)
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, res$Variable)
  res_list[['auc']] = auc
  return(res_list)
}

debugger <- function(data, job, instance, learner, reps = 5, ...){
  PFI = matrix(nrow = reps, ncol = 12, byrow = T, dimnames = list(NULL, sort(colnames(instance$train)[-1])))
  CPI = matrix(nrow = reps, ncol = 12, byrow = T, dimnames = list(NULL, sort(colnames(instance$train)[-1])))
  for (i in 1:reps) {
    res = pfi(instance = instance, learner = learner)
    PFI[i,] = unlist(split(res$importance, res$feature))#
    res_cpi = cpi_seq(instance = instance, learner = learner)
    CPI[i,] = unlist(split(res_cpi$CPI, res_cpi$Variable))#
  }
  print("prints CPI")
  print(CPI)
  print("prints PFI")
  print(PFI)
  # all in one data frame
  CPI_melted = melt(CPI)
  print("prints CPI melted")
  print(CPI_melted)
  CPI_melted$Var1 = "CPI"
  PFI_melted = melt(PFI)
  PFI_melted$Var1 = "PFI"
  print("prints PFI melted")
  print(PFI_melted)
  CPI_PFI = rbind(CPI_melted, PFI_melted)
  ggplot(data = CPI_PFI) + 
    #  ylim(min(CPI[,1]),max(PFI[,2]))+
    ylim(min(CPI), max(PFI))+
    geom_boxplot(aes( x = Var2, y = value, fill = Var1)) +
    geom_hline(aes(yintercept = 0, lty = 2), linetype = 'dotted') + 
    scale_x_discrete(guide = guide_axis(n.dodge=2))+
    xlab("") + 
    ylab("FI Score")+
    guides(color=guide_legend("FI Metric"))+
    scale_color_manual(values = c("#1763AA", "lightskyblue"),aesthetics = c("colour", "fill"))
}
#debugger(instance = instance, learner = "ranger")
# 
# some notes about logLoss
#mlr3measures::logloss(truth = as.factor(c(1,0)), 
#                      prob = matrix(data = c(0.5,0.5, 0.5,0.5), nrow =2, ncol = 2, dimnames  = list(c("1", "0"), c("1",0))))
#Metrics::logLoss(actual = c(1,0), predicted = c(0.5,0.5) )


source("SAGE_R_efficient.R")
SAGE <- function(data, job, instance, learner, ...){
  model <- make_mlr3(instance = instance, learner = learner)
  fit <- model$learner$train(model$task)
  res <- data.frame("importance" = sage(model = fit, data = instance$test, target = "y", loss = model$loss_instance))
  res$feature <- rownames(res)
  res = merge(x = model$true_importances, y = res, by.x = "Variable", by.y = "feature")
  auc <- pROC::auc(res$true, res$importance)
  res$rank = rank(-res$importance)
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, res$Variable)
  res_list[['auc']] = auc
  return(res_list)
}

####################################
# add CPI gaussian and CPI deep
# need to dummy encode data set 
####################################

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
    my_loss_subgroup <-my_logloss

  } else {print("please specify prediction type" )}
  # define groups of dummys that encode the same variable
  vars = unique(gsub("_.*", "",my_task$feature_names))
  level_cols <- lapply(vars, FUN = function(x){which(x == gsub("_.*", "",my_task$feature_names))})
  grps = mapply(function(vars,level_cols) { level_cols }, vars, level_cols, SIMPLIFY = FALSE,USE.NAMES = TRUE)
 # print(grps)

  return(list("learner" = my_learner, "task" = my_task, "measure" = my_measure, "loss" = my_loss, "loss_instance"= my_loss_instance,
              "loss_subgroup" = my_loss_subgroup,
              "true_importances" =data.frame("Variable" = colnames(instance$train)[!names(instance$test) %in% c("y")], 
                                             "true" = ifelse(grepl("_relevant",colnames(instance$train)[!names(instance$test) %in% c("y")]), 1,0)),
              "groups" = grps, "test" = dummy_dat$test))
}


cpi_gauss <- function(data, job, instance, learner, ...){
  my_mlr3_dummy <- make_mlr3_dummy(instance = instance, learner = learner)
  # generate knockoffs
  knockoffs <- my_mlr3_dummy$test %>%  select(-"y") %>% {create.second_order(as.matrix(.))} %>% as.data.frame()# CAUTION: avoid chr matrix
  print("This prints str(knockoffs)")
  print(str(knockoffs))
  
  CPI_r <- cpi(task = my_mlr3_dummy$task,
               learner = my_mlr3_dummy$learner,
               resampling = "none",
               log = FALSE, 
               test_data = my_mlr3_dummy$test,
               x_tilde = knockoffs,
               test = "t",
               groups = my_mlr3_dummy$groups)

  my_mlr3_dummy$true_importances$Group = gsub("_.*", "",my_mlr3_dummy$true_importances$Variable)
  res = merge(x = my_mlr3_dummy$true_importances, y = CPI_r, by.x = "Group", by.y = "Group")
  auc <- pROC::auc(res$true, res$CPI)
  res$p.value[res$CPI == 0] <- 1.0 # if CPI == 0.000, p.value should be set to 1
  res$rejected <- (res$p.value < 0.05)*1
  res = res %>% dplyr::select(Variable, Group,rejected, p.value,CPI) %>% as.data.frame()
  res$rank = rank(-res$CPI)
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, res$Variable)
  res_list[['auc']] = auc
  return(res_list)
}

cpi_deep <- function(data, job, instance, learner, ...){
  my_mlr3_dummy <- make_mlr3_dummy(instance = instance, learner = learner)
  # generate knockoffs
  knockoffs <-  my_mlr3_dummy$test %>%  mutate_if(is.integer,as.numeric) %>% select(-"y") %>%{generate_deep_knockoffs(.)} %>% as.data.frame()
  print("This prints str(knockoffs)")
  print(str(knockoffs))
  
  CPI_r <- cpi(task = my_mlr3_dummy$task,
               learner = my_mlr3_dummy$learner,
               resampling = "none",
               log = FALSE, 
               test_data = my_mlr3_dummy$test,
               x_tilde = knockoffs,
               test = "t",
               groups = my_mlr3_dummy$groups)
  
  my_mlr3_dummy$true_importances$Group = gsub("_.*", "",my_mlr3_dummy$true_importances$Variable)
  res = merge(x = my_mlr3_dummy$true_importances, y = CPI_r, by.x = "Group", by.y = "Group")
  auc <- pROC::auc(res$true, res$CPI)
  res$p.value[res$CPI == 0] <- 1.0 # if CPI == 0.000, p.value should be set to 1
  res$rejected <- (res$p.value < 0.05)*1
  res = res %>% dplyr::select(Variable, Group,rejected, p.value,CPI) %>% as.data.frame()
  res$rank = rank(-res$CPI)
  res$top_p = res$rank <= length(res$rank) / 2
  res_list = split(res$top_p*1, res$Variable)
  res_list[['auc']] = auc
  return(res_list)
}

###########################
# add Boruta
###########################

boruta = function(data, job, instance, learner, ...){
  
  res = TentativeRoughFix(Boruta(y ~ . , data = instance$train, num.threads = 1))$finalDecision  
  # not clear whether to use train or test sample here 
  # other methods use train to fit model, but test to determine feature importance; 
  # Boruta does everything on the same data set 
  res_list = as.list(1*(res == "Confirmed"))
  names(res_list) = names(res)
  res_list[['auc']] = NA
  return(res_list)
}