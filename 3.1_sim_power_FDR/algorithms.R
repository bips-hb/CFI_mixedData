#devtools::install_github("kormama1/seqknockoff")
library(ranger)
library(seqknockoff)
library(knockoff)
library(cpi)
library(mlr3)
library(mlr3learners)
library(reticulate)
# write one general CPI function
# call this function within each wrapper per knockoff sampler
# subset full factorial design later in batchtools script


# CPI function
wrapper_cpi <- function(data, job, instance, knockoffs,learner, grp = NULL,my_task, my_learner,...){
  if(is.integer(instance$test$y)){instance$test$y = as.factor(instance$test$y)}
  
  CPI_r <- cpi(task = my_task,
               learner = my_learner,
               resampling = "none",
               test_data = instance$test,
               x_tilde = knockoffs,
               test = "t", 
               groups = grp)
  print("this prints feature names")
  print(my_task$feature_names)
  print("this prints groups")
  print(grp)
  CPI_r$p.value[CPI_r$CPI == 0] <- 1.0 # if CPI == 0.000, p.value should be set to 1
  CPI_r$rejected <- (CPI_r$p.value < 0.05)*1
  if("Group" %in% colnames(CPI_r)){
    CPI_r = CPI_r %>% select(Group, rejected) %>% as.data.frame() 
    return(split(CPI_r$rejected, CPI_r$Group))
    
  } else{
    CPI_r = CPI_r %>% select(Variable, rejected) %>% as.data.frame() 
    split(CPI_r$rejected, CPI_r$Variable)}
}

# Sequential Knockoff sampler

wrapper_sequential <- function(data, job, instance, learner,...){

  instance_train <-  instance$train # just for laziness, delete later
  if (is.double(instance_train$y)){ 
    if (learner == "regression"){ learner = "lm"}
    my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
    # shuffle columns of relevant/irrelevant variables --> !!! TO DO: need to adjust this in case of grouped data
    # my_task <- as_task_regr(x = instance_train[,sample(ncol(instance_train))], target = "y") 
    my_task <- as_task_regr(x = instance_train, target = "y") 
    print("TEST PRINT numeric instance")
  }
  else if (is.integer(instance_train$y)){ 
    if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
    my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
    # shuffle columns of relevant/irrelevant variables --> !!! TO DO: need to adjust this in case of grouped data
    # my_task <-  as_task_classif(x = instance_train[,sample(ncol(instance_train))], target = "y") 
    instance_train$y <- as.factor(instance_train$y)
    my_task <-  as_task_classif(x = instance_train, target = "y") 
    print("TEST PRINT integer/ classification instance")
  } else {print("please specify prediction type" )}
  print("This prints str($test)")
  print(str(instance$test))
  knockoffs <- instance$test %>% mutate_if(is.integer,as.factor)%>% select(-"y") %>% {knockoffs_seq(.)} 
  print("This prints str(knockoffs)")
  print(str(knockoffs))
 # print(head(knockoffs))
  wrapper_cpi(data = data, job = job, instance = instance, knockoffs = knockoffs,my_task=my_task, my_learner=my_learner, ...)
}

# Gaussian Knockoff sampler

wrapper_gaussian <- function(data, job, instance, learner,...){

 # detect groups/ factors to dummy encode
  if("factor" %in% sapply(instance$test, class)){
    print("dummy encode dataset")
    # dummy encode instance
    dummy_dat = lapply(instance, function(x){dummy_cols(x, remove_selected_columns = TRUE, remove_first_dummy = TRUE)}) 
  
    # now define task ON DUMMY DATA
    # need to define task here because of reordering in mlr3 $feature_names()
    
    if (is.double(dummy_dat$train$y)){ 
      if (learner == "regression"){ learner = "lm"}
      my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <- as_task_regr(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      my_task <- as_task_regr(x = dummy_dat$train, target = "y") 
      print("TEST PRINT numeric instance")
    }
    else if (is.integer(dummy_dat$train$y)){ 
      if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
      my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <-  as_task_classif(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      dummy_dat$train$y <- as.factor(dummy_dat$train$y)
      my_task <-  as_task_classif(x = dummy_dat$train, target = "y") 
      print("TEST PRINT integer/ classification instance")
    } else {print("please specify prediction type" )}
    
     vars = unique(gsub("_.*", "",my_task$feature_names))
     level_cols <- lapply(vars, FUN = function(x){which(x == gsub("_.*", "",my_task$feature_names))})
     grps = mapply(function(vars,level_cols) { level_cols }, vars, level_cols, SIMPLIFY = FALSE,USE.NAMES = TRUE)
    print(grps)
    print("This prints str($test)")
    print(str(instance$test))
    knockoffs <- dummy_dat$test %>%  select(-"y") %>% {create.second_order(as.matrix(.))} %>% as.data.frame()# CAUTION: avoid chr matrix
    print("This prints str(knockoffs)")
    print(str(knockoffs))
  #  print(head(knockoffs))
    wrapper_cpi(data = data, job = job, instance = dummy_dat, knockoffs = knockoffs, grp = grps, my_task=my_task, my_learner=my_learner,...)
  }
  else{
    print("no need to dummy encode")
    # need to define task here because of reordering in  mlr3 $feature_names()
    instance_train <-  instance$train 
    if (is.double(instance_train$y)){ 
      if (learner == "regression"){ learner = "lm"}
      my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <- as_task_regr(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      my_task <- as_task_regr(x = instance_train, target = "y") 
      print("TEST PRINT numeric instance")
    }
    else if (is.integer(instance_train$y)){ 
      if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
      my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
      # shuffle columns of relevant/irrelevant variables
      # my_task <-  as_task_classif(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      instance_train$y <- as.factor(instance_train$y)
      my_task <-  as_task_classif(x = instance_train, target = "y") 
      print("TEST PRINT integer/ classification instance")
    } else {print("please specify prediction type" )}
    print("This prints str($test)")
    print(str(instance$test))
    knockoffs <- instance$test %>%  select(-"y") %>% {create.second_order(as.matrix(.))} %>% as.data.frame() # CAUTION: avoid chr matrix
    # print(knockoffs)
    print("This prints str(knockoffs)")
    print(str(knockoffs))
    wrapper_cpi(data = data, job = job, instance = instance, knockoffs = knockoffs, my_task =my_task, my_learner = my_learner, ...)
  }

}

# Deep Knockoff Sampler

wrapper_deep <- function(data, job, instance, learner,...){
  
  # detect groups/ factors to dummy encode
  if("factor" %in% sapply(instance$test, class)){
    print("dummy encode dataset")
    # dummy encode instance
    dummy_dat = lapply(instance, function(x){dummy_cols(x, remove_selected_columns = TRUE, remove_first_dummy = TRUE)}) 
    
    # now define task ON DUMMY DATA
    # need to define task here because of reordering in mlr3 $feature_names()
    
    if (is.double(dummy_dat$train$y)){ 
      if (learner == "regression"){ learner = "lm"}
      my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <- as_task_regr(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      my_task <- as_task_regr(x = dummy_dat$train, target = "y") 
      print("TEST PRINT numeric instance")
    }
    else if (is.integer(dummy_dat$train$y)){ 
      if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
      my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <-  as_task_classif(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      dummy_dat$train$y <- as.factor(dummy_dat$train$y)
      my_task <-  as_task_classif(x = dummy_dat$train, target = "y") 
      print("TEST PRINT integer/ classification instance")
    } else {print("please specify prediction type" )}
    
    vars = unique(gsub("_.*", "",my_task$feature_names))
    level_cols <- lapply(vars, FUN = function(x){which(x == gsub("_.*", "",my_task$feature_names))})
    grps = mapply(function(vars,level_cols) { level_cols }, vars, level_cols, SIMPLIFY = FALSE,USE.NAMES = TRUE)
    print(grps)
    print("This prints str(dummy_dat$test)")
    print(str(dummy_dat$test))
    # source relevant function 
    source_python('deep_knockoffs.py', envir = globalenv())
    knockoffs <- dummy_dat$test %>%  mutate_if(is.integer,as.numeric) %>% select(-"y") %>%{generate_deep_knockoffs(.)} %>% as.data.frame()
    print("This prints str(knockoffs)")
    print(str(knockoffs))
    #  print(head(knockoffs))
    wrapper_cpi(data = data, job = job, instance = dummy_dat, knockoffs = knockoffs, grp = grps, my_task=my_task, my_learner=my_learner,...)
  }
  else{
    print("no need to dummy encode")
    # need to define task here because of reordering in  mlr3 $feature_names()
    instance_train <-  instance$train 
    if (is.double(instance_train$y)){ 
      if (learner == "regression"){ learner = "lm"}
      my_learner <- lrn(paste0("regr.", learner), predict_type = "response")
      # shuffle columns of relevant/irrelevant variables --> !!! need to adjust this in case of grouped data
      # my_task <- as_task_regr(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      my_task <- as_task_regr(x = instance_train, target = "y") 
      print("TEST PRINT numeric instance")
    }
    else if (is.integer(instance_train$y)){ 
      if (learner == "regression"){ learner = "log_reg"} # need to respell for regression because learner name does not match actual learner name
      my_learner <- lrn(paste0("classif.", learner), predict_type = "prob")
      # shuffle columns of relevant/irrelevant variables
      # my_task <-  as_task_classif(x = instance_train[,sample(ncol(instance_train))], target = "y") 
      instance_train$y <- as.factor(instance_train$y)
      my_task <-  as_task_classif(x = instance_train, target = "y") 
      print("TEST PRINT integer/ classification instance")
    } else {print("please specify prediction type" )}
    print("This prints str($test)")
    print(str(instance$test))
    # source relevant function 
    source_python('deep_knockoffs.py', envir = globalenv())
    knockoffs <- instance$test %>%  mutate_if(is.integer,as.numeric) %>% select(-"y") %>%{generate_deep_knockoffs(.)} %>% as.data.frame()
    # print(knockoffs)
    print("This prints str(knockoffs)")
    print(str(knockoffs))
    wrapper_cpi(data = data, job = job, instance = instance, knockoffs = knockoffs, my_task =my_task, my_learner = my_learner, ...)
  }
  
}

#wrapper_deep(instance = ex2, learner = "ranger")
