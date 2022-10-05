# library(mlr3) #not relevant for sage function
# library(mlr3learners) #not relevant for sage function
# library(Metrics) #not relevant for sage function

#library(doParallel)
#cl <- makeCluster(5)
#registerDoParallel(cl) 

# -------------------------
# Implement SAGE in R
# -------------------------

# DISCLAIMER
# marginal imputation only
# hard coded: minimum coalition = empty set, maximum coalition = all features
# no early stopping in case of convergence of SAGE values phi -- using all instances instead of approx. sampling 

#--------------------------
# helper function : Marginal imputer
#--------------------------
marginal_imputer <-  function(model, x, S, ins){
  x[, S] = ins[! names(ins) %in% c("y")][S] 
  # if regression task - average predictions
  # if classification task - average probs 
  pred = ifelse(is.factor(ins$y),
                model$predict_newdata(newdata = x)$prob[,"1"],
                model$predict_newdata(newdata = x)$response)
  return(mean(pred))
}

#--------------------------
# SAGE
#--------------------------
# @param model: fitted mlr3 model
# @param data: data.frame() of test data to calculate SAGE on
# @param loss: loss function of form function(actual, predicted), e.g. Metrics::mse

sage <- function(model, data, target = "y", loss = Metrics::mse){
  data_x = data[, !names(data) %in% target]
  
  sage_values <- foreach(i = 1:nrow(data), .export = c("marginal_imputer"), .combine = rbind) %dopar% { 
    # initialize SAGE values phi
    phi <- c()
    # select an instance
    ins <- data[i,]
    # sample coalition setup D; subset S of D builds the actual coalition
    perm <- sample(1:ncol(data_x), ncol(data_x))
    # calculate initial loss - S = empty set
    S = logical(length(perm))
    loss_prev <- loss(actual = ins$y, predicted = marginal_imputer(model = model, x = data_x, S = S, ins = ins))
    
    for(d in perm){
      # add feature d to coalition
      S[d] = TRUE
      # impute values of variables not in S 
      y_hat_mean = marginal_imputer(model = model, x = data_x, S = S, ins = ins)
      loss_S = loss(actual = ins$y, predicted = y_hat_mean)
      delta = loss_prev - loss_S
      loss_prev = loss_S
      # save importance values phi
      phi[d] = delta
    }
    c(unlist(split(phi, colnames(data_x))))
  }
  
  return(colMeans(sage_values))
}

# Example
 df = read.csv("/home/blesch/knockoffs/sim_FI_comparison/registries/train.csv")
# df$y = factor(as.character(df$y))
train = df[2000:3600,1:9]
test = df[3601:5000,1:9]
learner = lrn("regr.lm", predict_type = "response")
task = as_task_regr(x = train, target = "y")
# learner = lrn("classif.ranger", predict_type = "prob")
# task = as_task_classif(x = train, target = "y")
 model = learner$train(task)
 start.time <- Sys.time()
 res = sage(model = model, data = test, target = "y", loss = Metrics::mse)
 res
 end.time <- Sys.time()
end.time - start.time


