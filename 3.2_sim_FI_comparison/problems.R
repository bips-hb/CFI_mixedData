#library(reticulate)
# Sys.setenv(RETICULATE_PYTHON = "/home/blesch/.local/share/r-miniconda/envs/knockoffenv/bin/python3.9")
library(mvtnorm)
library(fastDummies)
# helper functions 
quant_cut = function(to_quant_cut, num_levels){
  quantiles <- quantile(to_quant_cut, probs = seq(0, 1, length.out = num_levels + 1))
  cut(to_quant_cut, breaks = quantiles, labels = sample(LETTERS[1:num_levels]), include.lowest = TRUE)
}
internal_dummy_repr = function(to_dummy_code, effect_size_levels, num_levels){
  dummy_weights =  sapply(effect_size_levels, function(x) {seq(-x, x, length.out = num_levels)})
  as.matrix(dummy_cols(to_dummy_code, remove_selected_columns = TRUE)) %*% as.vector(dummy_weights)
}

dgp <-  function(data, job, n, p=12, num_cat = 3, error_level = 2, cov_base = 0.5, effect_factor = 3, outcome = "regr"){
  # p must be a multiple of 6
  my_mat = matrix(0, p,p)
  for (i in 1:p){
    if(i %% 2 == 1){
      my_mat[i, i+1] <- cov_base
    }else{
      my_mat[i, i-1] <- cov_base
    }
  }
  my_mat = my_mat + diag(p)
  #dat <-  data.frame(rmvnorm(n = n, sigma =  toeplitz(cov_base^(0:(p-1))))) # toeplitz structure for sigma
 # dat <-  data.frame(rmvnorm(n = n, sigma = matrix(data = rep(cov_base,p^2), nrow = p) + diag(1 - cov_base, nrow = p))) # fixed sigma
  dat <- data.frame(rmvnorm(n = n, sigma = my_mat)) # my defined matrix - pairwise corr

  # odd cols = irrelevant, even cols = relevant
  # 3 kinds of variables:
  # - continuous linear, variables 1:(p%/%3)
  # - continuous nonlinear, variables (p%/%3)+1: (2*(p%/%3))
  # - categorical, variables ((2*(p%/%3))+1=:p

  colnames(dat) <-sapply(1:ncol(dat), function(x){
    if (x %% 2 == 0 & x <= (p%/%3)){paste0("X", x, "_relevantContVar_linear")}
    else if (x %% 2 != 0 & x <= (p%/%3)){paste0("X", x, "_irrelevantContVar_linear")}
    else if(x %% 2 == 0 & (p%/%3) < x & x <= (2*(p%/%3))){paste0("X", x, "_relevantContVar_nonlinear")}
    else if (x %% 2 != 0 & (p%/%3) < x & x <= (2*(p%/%3))){paste0("X", x, "_irrelevantContVar_nonlinear")}
    else if (x%% 2 == 0 & x >(2*(p%/%3))){paste0("X", x, "_relevantCatVar")}
    else {paste0("X", x, "_irrelevantCatVar")}
  })

  # continuous nonlinear variables
  step <- dat[,((p%/%3)+1):(2*(p%/%3))] < -qnorm(0.75) |  dat[,((p%/%3)+1):(2*(p%/%3))] > qnorm(0.75)
  cont_internal <- data.frame(matrix(0, nrow = n, ncol = p%/%3, dimnames = list(NULL, colnames(dat)[((p%/%3)+1):(2*(p%/%3))])))
  cont_internal[step] <- -1
  cont_internal[!step] <- 1
  cont_internal <- cbind(dat[,1:(p%/%3)], cont_internal)

  # categorical variables
  dat[, ((2*(p%/%3))+1):p] <- as.data.frame(lapply(dat[, ((2*(p%/%3))+1):p], function(x){quant_cut(x, num_levels = num_cat)}))
  cat_internal <- apply(dat[, ((2*(p%/%3))+1):p], 2, function(x){internal_dummy_repr(to_dummy_code = x, effect_size_levels = 1, num_levels = num_cat)})
  dat_internal = cbind(cont_internal, cat_internal)

  # reorder categories to destroy ordinal character
  dat[, ((2*(p%/%3))+1):p] <- as.data.frame(lapply(dat[, ((2*(p%/%3))+1):p], function(x){factor(x,levels = LETTERS[1:num_cat],labels = LETTERS[1:num_cat])}))

  if (outcome == "regr"){
    # beta: odd columns irrelevant -> beta = 0; even columns relevant -> beta = 1
  #  betas = c(rep(c(0,1), length.out =p)) # just for debugging
   # betas = c(1,1,0,0,0,1)
   # y = as.matrix(dat_internal) %*% betas
    y = as.matrix(dat_internal) %*% (rep(c(0,1), length.out = p)*rep(c(0,1,0,effect_factor),3)) ## alternating strong and weak effect sizes
    # add noise according to SNR = var(y)/var(noise)
    # error_level = SNR
    y = y + rnorm(n, mean = 0, sd = sqrt(var(y) / error_level))
    return(cbind(y, dat))
    }
  else if (outcome == "classif"){
    # beta: odd columns irrelevant -> beta = 0; even columns relevant -> beta = 1
    # define bayes error acc. to error_level (for easier programming)
    # CAUTION higher SNR/ bayes error generates higher/lower signal, e.g.error_level = 2 gets translated to bayes_error = 0.2
    bayes_error = error_level/10
    beta_init = rep(c(0,1), length.out = p)*rep(c(0,1,0,effect_factor),3)
    # get initial bayes error rate
    #bayes_error_init <- 1 - mean(pmax(plogis(as.matrix(dat_internal) %*% beta_init), 1-plogis(as.matrix(dat_internal) %*% beta_init)))
    min_bayes_diff <- function(beta){ abs((1 - mean(pmax(plogis(as.matrix(dat_internal) %*% (beta_init*beta)), 1-plogis(as.matrix(dat_internal) %*% (beta_init*beta))))) - bayes_error)}
    beta_factor = optimize(min_bayes_diff, interval = c(0,5), tol = 0.01)$minimum # interval is fine for 0.1 < bayes_error < 0.5
   # achieved_bayes_error <- 1 - mean(pmax(plogis(as.matrix(dat_internal) %*% (beta_init*beta_factor)), 1-plogis(as.matrix(dat_internal) %*% (beta_init*beta_factor))))
    y = as.factor(rbinom(n = n, size = 1, prob = plogis(as.matrix(dat_internal) %*% (beta_init*beta_factor))))
    return(cbind(y, dat))
  }
}
#
# generated_dat <-  dgp(n=500,p=12, num_cat = 2, error_level = 2, effect_factor = 3,cov_base = 0.5, outcome = "classif")
# train_instances <- sample(1:nrow(generated_dat), nrow(generated_dat)*0.66, replace = F)
# instance = list(train = generated_dat[train_instances,],
#      test = generated_dat[-train_instances,])
# write.csv(instance$train, file = "/home/blesch/knockoffs/sim_FI_comparison/registries/train.csv", row.names = F)

#instance = lapply(instance, function(x){cbind('y' = x$y, dummy_cols(x[, !names(x) %in% c("y")], remove_selected_columns = TRUE, remove_first_dummy = TRUE))}) 
#pfi(instance = instance, learner = "regression")
#pfi_by_hand(instance = ex, learner = "ranger")
#cond_subgroup(instance = instance, learner = "ranger")
#my_cpi = cpi_seq(instance = instance, learner = "ranger")
#loco(instance = instance, learner = "ranger")
