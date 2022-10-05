library(dplyr)
library(fastDummies)
# Data Generating Processes 

# DAG Data: 
# all edges are directed from top to bottom (e.g. x1 -> x4)
#
#       X1  X2
#         \ /
#         / \
#       X3  X4
#         \ /
#          Y

##################################
# DAG categorical data 
##################################

# helper functions 
quant_cut = function(to_quant_cut, num_levels){
  quantiles <- quantile(to_quant_cut, probs = seq(0, 1, length.out = num_levels + 1))
  cut(to_quant_cut, breaks = quantiles, labels = sample(LETTERS[1:num_levels]), include.lowest = TRUE)
}
internal_dummy_repr = function(to_dummy_code, effect_size_levels, num_levels){
  dummy_weights =  sapply(effect_size_levels, function(x) {seq(-x, x, length.out = num_levels)})
  as.matrix(dummy_cols(to_dummy_code, remove_selected_columns = TRUE)) %*% as.vector(dummy_weights)
}

# # actual function
# e.g. some defaults: DAG_cat(n = 10, effect_size = c("x1x4" = 0.5, "x2x3" = 0.5, "x3y" = 0.5, "x4y" = 0.5),
#                outcome = "regr", cat_variables = c("X1","X3"), num_levels = 5)

DAG_cat = function(n, effect_size,outcome, cat_variables, num_levels){
  # x1
  if("X1" %in% cat_variables){
    x1.irrelevant = quant_cut(rnorm(n, mean = 0, sd = 1), num_levels=num_levels)
    x1.irrelevant_internal = internal_dummy_repr(to_dummy_code = x1.irrelevant,effect_size_levels = effect_size["x1x4"], num_levels=num_levels )
    x1.irrelevant = factor(x1.irrelevant,levels = LETTERS[1:num_levels],labels = LETTERS[1:num_levels]) # destroy ordinal character
    }
  else {
    x1.irrelevant =  rnorm(n, mean = 0, sd = 1)
    x1.irrelevant_internal = x1.irrelevant * effect_size["x1x4"] 
    }
  # x2
  if("X2" %in% cat_variables){
    x2.irrelevant = quant_cut(rnorm(n, mean = 0, sd = 1), num_levels=num_levels)
    x2.irrelevant_internal = internal_dummy_repr(to_dummy_code = x2.irrelevant,effect_size_levels = effect_size["x2x3"],num_levels=num_levels )
    x2.irrelevant = factor(x2.irrelevant,levels = LETTERS[1:num_levels],labels = LETTERS[1:num_levels]) # destroy ordinal character
  }
  else {
    x2.irrelevant =  rnorm(n, mean = 0, sd = 1)
    x2.irrelevant_internal = x2.irrelevant * effect_size["x2x3"]
    }
  # x3
  if("X3" %in% cat_variables){
    x3.relevant = quant_cut(x2.irrelevant_internal+ rnorm(n, mean = 0, sd = 1), num_levels=num_levels)
    x3.relevant_internal = internal_dummy_repr(to_dummy_code = x3.relevant,effect_size_levels = effect_size["x3y"],num_levels=num_levels )
    x3.relevant = factor(x3.relevant,levels = LETTERS[1:num_levels],labels = LETTERS[1:num_levels]) # destroy ordinal character
    }
  else{
    x3.relevant = x2.irrelevant_internal  + rnorm(n, mean = 0, sd = 1)
    x3.relevant_internal =x3.relevant * effect_size["x3y"]
    }
  # x4
  if("X4" %in% cat_variables){
    x4.relevant = quant_cut(x1.irrelevant_internal+rnorm(n, mean = 0, sd = 1) ,num_levels=num_levels)
    x4.relevant_internal = internal_dummy_repr(to_dummy_code = x4.relevant,effect_size_levels = effect_size["x4y"], num_levels=num_levels )
    x4.relevant = factor(x4.relevant,levels = LETTERS[1:num_levels],labels = LETTERS[1:num_levels]) # destroy ordinal character
  }
  else{
    x4.relevant = x1.irrelevant_internal + rnorm(n, mean = 0, sd = 1)
    x4.relevant_internal = x4.relevant * effect_size["x4y"] 
    }
  if(outcome == "regr"){ y = x3.relevant_internal + x4.relevant_internal + rnorm(n, mean = 0, sd = 1)}
  if(outcome == "classif") {
    prob = plogis(x3.relevant_internal - x4.relevant_internal)
    y = rbinom(n = n, 1, prob = prob)
    #  print(prob)
  }
  data.frame(x1.irrelevant,x2.irrelevant,x3.relevant, x4.relevant, y)
}

