library(cpi)
library(mlr3)
library(mlr)
library(mlr3learners)
library(ggplot2)
library(reshape2)
library(gridExtra)
set.seed(2022)
# Illustrate the difference between marginal and conditional variable importance measures

# Suppose data being generated according to the DAG:
#
# X1 --> X2 --> Y
#
# Analyze the importance scores attributed to X1, X2 by 
# (I) marginal VI measure: Permutation Feature importance (PFI)
# (II) conditional VI measure: Contitional Predictive impact (CPI)

sim = 1000
PFI = matrix(nrow = sim, ncol = 2, byrow = T, dimnames = list(NULL, c("X1", "X2")))
CPI = matrix(nrow = sim, ncol = 2, byrow = T, dimnames = list(NULL, c("X1", "X2")))

for (i in 1:sim){
  
  # Generate Data
  n = 100
  X1 = rnorm(n, mean = 0, sd = 1)
  X2 = 1+ X1 + rnorm(n, mean = 0, sd = 1)
  Y = 1 + X2 + rnorm(n, mean = 0, sd = 1)
  
  dat = data.frame(cbind(X1,X2,Y))
  
  # fit a predition model
  my_task <- as_task_regr(x = dat, target = "Y")
  my_learner <- lrn('regr.ranger', predict_type = "response")
  my_task_mlr <- makeRegrTask(data = dat, target = "Y")
  my_learner_mlr <- makeLearner('regr.ranger', predict.type = "response")
  # CPI
  CPI[i,] = cpi(task = my_task,
      learner = my_learner, 
      resampling = rsmp("holdout"))$CPI
  
  # PFI
  PFI[i,] = unlist(generateFeatureImportanceData(
    task = my_task_mlr,
    method = "permutation.importance",
    learner = my_learner_mlr,
    features = getTaskFeatureNames(my_task_mlr))$res)
  
}


# CPI vs PFI within same plot

# all in one data frame
CPI_melted = melt(CPI)
CPI_melted$Var1 = "CPI"
PFI_melted = melt(PFI)
PFI_melted$Var1 = "PFI"

CPI_PFI = rbind(CPI_melted, PFI_melted)
#write.csv(CPI_PFI, file = "./sim_marginal_vs_conditional/res.csv")
ggplot(data = CPI_PFI) + 
  ylim(min(CPI[,1]),max(PFI[,2]))+
  geom_boxplot(aes( x = Var2, y = value, fill = Var1)) +
  geom_hline(aes(yintercept = 0, lty = 2), linetype = 'dotted') + 
  xlab("") + 
  ylab("FI Score")+
  guides(color=guide_legend("FI Metric"))+
  scale_color_manual(values = c("#BC3C29FF","#0072B5FF"),aesthetics = c("colour", "fill"))+
  theme_minimal(base_size = 20)

