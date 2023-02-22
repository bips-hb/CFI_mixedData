library(cpi)
library(mlr3)
library(mlr3learners)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(seqknockoff)
set.seed(2023)

# Illustrate the difference between marginal and conditional variable importance measures

# Example: smoking (S) leads to increased use of chewing gum (G), but also to darker teeth (Y)
# Suppose data being generated according to:
#
# G <-- S --> Y
#
# (Note: in text we have renamed S and G to C and X for being more general)
# i.e. we have confounder (C) causes both covariate (X) and outcome (Y)  
# X <-- C --> Y

# Analyze the importance scores attributed to G, S by 
# (I) marginal VI measure: Permutation Feature importance (PFI)
# (II) conditional VI measure: Contitional Predictive impact (CPI)


sim = 1000

CPI = data.frame()
PFI = data.frame()
for (i in 1:sim){
  
  # Generate Data
  n = 200
  S = rbinom(n, size = 1, prob = 0.5)
  G = S + rnorm(n, mean = 0, sd = 1)
  Y = S + rnorm(n, mean = 0, sd = 1)
  
  dat = data.frame(cbind(G,S,Y))
  dat$S = as.factor(dat$S)
  idx = sample(1:nrow(dat), nrow(dat)*0.5, replace = F)
  train = dat[idx, ]
  test = dat[-idx,]
  t2 = data.frame("A" = rnorm(nrow(test)), test[,!names(dat)%in%c("Y")], row.names = NULL)
  kn = knockoffs_seq(t2)[,names(t2)%in%c("G", "S")]
  
  # fit a predition model
  my_task <- as_task_regr(x = train, target = "Y")
  my_learner <- lrn('regr.ranger', predict_type = "response")

  # # CPI
  # CPI[i,] = cpi(task = my_task,
  #               learner = my_learner, 
  #               test_data = test,
  #               resampling = rsmp("holdout"),
  #               x_tilde = kn)$CPI
  
  # CPI
  c = cpi(task = my_task,
          learner = my_learner, 
          test_data = test,
          x_tilde = kn)
  CPI = rbind(CPI, data.frame(split(c$CPI, c$Variable)))
  
  # PFI
  fit <- my_learner$train(my_task)
  pred = iml::Predictor$new(fit, data = test, y = "Y")
  
 # PFI[i,] = 
  p = iml::FeatureImp$new(predictor = pred, loss = Metrics::mse, compare = "ratio", n.repetitions = 20)$results
  PFI = rbind(PFI, data.frame(split(p$importance, p$feature)))
}


# CPI vs PFI within same plot

# all in one data frame
CPI_melted = melt(CPI)
CPI_melted$Var1 = "CPI"
PFI_melted = melt(PFI)
PFI_melted$Var1 = "PFI"

CPI_PFI = rbind(CPI_melted, PFI_melted)
write.csv(CPI_PFI, file = "./knockoffs/sim_marginal_vs_conditional/res_figure_1.csv")
ggplot(data = CPI_PFI) + 
  ylim(min(CPI[,1]),max(PFI[,2]))+
  geom_boxplot(aes( x = variable, y = value, fill = Var1)) +
  geom_hline(aes(yintercept = 0, lty = 2), linetype = 'dotted') + 
  xlab("") + 
  ylab("FI Score")+
  guides(color=guide_legend("FI Metric"))+
  scale_color_manual(values = c("#BC3C29FF","#0072B5FF"),aesthetics = c("colour", "fill"))+
  theme_minimal(base_size = 20)

