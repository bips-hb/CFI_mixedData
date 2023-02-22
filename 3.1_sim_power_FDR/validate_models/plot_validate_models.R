# raindrop plot
library(ggdist)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
#setwd("./knockoffs_sim_power_FDR")
df1 = read.csv("./validate_models/validate_for_sim_batchtools.csv")
df2 = read.csv("./validate_models/validate_for_main_plot_batchtools.csv")
df3 = read.csv("./validate_models/validate_for_all_categorical_batchtools.csv")
df = bind_rows(df1,df2,df3)


df = df %>% group_by(n, learner, outcome) %>% mutate(diff_rsq = (opt - rsquared ),diff_acc = (opt - accuracy )) 

df1 = df %>% filter(outcome == "regr") %>% filter(n > 450)
df1$n = as.factor(df1$n)
p1 = ggplot(df1 %>% filter(outcome == "regr") , aes(x=n, y=diff_rsq, fill =learner)) +
  scale_x_discrete(breaks = unique(df$n))+
  geom_violin(position = position_dodge(0.9))+
  theme_minimal()+
  # scale_x_discrete(breaks = c(100,500,1000,2000,3000,5000,7000), minor_breaks = c())+
  geom_hline(yintercept = 0, col = "black", lty = 2)+
  xlab("N") + 
  ylab("Deviation from optimal R squared") + 
  ggtitle("Model Validation: Regression Tasks")+
  theme(legend.position="none")+
  scale_fill_manual(name = "Prediction Model", values = c( "seagreen3","tomato1"))+
  geom_boxplot(position = position_dodge(0.9),width=0.1, aes(fill= learner), outlier.shape = NA)

df2 = df %>% filter(outcome == "classif") 
df2$n = as.factor(df2$n)
p2 = ggplot(df2 %>% filter(outcome == "classif"), aes(x=n, y=diff_acc, fill =learner)) +
  scale_x_discrete(breaks = unique(df$n))+
  geom_violin(position = position_dodge(0.9))+
  theme_minimal()+
 # scale_x_continuous(breaks = c(100,500,1000,2000,3000,5000,7000), minor_breaks = c())+
  geom_hline(yintercept = 0, col = "black", lty = 2)+
  xlab("N") + 
  ylab("Deviation from optimal accuracy") + 
  ggtitle("Model Validation: Classification Tasks")+
  scale_fill_manual(name = "Prediction Model", values = c( "seagreen3","tomato1"))+
  geom_boxplot(position = position_dodge(0.9),width=0.1, aes(fill= learner),outlier.shape = NA)


plot_grid(p1, p2, ncol = 2, rel_widths = c(.43, .58),rel_heights = c(.5, .5), labels = "AUTO", label_x = c(0.02, 0.02) )
ggsave("validate_models_3.1.png", width = 10, height = 5.7)

