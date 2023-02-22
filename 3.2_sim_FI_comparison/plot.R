library(ggplot2)
library(reshape2)
library(cowplot)
library(dplyr)
res = read.csv("./sim_FI_comparison/res_0808.csv") %>% dplyr::select(-space)  %>% dplyr::select(-job.id)  %>% dplyr::select(-auc) 
res2 = read.csv("./sim_FI_comparison/res_0123.csv") %>% dplyr::select(-space)  %>% dplyr::select(-job.id) %>% dplyr::select(-auc) 
res = rbind(res,res2)

# relevant & cont linear
res$relevant_cont_linear = rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("ContVar_linear", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cont linear
res$irrelevant_cont_linear = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("ContVar_linear", colnames(res)) ], na.rm=TRUE) 

# relevant & cont nonlinear
colnames(res)
res$relevant_cont_nonlinear = rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("ContVar_nonlinear", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cont nonlinear
res$irrelevant_cont_nonlinear = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("ContVar_nonlinear", colnames(res)) ], na.rm=TRUE) 

# relevant & cat
res$relevant_cat =  rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("CatVar", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cat
res$irrelevant_cat = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("CatVar", colnames(res))], na.rm=TRUE) 

res = res %>% dplyr::select(problem,algorithm, n, p, num_cat, error_level,cov_base, outcome, learner, relevant_cont_linear, irrelevant_cont_linear, relevant_cont_nonlinear, irrelevant_cont_nonlinear,relevant_cat, irrelevant_cat)
res = res %>% group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base) %>% summarise_all(mean)
res_plot <- melt(res, id.vars = c("problem", "algorithm", "n", "p", "error_level","cov_base", "num_cat", "learner", "outcome"), value.name = "rejection_rate")

colcol = c("#20854EFF", "#EE4C97FF","#7876B1FF" , "#BC3C29FF","#E18727FF", "#0072B5FF","#6F99ADFF") 

# just for an overview - full plot
ggplot(res_plot %>% filter(outcome == "regr") %>%  filter(num_cat == 2)  %>% filter(problem == "dgp")   %>% #filter(learner == "regr") 
          filter(error_level == "2") , aes(x = n, y = rejection_rate, col = algorithm )) +
  facet_grid( variable ~ learner + problem  + num_cat+cov_base+error_level+outcome)+ 
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+
  theme_minimal(base_size = 15)+
  geom_line()

###############################################
# for Figure 4 in manuscript: outcome == "regr", learner == "ranger",cov_base == 0.8, num_cat == 5
# for Figures in SI: change values accordingly
###############################################

my_outcome = "regr"   # in ["regr", "classif"]
my_learner= "ranger"  # in ["regression", "ranger", "nnet"]
my_cov_base = 0.8     # in [0.5;0.8]
my_num_cat = 5       # in [2;5]

colcol = c("#20854EFF", "#EE4C97FF","#7876B1FF" , "#BC3C29FF","#E18727FF", "#0072B5FF","#6F99ADFF") 

res_plot_cat =  res_plot[ grepl("cat", res_plot$variable),]

p1 <- ggplot(res_plot_cat %>% filter(outcome == my_outcome) 
       %>% filter(learner == my_learner)
       %>% filter(error_level == 2) 
       %>% filter(cov_base == my_cov_base)
       %>% filter(num_cat == my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
  #facet_grid( variable ~ p + num_cat  )+
  geom_line(size = 1.25)+
  theme_minimal(base_size = 15)+ theme(legend.position="none")+
  scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                        labels=c('relevant', 'irrelevant'),
                        name = "relevance")+
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+
  ggtitle("     Categorical Variables")+
  ylab("Proportion ranked top 6")+
  xlab("Sample Size")+
  scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
  ylim(c(0,1))

res_plot_cont_lin = res_plot[ grepl("cont_linear", res_plot$variable),]

p2 <- ggplot(res_plot_cont_lin %>% filter(outcome == my_outcome) 
       %>% filter(learner == my_learner)
       %>% filter(error_level == 2) 
       %>% filter(cov_base ==my_cov_base)
       %>% filter(num_cat ==my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
  scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                        labels=c('relevant', 'irrelevant'),
                        name = "relevance")+
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+ 
  #facet_grid( variable ~ p + num_cat  )+
  geom_line(size = 1.25)+
  theme_minimal(base_size = 15)+theme(legend.position="none",
                                      axis.text.y=element_blank())+
  ggtitle("Continuous Variables linear")+
  ylab("")+
  xlab("Sample Size")+
  scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
  ylim(c(0,1))

res_plot_cont_nonlin = res_plot[ grepl("cont_nonlinear", res_plot$variable),]
 p3 <- ggplot(res_plot_cont_nonlin %>% filter(outcome == my_outcome) 
              %>% filter(learner == my_learner)
              %>% filter(error_level ==2) 
              %>% filter(cov_base ==my_cov_base)
              %>% filter(num_cat ==my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
   scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                         labels=c('relevant', 'irrelevant'),
                         name = "Relevance")+
   scale_color_manual(values = colcol, 
                      labels = c("CS","CPIdeep", "CPIgauss", "CPIseq", "LOCO", "PFI", "SAGE"))+
   labs(col = "Feature Importance \nMeasure")+
   geom_line(size = 1.25)+
   theme_minimal(base_size = 15)+
   ggtitle("Continuous Variables nonlinear")+
   theme(legend.key.size = ggplot2::unit(3, "line"),
         axis.text.y=element_blank())+
   ylab("")+
   xlab("Sample Size")+
   scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
   ylim(c(0,1))

grid = plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(.3, .3, .5),rel_heights = c(.5, .5, .5), labels = "AUTO", label_x = c(0.02, 0.02,0.02) )
title_gg <- ggplot() + theme_minimal()+labs(title = paste0("outcome = ", my_outcome, ", num_cat = ", my_num_cat, ", cov_base = ", my_cov_base, ", learner = ", my_learner))
plot_grid(title_gg, grid, ncol = 1, rel_heights = c(0.15, 1))
#ggsave(grid, file="./sim_FI_comparison/trio.eps", device="eps")

###################################################
# add plot for Boruta
###################################################

res = read.csv("./sim_FI_comparison/res_0123_boruta.csv") %>% dplyr::select(-space)  %>% dplyr::select(-job.id)  %>% dplyr::select(-auc) 

# relevant & cont linear
res$relevant_cont_linear = rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("ContVar_linear", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cont linear
res$irrelevant_cont_linear = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("ContVar_linear", colnames(res)) ], na.rm=TRUE) 

# relevant & cont nonlinear
colnames(res)
res$relevant_cont_nonlinear = rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("ContVar_nonlinear", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cont nonlinear
res$irrelevant_cont_nonlinear = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("ContVar_nonlinear", colnames(res)) ], na.rm=TRUE) 

# relevant & cat
res$relevant_cat =  rowMeans(res[,grepl("_relevant", colnames(res)) & grepl("CatVar", colnames(res)) ], na.rm=TRUE) 

# irrelevant & cat
res$irrelevant_cat = rowMeans(res[,grepl("_irrelevant", colnames(res)) & grepl("CatVar", colnames(res))], na.rm=TRUE) 

res = res %>% dplyr::select(problem,algorithm, n, p, num_cat, error_level,cov_base, outcome, learner, relevant_cont_linear, irrelevant_cont_linear, relevant_cont_nonlinear, irrelevant_cont_nonlinear,relevant_cat, irrelevant_cat)
res = res %>% group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base) %>% summarise_all(mean)

res_plot <- melt(res, id.vars = c("problem", "algorithm", "n", "p", "error_level","cov_base", "num_cat", "learner", "outcome"), value.name = "rejection_rate")

# hyperparameters used
my_outcome = "regr"   # in ["regr", "classif"]
my_learner= "boruta"  # in ["regression", "ranger", "nnet"]
my_cov_base = 0.8     # in [0.5;0.8]
my_num_cat = 2       # in [2;5]

colcol = c("black") 

res_plot_cat =  res_plot[ grepl("cat", res_plot$variable),]

p1 <- ggplot(res_plot_cat %>% filter(outcome == my_outcome) 
             %>% filter(learner == my_learner)
             %>% filter(error_level == 2) 
             %>% filter(cov_base == my_cov_base)
             %>% filter(num_cat == my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
  #facet_grid( variable ~ p + num_cat  )+
  geom_line(size = 1.25)+
  theme_minimal(base_size = 15)+ theme(legend.position="none")+
  scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                        labels=c('relevant', 'irrelevant'),
                        name = "relevance")+
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+
  ggtitle("     Categorical Variables")+
  ylab("Proportion of variables detected as 'important'")+
  xlab("Sample Size")+
  scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
  ylim(c(0,1))

res_plot_cont_lin = res_plot[ grepl("cont_linear", res_plot$variable),]

p2 <- ggplot(res_plot_cont_lin %>% filter(outcome == my_outcome) 
             %>% filter(learner == my_learner)
             %>% filter(error_level == 2) 
             %>% filter(cov_base ==my_cov_base)
             %>% filter(num_cat ==my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
  scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                        labels=c('relevant', 'irrelevant'),
                        name = "relevance")+
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+ 
  #facet_grid( variable ~ p + num_cat  )+
  geom_line(size = 1.25)+
  theme_minimal(base_size = 15)+theme(legend.position="none",
                                      axis.text.y=element_blank())+
  ggtitle("Continuous Variables linear")+
  ylab("")+
  xlab("Sample Size")+
  scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
  ylim(c(0,1))

res_plot_cont_nonlin = res_plot[ grepl("cont_nonlinear", res_plot$variable),]
p3 <- ggplot(res_plot_cont_nonlin %>% filter(outcome == my_outcome) 
             %>% filter(learner == my_learner)
             %>% filter(error_level ==2) 
             %>% filter(cov_base ==my_cov_base)
             %>% filter(num_cat ==my_num_cat), aes(x = n, y = rejection_rate, col = algorithm, lty = variable )) +
  scale_discrete_manual(values = c("solid","dashed"), aesthetics = c("lty"),
                        labels=c('relevant', 'irrelevant'),
                        name = "Relevance")+
  scale_color_manual(values = colcol, 
                     labels = c("Boruta"))+
  labs(col = "Feature Importance \nMeasure")+
  geom_line(size = 1.25)+
  theme_minimal(base_size = 15)+
  ggtitle("Continuous Variables nonlinear")+
  theme(legend.key.size = ggplot2::unit(3, "line"),
        axis.text.y=element_blank())+
  ylab("")+
  xlab("Sample Size")+
  scale_x_continuous(breaks= c(300,750,1500,2000,2500,3000))+
  ylim(c(0,1))

grid = plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(.3, .3, .5),rel_heights = c(.5, .5, .5), labels = "AUTO", label_x = c(0.02, 0.02,0.02) )
title_gg <- ggplot() + theme_minimal()+labs(title = paste0("outcome = ", my_outcome, ", num_cat = ", my_num_cat, ", cov_base = ", my_cov_base, ", learner = ", my_learner))
plot_grid(title_gg, grid, ncol = 1, rel_heights = c(0.15, 1))