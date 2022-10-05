library(ggplot2)
library(cowplot)
library(dplyr)
res = read.csv("./sim_FI_comparison/res_0808.csv") %>% dplyr::select(-space)  %>% dplyr::select(-job.id)


# single plot for paper
res_plot = res  %>% group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base) %>%dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc),
                                                                                                                            min_auc = min(auc), max_auc = max(auc))
colcol = c("#20854EFF",  "#BC3C29FF","#E18727FF", "#0072B5FF","#6F99ADFF")

auc_plot=ggplot(res_plot  %>% dplyr::filter(outcome == "regr")   %>% dplyr::filter(problem == "dgp")   %>% 
         filter(learner == "ranger")  %>%  dplyr::filter(num_cat == 5) %>% filter(cov_base == 0.8) %>%
         dplyr::filter(error_level == "2") , aes(x = n, y = mean_auc, col = algorithm )) +
  geom_point(aes(shape = algorithm), size = 4) +
  geom_ribbon(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, fill = algorithm), linetype = 0, alpha = 0.1)+
 # geom_errorbar(aes(ymin = min_auc, ymax =max_auc, col = algorithm), alpha = 0.5)+
  scale_fill_manual(values = colcol, 
                        labels = c("Conditional Subgroup", "CPI sequential", "LOCO", "PFI", "SAGE"))+
  scale_color_manual(values = colcol, 
                    labels = c("Conditional Subgroup", "CPI sequential", "LOCO", "PFI", "SAGE"))+
  scale_shape_manual(values = 1:5, 
                     labels = c("Conditional Subgroup", "CPI sequential", "LOCO", "PFI", "SAGE"))+
 # ggtitle("sd - outcome = regr, num_cat = 5, cov_base = 0.8, error_level = 2")+
  theme_minimal(base_size = 20)+ylim(c(0.6,1.035))+
  theme(legend.key.size = ggplot2::unit(2, "line"))+
  ylab("AUC")+
  xlab("Sample Size")+
  labs(fill = "Feature Importance \nMeasure", col = "Feature Importance \nMeasure",shape = "Feature Importance \nMeasure")+
  geom_line(size = 1.5)
ggsave(auc_plot, file="./sim_FI_comparison/auc.eps", device="eps")

# some additional facet plots

#res = res %>% group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base) %>% summarise_all(mean)
res_plot = res  %>% group_by(problem,algorithm, n, p, num_cat, learner, outcome,  error_level,cov_base) %>%dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc))
#res_plot <- melt(res, id.vars = c("problem", "algorithm", "n", "p", "error_level","cov_base", "num_cat", "learner", "outcome"), value.name = "auc")
colcol = c("seagreen3",  "red2","tomato1", "steelblue3","darkblue")
#res_plot = res %>% dplyr::select(sd_auc)
ggplot(res_plot  %>% dplyr::filter(outcome == "regr")   %>% dplyr::filter(problem == "dgp")   %>% #filter(learner == "regr") # %>%  dplyr::filter(num_cat == 2)
         dplyr::filter(error_level == "2") , aes(x = n, y = mean_auc, col = algorithm )) +
  facet_grid(learner ~ num_cat+cov_base+error_level+outcome) + geom_line()+
  #  geom_ribbon(aes(ymin = mean_auc-sd_auc, ymax = mean_auc+sd_auc, fill = algorithm), alpha = 0.2)+
  scale_discrete_manual(values = colcol, aesthetics = c("color"))+
  # scale_discrete_manual(values = colcol, aesthetics = c("fill"))+
  theme_bw()+ylim(c(0,1))+
  geom_line()

