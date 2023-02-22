library(ggplot2)
library(dplyr)
library(reshape2)

res = read.csv("./res_082022.csv") %>% select(-X)

# ----------------------------
# plot for appendix: all Gaussian

res_plot <- res %>% select(-job.id) %>% select(-space) %>%filter(cat_variables == "all Gaussian")%>% filter(num_levels == 2) %>%
#  filter(learner == "ranger") %>% filter(outcome == "regr") %>% filter(num_levels == 2) %>% filter(effect_size == "c(x1x4 = 0.9, x2x3 = 0.9, x3y = 0.9, x4y = 0.9)") %>% 
  # note: num_levels irrelevant anyways, just for keeping the repls here
  group_by(problem, algorithm, n, num_levels, cat_variables, learner, effect_size, outcome) %>% summarise_all(mean)
res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n","cat_variables", "num_levels", "learner", "effect_size", "outcome"), value.name = "rejection_rate")
res_plot$cat_variables <- do.call(rbind, lapply(res_plot$cat_variables, function(x){paste(x, sep="", collapse="")})) 
ggplot(res_plot, aes(x = n, y = rejection_rate, col = variable, linetype = algorithm)) +
  facet_grid(effect_size ~learner, labeller = labeller(
    effect_size = c(`c(x1x4 = 0.5, x2x3 = 0.5, x3y = 0.5, x4y = 0.5)` = "beta = 0.5", `c(x1x4 = 0.9, x2x3 = 0.9, x3y = 0.9, x4y = 0.9)` = "beta = 0.9"),
    learner = c(`ranger` = "learner: random forest", `regression` = "learner: linear model")
  ) )+
  geom_hline(yintercept=0.05, linetype=1, color = "darkgrey", size=1, alpha=1)+
  geom_line(size = 1)+
  scale_linetype_manual(values=c("deep"=3,"gaussian"=2, "sequential" = 1))+
  scale_color_manual(values = c( "#BC3C29FF","#20854EFF" , "#E18727FF","#0072B5FF"), 
                     labels = c("X1: irrelevant \ncontinuous", "X2: irrelevant \ncontinuous", "X3: relevant \ncontinuous", "X4: relevant \ncontinuous"))+
  labs(x = "Sample Size", y = "Rejection Rate", color = "Variable",linetype = "Knockoffs",
       title = "")+
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    legend.key.size = ggplot2::unit(3, "line"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .5))+
  scale_x_continuous(breaks = unique(res$n),
                     labels = unique(res$n))+
  scale_y_continuous(breaks = c(0, 0.05, 0.25,0.5,0.75,1),
                     labels = c(0, 0.05, 0.25,0.5,0.75,1),  expand = c(0.02,0.001))+
  ggtitle("all gaussian. outcome: regr")


# -----------------
# plot for appendix: binary and low categorical case

res_plot <- res %>% select(-job.id) %>% select(-space) %>%filter(cat_variables == "X3")%>% filter(num_levels == 4) %>%
#  filter(learner == "ranger") %>% filter(outcome == "regr")%>% filter(effect_size == "c(x1x4 = 0.9, x2x3 = 0.9, x3y = 0.9, x4y = 0.9)") %>% 
  group_by(problem, algorithm, n, num_levels, cat_variables, learner, effect_size, outcome) %>% summarise_all(mean)
res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n","cat_variables", "num_levels", "learner", "effect_size", "outcome"), value.name = "rejection_rate")
res_plot$cat_variables <- do.call(rbind, lapply(res_plot$cat_variables, function(x){paste(x, sep="", collapse="")})) 
ggplot(res_plot, aes(x = n, y = rejection_rate, col = variable, linetype = algorithm)) +
  facet_grid(effect_size ~learner, labeller = labeller(
    effect_size = c(`c(x1x4 = 0.5, x2x3 = 0.5, x3y = 0.5, x4y = 0.5)` = "beta = 0.5", `c(x1x4 = 0.9, x2x3 = 0.9, x3y = 0.9, x4y = 0.9)` = "beta = 0.9"),
    learner = c(`ranger` = "learner: random forest", `regression` = "learner: linear model")
  ) )+
  geom_hline(yintercept=0.05, linetype=1, color = "darkgrey", size=1, alpha=1)+
  geom_line(size = 1)+
  scale_linetype_manual(values=c("deep"=3,"gaussian"=2, "sequential" = 1))+
  scale_color_manual(values = c( "#BC3C29FF","#20854EFF" , "#E18727FF","#0072B5FF"), 
                     labels = c("X1: irrelevant \ncontinuous", "X2: irrelevant \ncontinuous", "X3: relevant \ncategorical", "X4: relevant \ncontinuous"))+
  labs(x = "Sample Size", y = "Rejection Rate", color = "Variable",linetype = "Knockoffs",
       title = "")+
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    legend.key.size = ggplot2::unit(3, "line"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .5))+
  scale_x_continuous(breaks = unique(res$n),
                     labels = unique(res$n))+
  scale_y_continuous(breaks = c(0, 0.05, 0.25,0.5,0.75,1),
                     labels = c(0, 0.05, 0.25,0.5,0.75,1),  expand = c(0.02,0.001))+
  ggtitle("categorical X3 - 4 levels. outcome: regr")



#------------------
# plot for appendix: X3 high cardinality case 

res_plot <- res %>% select(-job.id) %>% select(-space) %>%filter(cat_variables == "X3")%>%filter(num_levels == 10)%>%
 # filter(learner == "ranger") %>% filter(outcome == "regr") %>% filter(num_levels == 10) %>% filter(effect_size == "c(x1x4 = 0.5, x2x3 = 0.5, x3y = 0.5, x4y = 0.5)") %>% 
  group_by(problem, algorithm, n, num_levels, cat_variables, learner, effect_size, outcome) %>% summarise_all(mean)
res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n","cat_variables", "num_levels", "learner", "effect_size", "outcome"), value.name = "rejection_rate")
res_plot$cat_variables <- do.call(rbind, lapply(res_plot$cat_variables, function(x){paste(x, sep="", collapse="")})) 
ggplot(res_plot, aes(x = n, y = rejection_rate, col = variable, linetype = algorithm)) +
  facet_grid(outcome ~learner , labeller = labeller(
    outcome = c(`classif` = "classification task", `regr` = "regression task"),
    learner = c(`ranger` = "learner: random forest", `regression` = "learner: linear model")
  ) )+
  geom_hline(yintercept=0.05, linetype=1, color = "darkgrey", size=1, alpha=1)+
  geom_line(size = 1)+
  scale_linetype_manual(values=c("deep"=3,"gaussian"=2, "sequential" = 1))+
 # scale_color_brewer(palette = "Set2", direction = -1,  aesthetics = c("color", "fill")) +
 # scale_color_manual(values=c("dd"="#66C2A5", "deep"="#FC8D62","gaussian"= "#8DA0CB", "sequential" = "#E78AC3"))+
  scale_color_manual(values = c( "#BC3C29FF","#20854EFF" , "#E18727FF","#0072B5FF"), 
                     labels = c("X1: irrelevant \ncontinuous", "X2: irrelevant \ncontinuous", "X3: relevant \ncategorical", "X4: relevant \ncontinuous"))+
  labs(x = "Sample Size", y = "Rejection Rate", color = "Variable",linetype = "Knockoffs",
       title = "")+
  scale_x_continuous(breaks = c(500, seq(1000,7000, by=1000)),
                     labels = c(500, seq(1000,7000, by=1000)), expand = c(0.03,0.03))+
  scale_y_continuous(breaks = c(0, 0.05, 0.25,0.5,0.75,1),
                     labels = c(0, 0.05, 0.25,0.5,0.75,1),  expand = c(0.02,0.001))+
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
   # legend.key.size = unit(3, "line"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .5))+
  ggtitle("categorical X3: 10 levels. effect sizes: 0.9")


#------------------------------------------------------------------
# Fig. 2 in Main text

res = read.csv("./res_0922022.csv") %>% select(-X)

res_plot <- res %>% select(-job.id) %>% select(-space) %>%filter(cat_variables == 'c("X1", "X3")')%>%
  filter(learner == "ranger") %>% filter(outcome == "regr") %>% filter(num_levels == 10) %>% filter(effect_size == "c(x1x4 = 0.5, x2x3 = 0.5, x3y = 0.5, x4y = 0.5)") %>% 
  group_by(problem, algorithm, n, num_levels, cat_variables, learner, effect_size, outcome) %>% summarise_all(mean)
res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n","cat_variables", "num_levels", "learner", "effect_size", "outcome"), value.name = "rejection_rate")
res_plot$cat_variables <- do.call(rbind, lapply(res_plot$cat_variables, function(x){paste(x, sep="", collapse="")})) 
ggplot(res_plot, aes(x = n, y = rejection_rate, col = variable, linetype = algorithm)) +
  geom_hline(yintercept=0.05, linetype=1, color = "darkgrey", size=2, alpha=0.5)+
  geom_line(size = 1.5)+
  scale_linetype_manual(values=c("deep"=3,"gaussian"=2, "sequential" = 1))+
  scale_color_manual(values = c( "#BC3C29FF","#20854EFF" , "#E18727FF","#0072B5FF"), 
                     labels = c("X1: irrelevant \ncategorical", "X2: irrelevant \ncontinuous", "X3: relevant \ncategorical", "X4: relevant \ncontinuous"))+
  labs(x = "Sample Size", y = "Rejection Rate", color = "Variable",linetype = "Knockoffs",
       title = "")+
  scale_x_continuous(breaks = c(500, seq(1000,7000, by=1000)),
                     labels = c(500, seq(1000,7000, by=1000)), expand = c(0.03,0.03))+
  scale_y_continuous(breaks = c(0, 0.05, 0.25,0.5,0.75,1),
                     labels = c(0, 0.05, 0.25,0.5,0.75,1),  expand = c(0.02,0.001))+
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    legend.key.size = ggplot2::unit(3, "line"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .5))


# ----------------------------
# plot for appendix: all categorical case
# 2 settings: binary (num_levels == 2) and truly categorical (num_levels == 4)

res = read.csv("./res_all_cat_082022.csv") %>% select(-X)

res_plot <- res %>% select(-job.id) %>% select(-space) %>%filter(cat_variables == res$cat_variables[1])%>% filter(num_levels == 2) %>%
  group_by(problem, algorithm, n, num_levels, cat_variables, learner, effect_size, outcome) %>% summarise_all(mean)
res_plot <- melt(res_plot, id.vars = c("problem", "algorithm", "n","cat_variables", "num_levels", "learner", "effect_size", "outcome"), value.name = "rejection_rate")
res_plot$cat_variables <- do.call(rbind, lapply(res_plot$cat_variables, function(x){paste(x, sep="", collapse="")})) 
ggplot(res_plot, aes(x = n, y = rejection_rate, col = variable, linetype = algorithm)) +
  facet_grid(effect_size ~learner, labeller = labeller(
    effect_size = c(`c(x1x4 = 0.5, x2x3 = 0.5, x3y = 0.5, x4y = 0.5)` = "beta = 0.5", `c(x1x4 = 0.9, x2x3 = 0.9, x3y = 0.9, x4y = 0.9)` = "beta = 0.9"),
    learner = c(`ranger` = "learner: random forest", `regression` = "learner: linear model")
  ) )+
  geom_hline(yintercept=0.05, linetype=1, color = "darkgrey", size=1, alpha=1)+
  geom_line(size = 1)+
  scale_linetype_manual(values=c("deep"=3,"gaussian"=2, "sequential" = 1))+
  scale_color_manual(values = c( "#BC3C29FF","#20854EFF" , "#E18727FF","#0072B5FF"), 
                     labels = c("X1: irrelevant \ncategorical", "X2: irrelevant \ncategorical", "X3: relevant \ncategorical", "X4: relevant \ncategorical"))+
  labs(x = "Sample Size", y = "Rejection Rate", color = "Variable",linetype = "Knockoffs",
       title = "")+
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "right",
    plot.title.position = "plot",
    legend.key.size = ggplot2::unit(3, "line"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .5))+
  scale_x_continuous(breaks = unique(res$n),
                     labels = unique(res$n))+
  scale_y_continuous(breaks = c(0, 0.05, 0.25,0.5,0.75,1),
                     labels = c(0, 0.05, 0.25,0.5,0.75,1),  expand = c(0.02,0.001))+
  ggtitle(paste0("all categprical; outcome: regr; num_levels = ",unique(res_plot$num_levels) ))


