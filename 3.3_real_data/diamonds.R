set.seed(1)

library(farff)
library(iml)
library(cs)
library(cpi)
library(fastDummies)
library(cowplot)
library(reticulate)
library(dplyr)

df = readARFF("./real_data/diamonds_data/dataset.arff")
df = df[which(abs(df$x - df$y) < 0.02),]

# rename variables
df = df %>% dplyr::rename(depth_perc = depth, x_length = x, y_width = y, z_depth = z, y = price)


# split train test set
train_id <- sample(1:nrow(df), size = (2/3) * nrow(df))

train <- df[train_id,]
test <- df[-train_id,]

# preprare for model training
my_learner <- lrn("regr.ranger", predict_type = "response")
my_task <- as_task_regr(x = train, target = "y") 
my_measure <- msr("regr.mse")
my_loss <- Metrics::mse
my_loss_instance <-  Metrics::se

###############
# Variable Importance measures
###############

# CPI sequential knockoffs
res_cpi_seq <- cpi(task = my_task,
                   learner = my_learner,
                   resampling = "none",
                   log = FALSE, 
                   test_data = test,
                   knockoff_fun = seqknockoff::knockoffs_seq,
                   test = "t")
res_cpi_seq$feature = res_cpi_seq$Variable
res_cpi_seq$importance = res_cpi_seq$CPI
res_cpi_seq$holm = p.adjust(res_cpi_seq$p.value, "holm")


# train model, predict
fit <- my_learner$train(my_task)
pred <-  Predictor$new(fit, data = test, y = "y")

# validate model
fit$predict_newdata(newdata = test)$score(measures = msr("regr.mse")) ## MSE: 538157.1
fit$predict_newdata(newdata = test)$score(measures = msr("regr.rsq")) ## R squared: 0.9425527

# PFI
res_pfi =  iml::FeatureImp$new(predictor = pred, loss = my_loss, compare = "difference", n.repetitions = 5)$results 

# SAGE
source("./sim_FI_comparison/SAGE_R_efficient.R")
# rename target to "y"
res_sage <- data.frame("importance" = sage(model = fit, data = test, target = "y", loss = my_loss_instance))
res_sage$feature <- rownames(res_sage)
##


# cond. subgroup
# 1. fit tree
feature_names = setdiff(colnames(train), "y")
my_ctrl = partykit::ctree_control(maxdepth = 10, minbucket = 50) # 10, 50
my_conds = cs::fit_conditionals(data = train[, feature_names], ctrl = my_ctrl, type = "trtf")
# 2. caclulate cPFI 
# res = grouped_pfi(pred = pred, loss = model$loss_instance , conds = my_conds, repetitions = 5) # it's not the instance wise loss but just for 
res_cs = cs::grouped_pfi(pred = pred, loss = my_loss , conds = my_conds, repetitions = 5) 


# LOCO
#full_fit <- my_learner$train(my_task)
full_pred <-  fit$predict_newdata(newdata = test)
full_loss <-  full_pred$score(measures = my_measure)
full_loss = (test$y - full_pred$response)^2
# find "y"
s = 1:ncol(train)
s = s[!s %in% which(colnames(train) == "y")]
# leave one out
# modify task:
res_loco <-  sapply(s, function(i) {
  my_loco_task <- as_task_regr(x = train[,-i], target = "y") 
  red_fit <- my_learner$train(my_loco_task)
  red_pred <- my_learner$predict_newdata(newdata = test[,-i])                 
  #red_loss <- red_pred$score(measures = my_measure)
  red_loss = (test$y - red_pred$response)^2
  delta <- red_loss - full_loss
  list(i = i, importance = mean(delta), feature =  colnames(train)[i], ttest = t.test(delta, alternative = 'greater')$p.value) 
}) 
res_loco = data.frame(t(res_loco))
res_loco$importance = as.numeric(res_loco$importance)
res_loco$feature = unlist(res_loco$feature)
res_loco$holm = p.adjust(res_loco$ttest, "holm")

# Boruta -- print results
library(Boruta)
print("Variables confirmed by Boruta for being relevant:")
Boruta(y ~ . , data = train, num.threads = 1)$finalDecision  
# --> Boruta does classify relevance of all variables as 'Confirmed'


## plot results

plt_cpi_seq <- ggplot(res_cpi_seq, aes(x= feature , y=importance, fill = holm < 0.05)) + scale_fill_manual(values = c("#BC3C29FF", "darkblue"))+
  geom_bar(stat='identity') + ggtitle("CPIseq")+ 
  coord_flip() +  labs(fill='CPIseq: Significant') +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5), legend.position="none",axis.title.y=element_blank(),
                        legend.title=element_text(size=12), 
                        legend.text=element_text(size=12))+ylab("Importance")
plt_loco <- ggplot(res_loco, aes(x= feature , y=importance,  fill = holm < 0.05)) + scale_fill_manual(values = c("#BC3C29FF", "darkblue"))+
  geom_bar(stat='identity') +ggtitle("LOCO")+
  coord_flip()+ xlab("")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5),
                        axis.title.y=element_blank(),
                        #   axis.text.y =element_blank(),
                        #   axis.ticks.y=element_blank(),
                        legend.position="none")+ylab("Importance")

plt_cs <- ggplot(res_cs, aes(x= feature , y=importance, fill = "#0072B5FF"))  + scale_fill_manual(values = c( "#0072B5FF"))+
  geom_bar(stat='identity') +ggtitle("Conditional Subgroup")+
  coord_flip()+ xlab("")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5),
                        axis.title.y=element_blank(),
                        #  axis.text.y =element_blank(),
                        #  axis.ticks.y=element_blank(),
                        legend.position="none")+ylab("Importance")
plt_pfi <- ggplot(res_pfi, aes(x= feature , y=importance, fill = "#0072B5FF"))  + scale_fill_manual(values = c("#0072B5FF"))+
  geom_bar(stat='identity') + ggtitle("PFI")+
  coord_flip()+ xlab("")+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5),
                        axis.title.y=element_blank(),
                        #  axis.text.y =element_blank(),
                        #  axis.ticks.y=element_blank(),
                        legend.position="none")+ylab("Importance")
plt_sage <- ggplot(res_sage, aes(x= feature , y=importance, fill = "#0072B5FF"))  + scale_fill_manual(values = c( "#0072B5FF"))+
  geom_bar(stat='identity') +ggtitle("SAGE")+
  coord_flip()+ xlab("")+
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),
                          axis.title.y=element_blank(),
                          #   axis.text.y =element_blank(),
                          #  axis.ticks.y=element_blank(),
                          legend.position="none")+ylab("Importance")

legend_dat <- data.frame(x = c(1,2,3), y = c(1,2,3), importance = c("t-test significant", "t-test not significant", "no test" ))

legend_plt <- ggplot(legend_dat, aes(x= x , y=y, fill = importance)) + scale_fill_manual(values = c("#0072B5FF" ,"darkblue","#BC3C29FF"),
                                                                                         breaks = c("no test", "t-test significant", "t-test not significant"))+
  geom_bar(stat='identity') + ggtitle("CPI sequential")+ 
  coord_flip() +  labs(fill='Variable Importance Score') +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5), legend.position="right",axis.title.y=element_blank(),
                        legend.title=element_text(size=16), 
                        legend.text=element_text(size=16))
legend_plt
legend <- cowplot::get_legend(legend_plt)
final_plot <- plot_grid(plt_cpi_seq,plt_loco, plt_cs,plt_pfi,plt_sage,legend, ncol = 3, rel_widths = c(.3,.3,.3),rel_heights = c(.5), labels =c("A", "B", "C", "D", "E"), label_x = c(0.02, 0.02,0.02) )
ggsave(final_plot, file="./real_data/diamonds.eps", device="eps")
res <- data.frame(bind_rows(res_cpi_seq, res_cs, res_loco, res_pfi, res_sage), method = rep(c("CPI", "CS", "LOCO", "PFI", "SAGE"), each = 9))
write.csv(apply(res,2,as.character), file = "./real_data/res_diamonds.csv")
