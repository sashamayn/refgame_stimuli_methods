library('tidyLPA')
library('ggplot2')
library('ggpubr')
library('dplyr')

source('stat_helpers.R')

data <- read.csv('../data/all_experiments.csv')
annotations <- read.csv('../data/all_annotations.csv')
### Exp 1 ###
exp1 <- subset(data,data$experiment==1)
exp1 <- remove_outliers(exp1)
data1 <- df_avg_bypartic(exp1)

#model comparison
profiles1 <- estimate_profiles(df = data1[4:5], n_profiles = 1:6, variances = "equal",
            covariances = "zero",nrep=100) %>% compare_solutions(statistics = c("AIC", "BIC"))

by_partic.est_classes1 <- estimate_profiles(data1[4:5], n_profiles = 4, variances="equal",covariances="zero",nrep=100) %>% 
  get_data()
by_partic.est_classes1 <- data.frame(by_partic.est_classes1)
by_partic.est_classes1$id  <- 1:nrow(by_partic.est_classes1)
data1$id <- 1:nrow(data1)
data1$Class <- as.factor(by_partic.est_classes1$Class[match(by_partic.est_classes1$id,data1$id)])

### Exp 4 ###
exp4 <- subset(data,data$experiment==4)
exp4 <- remove_outliers(exp4)
data4 <- df_avg_bypartic(exp4)

profiles4 <- estimate_profiles(df = data4[4:5], n_profiles = 1:6, variances = "equal",
                               covariances = "zero",nrep=100) %>% compare_solutions(statistics = c("AIC", "BIC"))

by_partic.est_classes4 <- estimate_profiles(data4[4:5], n_profiles = 4, variances="equal",covariances="zero",nrep=100) %>% 
  get_data()
by_partic.est_classes4 <- data.frame(by_partic.est_classes4)
by_partic.est_classes4$id  <- 1:nrow(by_partic.est_classes4)
data4$id <- 1:nrow(data4)
data4$Class <- as.factor(by_partic.est_classes4$Class[match(by_partic.est_classes4$id,data4$id)])

annotations$tag_both[annotations$subtag_both == 'misunderstood_instructions'] <- 'misunderstood_instr'
anns_simple <- subset(annotations, itemtype=='target simple')
data1$ann_simple <- as.factor(anns_simple$tag_both[match(data1$participantid,anns_simple$participantid)])
data4$ann_simple <- as.factor(anns_simple$tag_both[match(data4$participantid,anns_simple$participantid)])

anns_complex <- subset(annotations, itemtype=='target complex')
data1$ann_complex <- as.factor(anns_complex$tag_both[match(data1$participantid,anns_complex$participantid)])
data4$ann_complex <- as.factor(anns_complex$tag_both[match(data4$participantid,anns_complex$participantid)])


colnames(data1)[8] <- 'annotation (simple condition)'
colnames(data1)[9] <- 'annotation (complex condition)'

colnames(data4)[8] <- 'annotation (simple condition)'
colnames(data4)[9] <- 'annotation (complex condition)'

data1$`reasoning type` <- NA
data1$`reasoning type`[data1$Class==1] <- 'L0'
data1$`reasoning type`[data1$Class==2] <- 'L2'
data1$`reasoning type`[data1$Class==3] <- 'L1'
data1$`reasoning type`[data1$Class==4] <- 'below_chance'

data4$`reasoning type` <- NA
data4$`reasoning type`[data4$Class==1] <- 'L1'
data4$`reasoning type`[data4$Class==2] <- 'L2'
data4$`reasoning type`[data4$Class==3] <- 'below_chance'
data4$`reasoning type`[data4$Class==4] <- 'L0'


lpa_exp1_simple <- ggplot(data=subset(data1, data1$`annotation (simple condition)`!='unclear' & data1$`annotation (simple condition)`!='exclude'), 
  aes(x=`target simple` ,y=`target complex`, shape = `annotation (simple condition)`, color=`reasoning type`)) + 
  geom_point(size=12, position='jitter') + xlim(-0.1,1.1) +ylim(-0.1,1.1) +
  xlab('simple: proportion correct') + ylab('complex: proportion correct') + 
  scale_shape_manual(values=c(19,17,18,15)) +
  theme(text = element_text(size = 36)) + scale_size_area(max_size = 56) +
  guides(
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )

lpa_exp4_simple <- ggplot(data=subset(data4, data4$`annotation (simple condition)`!='unclear' & data4$`annotation (simple condition)`!='exclude'), 
  aes(x=`target simple` ,y=`target complex`, shape = `annotation (simple condition)`, color=`reasoning type`)) + 
  geom_point(size=12, position='jitter') + xlim(-0.1,1.1) +ylim(-0.1,1.1) +
  xlab('simple: proportion correct') + ylab('complex: proportion correct') + 
  scale_shape_manual(values=c(19,17,18,15)) +
  theme(text = element_text(size = 36)) + scale_size_area(max_size = 56) +
  guides(
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )

#Figure 7 in the paper
both_lpas_simple <-ggarrange(lpa_exp1_simple, lpa_exp4_simple, ncol = 1, nrow = 2)

#png("figure7.png", width=1900, height = 1700)
#both_lpas_simple
#dev.off()

lpa_exp1_complex <- ggplot(data=subset(data1, data1$`annotation (complex condition)`!='unclear' & data1$`annotation (complex condition)`!='exclude'), 
                          aes(x=`target simple` ,y=`target complex`, shape = `annotation (complex condition)`, color=`reasoning type`)) + 
  geom_point(size=12, position='jitter') + xlim(-0.1,1.1) +ylim(-0.1,1.1) +
  xlab('simple: proportion correct') + ylab('complex: proportion correct') + 
  scale_shape_manual(values=c(19,17,15)) +
  theme(text = element_text(size = 36)) + scale_size_area(max_size = 56) +
  guides(
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )

lpa_exp4_complex <- ggplot(data=subset(data4, data4$`annotation (complex condition)`!='unclear' & data4$`annotation (complex condition)`!='exclude'), 
                          aes(x=`target simple` ,y=`target complex`, shape = `annotation (complex condition)`, color=`reasoning type`)) + 
  geom_point(size=12, position='jitter') + xlim(-0.1,1.1) +ylim(-0.1,1.1) +
  xlab('simple: proportion correct') + ylab('complex: proportion correct') + 
  scale_shape_manual(values=c(19,17,18,15)) +
  theme(text = element_text(size = 36)) + scale_size_area(max_size = 56) +
  guides(
    color = guide_legend(order = 2),
    shape = guide_legend(order = 1)
  )

#Figure 10 in the paper
both_lpas_complex <- ggarrange(lpa_exp1_complex, lpa_exp4_complex, ncol = 1, nrow = 2)

#png("figure10.png", width=1900, height = 1700)
#both_lpas_complex
#dev.off()

