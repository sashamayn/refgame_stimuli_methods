library('dplyr')
library('ggplot2')
library('lme4')
library('brms')

source('stat_helpers.R')

df_allexps <- read.csv('../data/all_experiments.csv')

### Exp 1 ###
exp1 <- subset(df_allexps,df_allexps$experiment==1)
exp1 <- remove_outliers(exp1)

exp1_num_ambig_t_and_c <- length(exp1$correct[exp1$condition == 'filler ambiguous' & 
                          (exp1$answer_which == 'target' | 
                           exp1$answer_which == 'competitor')])

more_targets <- sample(0:1,size=1)

num_targets <- ifelse(more_targets, ceiling(exp1_num_ambig_t_and_c/2), floor(exp1_num_ambig_t_and_c/2))
num_competitors <- ifelse(more_targets, floor(exp1_num_ambig_t_and_c/2), ceiling(exp1_num_ambig_t_and_c/2))

targets <- rep(1, num_targets)
competitors <- rep(0, num_competitors)

both <- c(targets, competitors)
both_shuffled <- sample(both, size=exp1_num_ambig_t_and_c,rep=F)

exp1$correct[exp1$condition == 'filler ambiguous' & (exp1$answer_which == 'target' | 
             exp1$answer_which == 'competitor')] <- both_shuffled

exp1$answer_which[exp1$condition == 'filler ambiguous' & exp1$answer_which != 'distractor'] <- 
  ifelse(exp1$correct[exp1$condition == 'filler ambiguous' & exp1$answer_which != 'distractor'] == 1,
 'target','competitor')

barplot_exp1 <- barplot_by_condition(exp1)

exp1_for_logregr <- prep_for_logregr(exp1,c('rh','bh'))
exp1_for_logregr$msgtype <- as.factor(exp1_for_logregr$msgtype)

### Exp 2 ###
exp2 <- subset(df_allexps,df_allexps$experiment==2)
exp2 <- remove_outliers(exp2)

exp2_num_ambig_t_and_c <- length(exp2$correct[exp2$condition == 'filler ambiguous' & 
                                                (exp2$answer_which == 'target' | 
                                                   exp2$answer_which == 'competitor')])

more_targets <- sample(0:1,size=1)

num_targets <- ifelse(more_targets, ceiling(exp2_num_ambig_t_and_c/2), floor(exp2_num_ambig_t_and_c/2))
num_competitors <- ifelse(more_targets, floor(exp2_num_ambig_t_and_c/2), ceiling(exp2_num_ambig_t_and_c/2))

targets <- rep(1, num_targets)
competitors <- rep(0, num_competitors)

both <- c(targets, competitors)
both_shuffled <- sample(both, size=exp2_num_ambig_t_and_c,rep=F)

exp2$correct[exp2$condition == 'filler ambiguous' & (exp2$answer_which == 'target' | 
                                                       exp2$answer_which == 'competitor')] <- both_shuffled

exp2$answer_which[exp2$condition == 'filler ambiguous' & exp2$answer_which != 'distractor'] <- 
  ifelse(exp2$correct[exp2$condition == 'filler ambiguous' & exp2$answer_which != 'distractor'] == 1,
         'target','competitor')

barplot_exp2 <- barplot_by_condition(exp2)

exp2_for_logregr <- prep_for_logregr(exp2,c('rh','sc'))
exp2_for_logregr$msgtype <- as.factor(exp2_for_logregr$msgtype)

### Exp 3 ###
exp3 <- subset(df_allexps,df_allexps$experiment==3)
exp3 <- remove_outliers(exp3)

exp3_num_ambig_t_and_c <- length(exp3$correct[exp3$condition == 'filler ambiguous' & 
                                                (exp3$answer_which == 'target' | 
                                                   exp3$answer_which == 'competitor')])

more_targets <- sample(0:1,size=1)

num_targets <- ifelse(more_targets, ceiling(exp3_num_ambig_t_and_c/2), floor(exp3_num_ambig_t_and_c/2))
num_competitors <- ifelse(more_targets, floor(exp3_num_ambig_t_and_c/2), ceiling(exp3_num_ambig_t_and_c/2))

targets <- rep(1, num_targets)
competitors <- rep(0, num_competitors)

both <- c(targets, competitors)
both_shuffled <- sample(both, size=exp3_num_ambig_t_and_c,rep=F)

exp3$correct[exp3$condition == 'filler ambiguous' & (exp3$answer_which == 'target' | 
                                                       exp3$answer_which == 'competitor')] <- both_shuffled

exp3$answer_which[exp3$condition == 'filler ambiguous' & exp3$answer_which != 'distractor'] <- 
  ifelse(exp3$correct[exp3$condition == 'filler ambiguous' & exp3$answer_which != 'distractor'] == 1,
         'target','competitor')

barplot_exp3 <- barplot_by_condition(exp3)

exp3_for_logregr <- prep_for_logregr(exp3,c('rh','bh'))
exp3_for_logregr$msgtype <- as.factor(exp3_for_logregr$msgtype)

### Exp 4 ###
exp4 <- subset(df_allexps,df_allexps$experiment==4)
exp4 <- remove_outliers(exp4)

exp4_num_ambig_t_and_c <- length(exp4$correct[exp4$condition == 'filler ambiguous' & 
                                                (exp4$answer_which == 'target' | 
                                                   exp4$answer_which == 'competitor')])

more_targets <- sample(0:1,size=1)

num_targets <- ifelse(more_targets, ceiling(exp4_num_ambig_t_and_c/2), floor(exp4_num_ambig_t_and_c/2))
num_competitors <- ifelse(more_targets, floor(exp4_num_ambig_t_and_c/2), ceiling(exp4_num_ambig_t_and_c/2))

targets <- rep(1, num_targets)
competitors <- rep(0, num_competitors)

both <- c(targets, competitors)
both_shuffled <- sample(both, size=exp4_num_ambig_t_and_c,rep=F)

exp4$correct[exp4$condition == 'filler ambiguous' & (exp4$answer_which == 'target' | 
                                                       exp4$answer_which == 'competitor')] <- both_shuffled

exp4$answer_which[exp4$condition == 'filler ambiguous' & exp4$answer_which != 'distractor'] <- 
  ifelse(exp4$correct[exp4$condition == 'filler ambiguous' & exp4$answer_which != 'distractor'] == 1,
         'target','competitor')

barplot_exp4 <- barplot_by_condition(exp4)

exp4_for_logregr <- prep_for_logregr(exp4,c('re','gr'))
exp4_for_logregr$msgtype <- as.factor(exp4_for_logregr$msgtype)


### brms models ###
exp1_for_logregr$condition <- factor(exp1_for_logregr$condition,levels=c('target complex','target simple','filler ambiguous'))
m1_brms <- brm(correct ~ condition + trialid_centered + condition*trialid_centered +
                 msgtype + targetpos + (1+condition+msgtype+trialid_centered|participantid) + 
                 (1|itemid),data=exp1_for_logregr, family=binomial, iter=4000)

exp2_for_logregr$condition <- factor(exp2_for_logregr$condition,levels=c('target complex','target simple','filler ambiguous'))
m2_brms <- brm(correct ~ condition + trialid_centered + condition*trialid_centered +
                 msgtype + targetpos + (1+condition+msgtype+trialid_centered|participantid) + 
                 (1|itemid),data=exp2_for_logregr, family=binomial, iter=4000)

exp3_for_logregr$condition <- factor(exp3_for_logregr$condition,levels=c('target complex','target simple','filler ambiguous'))
m3_brms <- brm(correct ~ condition + trialid_centered + condition*trialid_centered +
                 msgtype + targetpos + (1+condition+msgtype+trialid_centered|participantid) + 
                 (1|itemid),data=exp3_for_logregr, family=binomial, iter=4000)

exp4_for_logregr$condition <- factor(exp4_for_logregr$condition,levels=c('target complex','target simple','filler ambiguous'))
m4_brms <- brm(correct ~ condition + trialid_centered + condition*trialid_centered +
                 msgtype + targetpos + (1+condition+msgtype+trialid_centered|participantid) + 
                 (1|itemid),data=exp4_for_logregr, family=binomial, iter=4000)

summary(m1_brms)
summary(m2_brms)
summary(m3_brms)
summary(m4_brms)

bayesian_models = list(m1_brms, m2_brms, m3_brms, m4_brms)
barplots = list(barplot_exp1, barplot_exp2, barplot_exp3, barplot_exp4)
dfs_for_logregr = list(exp1_for_logregr, exp2_for_logregr, exp3_for_logregr, exp4_for_logregr)

save(barplots, file='RData/barplots.RData')
save(dfs_for_logregr, file='RData/dfs_for_logregr.RData')

#save(bayesian_models, file='RData/brms_models.RData') 
##  The brms model files are quite big -- 276,1 MB. 
##  Therefore, we did not upload them to the repository. 
##  They can be generated by running the code above
##  If you want the same randomization of the ambiguous condition,
##  first load the data -- dfs_for_logregr.RData.
