library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
source('stat_helpers.R')

anns <- read.csv('../data/all_annotations.csv')
data <- read.csv('../data/all_experiments.csv')
data_wo_outliers <- remove_outliers(data)

data <- data[,c('participantid','itemid','strategy_text','strategy_correct','strategy_answer_which')]
anns <- merge(anns, data, by=c('participantid','itemid','strategy_text'))

anns$exp <- NA
anns$exp[anns$experiment == 1] <- 'replication'
anns$exp[anns$experiment == 2] <- 'remapped'
anns$exp[anns$experiment == 3] <- 'all messages'
anns$exp[anns$experiment == 4] <- 'shapes'

anns_filtered <- subset(anns, anns$tag_both != 'unclear' & anns$tag_both != 'exclude')
anns_filtered <- subset(anns_filtered, anns_filtered$participantid %in% data_wo_outliers$participantid)

#coarse-grained tag 
anns_filtered$tag_fewer_cats <- anns_filtered$tag_both
anns_filtered$tag_fewer_cats[anns_filtered$subtag_both == 'odd_one_out' |
                               anns_filtered$subtag_both == 'visual_resemblance' |
                               anns_filtered$subtag_both == 'salience' |
                               anns_filtered$subtag_both == 'preference' |
                               anns_filtered$subtag_both == 'other'] <- 'other_reason'

anns_filtered$tag_fewer_cats[anns_filtered$subtag_both == 'variability' |
                               anns_filtered$subtag_both == 'multiturn' |
                               anns_filtered$subtag_both == 'screenloc'] <- 'guess'
anns_filtered$tag_fewer_cats[anns_filtered$subtag_both == 'misunderstood_instructions'] <- 'misunderstood_instr'


#fine-grained tag (incl subcategory)
anns_filtered$tag_fine <- anns_filtered$subtag_both
anns_filtered$tag_fine[anns_filtered$tag_both == 'correct_reasoning'] <- 'correct_reasoning'
anns_filtered$tag_fine[anns_filtered$tag_both == 'guess'] <- 'guess'
anns_filtered$tag_fine[anns_filtered$tag_both == 'unclear'] <- 'unclear'
anns_filtered$tag_fine[anns_filtered$tag_both == 'exclude'] <- 'exclude'

#### stacked bar chart, simple only
anns_simple <- subset(anns_filtered, itemtype=="target simple")
anns_simple$divideby <- NA

for (exp_name in c("replication","remapped","all messages","shapes")){
  anns_simple$divideby[anns_simple$exp == exp_name] <- nrow(subset(anns_simple,anns_simple$exp == exp_name))
}

anns_long_simple <- anns_simple %>% group_by(exp, tag_fewer_cats, strategy_correct, divideby) %>% summarize(n=n())
anns_long_simple$prop <- anns_long_simple$n/anns_long_simple$divideby

anns_long_simple$strategy_correct <- factor(anns_long_simple$strategy_correct, levels=c('True','False'))
anns_long_simple$tag_fewer_cats <- factor(anns_long_simple$tag_fewer_cats, levels=c("misunderstood_instr","guess",
                                                                                    "other_reason","correct_reasoning"))

anns_long_simple_answerWhich <- anns_simple %>% group_by(exp, tag_fewer_cats, strategy_answer_which, divideby) %>% summarize(n=n())
anns_long_simple_answerWhich$prop <- anns_long_simple_answerWhich$n/anns_long_simple_answerWhich$divideby
anns_long_simple_answerWhich$answer_which <- factor(anns_long_simple_answerWhich$strategy_answer_which, levels=c("target","competitor","distractor"))
colnames(anns_long_simple)[2] <- 'annotation tag'

anns_long_simple$corr <- ifelse(anns_long_simple$strategy_correct=='True','correct','incorrect')
anns_long_simple$exp <- factor(anns_long_simple$exp,levels=c('replication','remapped','all messages','shapes'))

simple_byexp <- ggplot(anns_long_simple) + geom_bar(aes(corr, prop, fill = `annotation tag`),position  = 'stack', width = 0.6, stat="identity") + 
  facet_wrap(~exp, nrow = 1) + scale_fill_manual(values = c("#ec7063","#99a3a4","#3498db","#229954")) +
  theme(panel.spacing.x = unit(0.5, "lines")) + theme(panel.spacing.x = unit(0.5, "lines"), axis.text=element_text(size=14), 
                                                      axis.title=element_text(size=15), strip.text.x = element_text(size = 16), 
                                                      legend.text=element_text(size=15), legend.title=element_text(size=15)) +
  
  xlab('')+ylab('Proportion')

#### stacked bar chart, complex only
anns_complex <- subset(anns_filtered, itemtype=="target complex")
anns_complex$divideby <- NA

for (exp_name in c("replication","remapped","all messages","shapes")){
  anns_complex$divideby[anns_complex$exp == exp_name] <- nrow(subset(anns_complex,anns_complex$exp == exp_name))
}

anns_long_complex <- anns_complex %>% group_by(exp, tag_fewer_cats, strategy_correct, divideby) %>% summarize(n=n())
anns_long_complex$prop <- anns_long_complex$n/anns_long_complex$divideby

anns_long_complex$strategy_correct <- factor(anns_long_complex$strategy_correct, levels=c('True','False'))
anns_long_complex$tag_fewer_cats <- factor(anns_long_complex$tag_fewer_cats, levels=c("misunderstood_instr","guess",
                                                                                    "other_reason","correct_reasoning"))

anns_long_complex_answerWhich <- anns_complex %>% group_by(exp, tag_fewer_cats, strategy_answer_which, divideby) %>% summarize(n=n())
anns_long_complex_answerWhich$prop <- anns_long_complex_answerWhich$n/anns_long_complex_answerWhich$divideby
anns_long_complex_answerWhich$answer_which <- factor(anns_long_complex_answerWhich$strategy_answer_which, levels=c("target","competitor","distractor"))
colnames(anns_long_complex)[2] <- 'annotation tag'

anns_long_complex$corr <- ifelse(anns_long_complex$strategy_correct=='True','correct','incorrect')
anns_long_complex$exp <- factor(anns_long_complex$exp,levels=c('replication','remapped','all messages','shapes'))

complex_byexp <- ggplot(anns_long_complex) + geom_bar(aes(corr, prop, fill = `annotation tag`),position  = 'stack', width = 0.6, stat="identity") + 
  facet_wrap(~exp, nrow = 1) + scale_fill_manual(values = c("#ec7063","#99a3a4","#3498db","#229954")) +
  theme(panel.spacing.x = unit(0.5, "lines")) + theme(panel.spacing.x = unit(0.5, "lines"), axis.text=element_text(size=14), 
                                                      axis.title=element_text(size=15), strip.text.x = element_text(size = 16), 
                                                      legend.text=element_text(size=15), legend.title=element_text(size=15)) +
  
  xlab('')+ylab('Proportion')

### stacked bar charts for other_reason (breakdown by tag), simple only
other_reason_simple <- subset(anns_simple,anns_simple$itemtype=="target simple" & anns_simple$tag_both == "other_reason")
other_reason_simple <- subset(other_reason_simple, tag_fine != "misunderstood_instructions")

exps_long_simple_other <- other_reason_simple %>% group_by(exp, tag_fine, strategy_correct, divideby) %>% summarize(n=n())
exps_long_simple_other$corr <- ifelse(exps_long_simple_other$strategy_correct=='True','correct','incorrect')
exps_long_simple_other$exp <- factor(exps_long_simple_other$exp,levels=c('replication','remapped','all messages','shapes'))

other_reason_simple_plot <- ggplot(exps_long_simple_other) + 
  geom_bar(aes(factor(corr),n, fill = tag_fine),position  = 'stack', width = 0.6, 
           stat="identity") + facet_wrap(~factor(exp), nrow = 1) + theme(panel.spacing = unit(0, "lines")) + 
  theme(panel.spacing.x = unit(0.5, "lines"), axis.text=element_text(size=14), 
        axis.title=element_text(size=14), strip.text.x = element_text(size = 15), 
        legend.text=element_text(size=15), legend.title=element_text(size=15), 
  ) + xlab('')+
  ylab('Count') + scale_fill_manual('other reason type', values = 
                                      c(odd_one_out = "#809fff", other = "#a3a3c2", preference = "#ff66b3", 
                                        salience = "#d5ff80", visual_resemblance = "#00cc99"))

### stacked bar charts for other_reason (breakdown by tag), complex only
other_reason_complex <- subset(anns_complex,anns_complex$itemtype=="target complex" & anns_complex$tag_both == "other_reason")
other_reason_complex <- subset(other_reason_complex, tag_fine != "misunderstood_instructions")

exps_long_complex_other <- other_reason_complex %>% group_by(exp, tag_fine, strategy_correct, divideby) %>% summarize(n=n())
exps_long_complex_other$corr <- ifelse(exps_long_complex_other$strategy_correct=='True','correct','incorrect')
exps_long_complex_other$exp <- factor(exps_long_complex_other$exp,levels=c('replication','remapped','all messages','shapes'))

other_reason_complex_plot <- ggplot(exps_long_complex_other) + 
  geom_bar(aes(factor(corr),n, fill = tag_fine),position  = 'stack', width = 0.6, 
           stat="identity") + facet_wrap(~factor(exp), nrow = 1) + theme(panel.spacing = unit(0, "lines")) + 
  theme(panel.spacing.x = unit(0.5, "lines"), axis.text=element_text(size=14), 
        axis.title=element_text(size=14), strip.text.x = element_text(size = 15), 
        legend.text=element_text(size=15), legend.title=element_text(size=15), 
  ) + xlab('')+
  ylab('Count') + scale_fill_manual('other reason type', values = 
                                      c(odd_one_out = "#809fff", other = "#a3a3c2", preference = "#ff66b3", 
                                        salience = "#d5ff80", visual_resemblance = "#00cc99"))

both_simple_plots <- ggarrange(simple_byexp, other_reason_simple_plot, ncol = 1, nrow = 2)
both_complex_plots <- ggarrange(complex_byexp, other_reason_complex_plot, ncol = 1, nrow = 2)

#png(file="simple_anns.png",width=800, height=600)
#both_simple_plots
#dev.off()

#png(file="complex_anns.png",width=800, height=600)
#both_complex_plots
#dev.off()

