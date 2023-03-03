library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

anns <- read.csv('../data/all_annotations.csv')
data <- read.csv('../data/all_experiments.csv')[,c('participantid','itemid','strategy_text','correct','answer_which')]
anns <- merge(anns, data, by=c('participantid','itemid','strategy_text'))

anns_filtered <- subset(anns, anns$tag_both != 'unclear' & anns$tag_both != 'exclude')

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

for (exp in 1:4){
  anns_simple$divideby[anns_simple$experiment == exp] <- nrow(subset(anns_simple,anns_simple$experiment == exp))
}

anns_long_simple <- anns_simple %>% group_by(experiment, tag_fewer_cats, correct, divideby) %>% summarize(n=n())
anns_long_simple$prop <- anns_long_simple$n/anns_long_simple$divideby

anns_long_simple$correct <- factor(anns_long_simple$correct, levels=c('True','False'))
anns_long_simple$tag_fewer_cats <- factor(anns_long_simple$tag_fewer_cats, levels=c("misunderstood_instr","guess",
                                                                                    "other_reason","correct_reasoning"))

anns_long_simple_answerWhich <- anns_simple %>% group_by(experiment, tag_fewer_cats, answer_which, divideby) %>% summarize(n=n())
anns_long_simple_answerWhich$prop <- anns_long_simple_answerWhich$n/anns_long_simple_answerWhich$divideby
anns_long_simple_answerWhich$answer_which <- factor(anns_long_simple_answerWhich$answer_which, levels=c("target","competitor","distractor"))
colnames(anns_long_simple)[2] <- 'annotation tag'

anns_long_simple$corr <- ifelse(anns_long_simple$correct=='True','correct','incorrect')
anns_long_simple$ann_tag <- anns_long_simple$`annotation tag`
anns_long_simple$ann_tag <- as.character(anns_long_simple$ann_tag)

anns_long_simple$ann_tag <- factor(anns_long_simple$ann_tag,levels=c('misunderstood_instr','guess','other_reason','correct_reasoning'))

simple_byexp <- ggplot(anns_long_simple) + geom_bar(aes(correct, prop, fill = ann_tag),position  = 'stack', width = 0.6, stat="identity") + 
  facet_wrap(~experiment, nrow = 1) + theme(panel.spacing = unit(0, "lines")) + scale_fill_manual(values = c("#ec7063","#99a3a4",
                                                                                                      "#3498db","#229954")) +
  theme(panel.spacing.x = unit(0.5, "lines")) + xlab('Correct')+ylab('Prop.')

#### stacked bar chart, complex only
anns_complex <- subset(anns_filtered, itemtype=="target complex")
anns_complex$divideby <- NA

for (exp in 1:4){
  anns_complex$divideby[anns_complex$experiment == exp] <- nrow(subset(anns_complex,anns_complex$experiment == exp))
}

anns_long_complex <- anns_complex %>% group_by(experiment, tag_fewer_cats, correct, divideby) %>% summarize(n=n())
anns_long_complex$prop <- anns_long_complex$n/anns_long_complex$divideby

anns_long_complex$correct <- factor(anns_long_complex$correct, levels=c('True','False'))
anns_long_complex$tag_fewer_cats <- factor(anns_long_complex$tag_fewer_cats, levels=c("misunderstood_instr","guess",
                                                                                    "other_reason","correct_reasoning"))

anns_long_complex_answerWhich <- anns_complex %>% group_by(experiment, tag_fewer_cats, answer_which, divideby) %>% summarize(n=n())
anns_long_complex_answerWhich$prop <- anns_long_complex_answerWhich$n/anns_long_complex_answerWhich$divideby
anns_long_complex_answerWhich$answer_which <- factor(anns_long_complex_answerWhich$answer_which, levels=c("target","competitor","distractor"))
colnames(anns_long_complex)[2] <- 'annotation tag'

anns_long_complex$corr <- ifelse(anns_long_complex$correct=='True','correct','incorrect')
anns_long_complex$ann_tag <- anns_long_complex$`annotation tag`
anns_long_complex$ann_tag <- as.character(anns_long_complex$ann_tag)

anns_long_complex$ann_tag <- factor(anns_long_complex$ann_tag,levels=c('misunderstood_instr','guess','other_reason','correct_reasoning'))

complex_byexp <- ggplot(anns_long_complex) + geom_bar(aes(correct, prop, fill = ann_tag),position  = 'stack', width = 0.6, stat="identity") + 
  facet_wrap(~experiment, nrow = 1) + theme(panel.spacing = unit(0, "lines")) + scale_fill_manual(values = c("#ec7063","#99a3a4",
                                                                                                             "#3498db","#229954")) +
  theme(panel.spacing.x = unit(0.5, "lines")) + xlab('Correct')+ylab('Prop.')

### stacked bar charts for other_reason (breakdown by tag), simple only
other_reason_simple <- subset(anns_simple,anns_simple$itemtype=="target simple" & anns_simple$tag_both == "other_reason")
other_reason_simple <- subset(other_reason_simple, tag_fine != "misunderstood_instructions")

exps_long_simple_other <- other_reason_simple %>% group_by(experiment, tag_fine, correct, divideby) %>% summarize(n=n())
exps_long_simple_other$corr <- ifelse(exps_long_simple_other$correct=='True','correct','incorrect')

other_reason_simple_plot <- ggplot(exps_long_simple_other) + 
  geom_bar(aes(factor(corr),n, fill = tag_fine),position  = 'stack', width = 0.6, 
           stat="identity") + facet_wrap(~factor(experiment), nrow = 1) + theme(panel.spacing = unit(0, "lines")) + 
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

exps_long_complex_other <- other_reason_complex %>% group_by(experiment, tag_fine, correct, divideby) %>% summarize(n=n())
exps_long_complex_other$corr <- ifelse(exps_long_complex_other$correct=='True','correct','incorrect')

other_reason_complex_plot <- ggplot(exps_long_complex_other) + 
  geom_bar(aes(factor(corr),n, fill = tag_fine),position  = 'stack', width = 0.6, 
           stat="identity") + facet_wrap(~factor(experiment), nrow = 1) + theme(panel.spacing = unit(0, "lines")) + 
  theme(panel.spacing.x = unit(0.5, "lines"), axis.text=element_text(size=14), 
        axis.title=element_text(size=14), strip.text.x = element_text(size = 15), 
        legend.text=element_text(size=15), legend.title=element_text(size=15), 
  ) + xlab('')+
  ylab('Count') + scale_fill_manual('other reason type', values = 
                                      c(odd_one_out = "#809fff", other = "#a3a3c2", preference = "#ff66b3", 
                                        salience = "#d5ff80", visual_resemblance = "#00cc99"))

both_simple_plots <- ggarrange(simple_corr_incorr, other_reason_simple_plot, ncol = 1, nrow = 2)
both_complex_plots <- ggarrange(complex_corr_incorr, other_reason_complex_plot, ncol = 1, nrow = 2)

