### function to remove partics who performed below 80% on unambiguous fillers
remove_outliers <- function(df){
  df$itemtype <- as.factor(df$itemtype)
  df$correct <- as.integer(as.logical(df$correct))
  
  grouped <- df %>% select(participantid, itemtype, correct) 
  
  df_by_partic <- df %>% group_by(participantid,itemtype) %>% summarize(propcorrect = mean(correct))
  df_by_partic_wide <- tidyr::spread(df_by_partic, key = itemtype, value = propcorrect)
  
  df$answer_which <- factor(df$answer_which, levels = c("target","competitor","distractor"))
  
  df$condition <- ifelse(df$itemtype %in% c('target simple','target complex',
                        'filler ambiguous'),levels(df$itemtype)[df$itemtype],'filler unambiguous')
  
  df2_by_partic <- df %>% group_by(participantid,condition) %>% summarize(propcorrect = mean(correct))
  df2_by_partic_wide_old <- tidyr::spread(df2_by_partic, key = condition, value = propcorrect)
  
  df2_by_partic_wide <- subset(df2_by_partic_wide_old,df2_by_partic_wide_old$`filler unambiguous`>=0.8)
  
  df_new <- subset(df, df$participantid %in% df2_by_partic_wide$participantid)
  return(df_new)
}

### logregr function ###
prep_for_logregr <- function(df,available_messages_accessory){
  for_logregr_final <- subset(df, df$condition != 'filler unambiguous')
  for_logregr_final$condition <- factor(for_logregr_final$condition, levels=c("target simple","target complex", "filler ambiguous"))
  for_logregr_final$targetpos <- as.factor(for_logregr_final$targetpos)
  
  for_logregr_final <- subset(for_logregr_final, answer_which != 'distractor' & grepl('unambiguous',condition)== FALSE)
  
  #names(for_logregr_final)[names(for_logregr_final) == 'item_fewer_cats'] <- 'condition'
  for_logregr_final$condition <- factor(for_logregr_final$itemtype, levels=c("target simple", "target complex", "filler ambiguous"))
  
  meantrial <- round(mean(for_logregr_final$trialid))
  for_logregr_final$trialid_centered <- for_logregr_final$trialid - meantrial
  # c('rh','bh')
  for_logregr_final$msgtype <- ifelse(for_logregr_final$message %in% available_messages_accessory, 'accessory','creature')
  
  for_logregr_final$targetpos <- as.factor(for_logregr_final$targetpos)
  
  return(for_logregr_final)
  
}

### barplot by condition
barplot_by_condition <- function(df){
  df_len <- df %>% count(condition,answer_which)
  sums <- data.frame(df_len %>% group_by(condition) %>% summarise(sum = sum(n)))
  
  df_len$props <- (df_len$n)/(sums[match(df_len$condition,sums$condition),2])
  
  df_len$condition <- factor(df_len$condition, levels = c("filler ambiguous", "target complex", 
                            "target simple","filler unambiguous"))
  
  p <- ggplot(df_len, aes(fill=answer_which, y=props, x=condition)) + 
    geom_bar(position="dodge", stat="identity") + xlab('item type') + ylab('prop. responses')
  
  return(p)
}

df_avg_bypartic <- function(df){
  df$itemtype <- as.factor(df$itemtype)
  df$correct <- as.integer(as.logical(df$correct))
  
  grouped <- df %>% select(participantid, itemtype, correct) 
  
  df_by_partic <- df %>% group_by(participantid,itemtype) %>% summarize(propcorrect = mean(correct))
  df_by_partic_wide <- tidyr::spread(df_by_partic, key = itemtype, value = propcorrect)
  
  df$answer_which <- factor(df$answer_which, levels = c("target","competitor","distractor"))
  
  df$condition <- ifelse(df$itemtype %in% c('target simple','target complex',
                                                  'filler ambiguous'),levels(df$itemtype)[df$itemtype],'filler unambiguous')
  
  df2_by_partic <- df %>% group_by(participantid,condition) %>% summarize(propcorrect = mean(correct))
  df2_by_partic_wide_old <- tidyr::spread(df2_by_partic, key = condition, value = propcorrect)
  
  df2_by_partic_wide <- subset(df2_by_partic_wide_old,df2_by_partic_wide_old$`filler unambiguous`>=0.8)
  
  df_new <- subset(df, df$participantid %in% df2_by_partic_wide$participantid)
  return(df2_by_partic_wide)
}
