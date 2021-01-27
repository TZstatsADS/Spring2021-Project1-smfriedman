#
# prepare_plots_tables.R
#
# Functions to prepare visualizations and data tables to be used in main analysis.
# These should also be saved to /figs and /output with save_plots.R.
#

library(dplyr)
library(DT)
library(ggplot2)
library(Hmisc)
library(tidyr)

social.usage.by.age.hist = function(media.consumption, demographics){
  data = cbind(media.consumption$social.habits, demographics) %>%
    filter(social.media.political.days >= 0) %>%
    filter(age.group >= 0)
  
  p = ggplot(data, aes(as_factor(social.media.political.days), color=as_factor(age.group), fill=as_factor(age.group))) +
    geom_histogram(stat='count', position='dodge') +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Social Media Usage for Political Information") +
    guides(fill=guide_legend(title="Days Per Week"), color=guide_legend(title="Days Per Week")) +
    xlab("Days Per Week Spent Getting Political News From Social Media") +
    ylab("Count")
  
  return(p)
}

confidence.by.candidate.bar = function(confidence, political.affiliation){
  data = cbind(confidence, political.affiliation) %>%
    filter(vote >= 1 & vote <= 4) %>%
    filter(understands.issues >= 0)

  p = ggplot(data, aes(as_factor(vote), color=as_factor(understands.issues), fill=as_factor(understands.issues))) + 
    geom_bar(position = 'fill') +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Confidence by 2016 Presidential Vote") +
    guides(fill=guide_legend(title="Belief in Own Understanding\nof Political Issues"), 
           color=guide_legend(title="Belief in Own Understanding\nof Political Issues")) +
    xlab("Days Per Week Getting Political Info From Social Media") + 
    ylab("Frequency") 
  
  return(p)
}

confidence.by.social.bar = function(media.consumption, confidence){
  data = cbind(media.consumption$social.habits, confidence) %>%
    filter(understands.issues >= 0) %>%
    filter(social.media.political.days >= 0)
  
  p = ggplot(data, aes(as_factor(social.media.political.days), color=as_factor(understands.issues), fill=as_factor(understands.issues))) + 
    geom_bar(position = 'fill') +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Confidence by Usage of Social Media for Political News") +
    guides(fill=guide_legend(title="Belief in Own Understanding\nof Political Issues"), 
           color=guide_legend(title="Belief in Own Understanding\nof Political Issues")) +
    xlab("Days Per Week Getting Political News From Social Media") + 
    ylab("Frequency") 
  
  return(p)
}

confidence.by.age.bar = function(demographics, confidence){
  data = cbind(demographics, confidence) %>%
    filter(age.group >= 0) %>%
    filter(understands.issues >= 0)
  
  p = ggplot(data, aes(as_factor(age.group), color=as_factor(understands.issues), fill=as_factor(understands.issues))) + 
    geom_bar(position = 'fill') +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Confidence by Age Group") +
    guides(fill=guide_legend(title="Belief in Own Understanding\nof Political Issues"), 
           color=guide_legend(title="Belief in Own Understanding\nof Political Issues")) +
    xlab("Age Group") + 
    ylab("Frequency") 
  
  return(p)
}

unemp.rate.by.age.table = function(awareness, demographics){
  data = cbind(awareness$economy, demographics) %>%
    filter(age.group >= 0) %>%
    filter(unemp.rate > 0 )%>%
    group_by(age.group, unemp.rate) %>%
    dplyr::summarize(n = n()) %>%
    pivot_wider(id_cols = age.group, names_from = unemp.rate, values_from = n) 
  
  data = rbind(data$age.group, apply(data[,2:5], 1, function(x) x/sum(x)))
  data = t(data)
  data[,2:5] = sapply(2:5, function(i) paste(round(data[,i], 4) * 100, "%", sep=""))
  
  age.labels = unlist(lapply(sort(unique(as_factor(demographics$age.group[demographics$age.group >= 0]))), as.character))
  unemp.labels = unlist(lapply(sort(unique(as_factor(awareness$economy$unemp.rate[awareness$economy$unemp.rate >= 0]))), as.character))
  
  colnames(data) = c("Age group", unemp.labels)
  data[,"Age group"] = age.labels
  
  return(data)
}

unemp.rate.by.party.table = function(awareness, political.affiliation){
  data = cbind(awareness$economy, political.affiliation) %>%
    filter(vote >= 0 & vote <= 4) %>%
    filter(unemp.rate > 0)%>%
    group_by(vote, unemp.rate) %>%
    dplyr::summarize(n = n()) %>%
    pivot_wider(id_cols = vote, names_from = unemp.rate, values_from = n) 
  
  data = rbind(data$vote, apply(data[,2:5], 1, function(x) x/sum(x)))
  data = t(data)
  data[,2:5] = sapply(2:5, function(i) paste(round(data[,i], 4) * 100, "%", sep=""))
  
  vote.labels = unlist(lapply(sort(unique(as_factor(political.affiliation$vote[political.affiliation$vote >= 0 & political.affiliation$vote <= 4]))), as.character))
  unemp.labels = unlist(lapply(sort(unique(as_factor(awareness$economy$unemp.rate[awareness$economy$unemp.rate >= 0]))), as.character))
  
  colnames(data) = c("Candidate", unemp.labels)
  data[,"Candidate"] = vote.labels
  
  return(data)
}

unemp.rate.by.social.table = function(awareness, media.consumption){
  data = cbind(awareness$economy, media.consumption$social.habits) %>%
    filter(social.media.political.days >= 0) %>%
    filter(unemp.rate > 0 )%>%
    group_by(social.media.political.days, unemp.rate) %>%
    dplyr::summarize(n = n()) %>%
    pivot_wider(id_cols = social.media.political.days, names_from = unemp.rate, values_from = n)
  
  data = rbind(data$social.media.political.days, apply(data[,2:5], 1, function(x) x/sum(x)))
  data = t(data)
  data[,2:5] = sapply(2:5, function(i) paste(round(data[,i], 4) * 100, "%", sep=""))
  
  unemp.labels = unlist(lapply(sort(unique(as_factor(awareness$economy$unemp.rate[awareness$economy$unemp.rate >= 0]))), as.character))
  colnames(data) = c("Days/Week", unemp.labels)
  
  return(data)
}

congress.by.age.vote.tile = function(political.affiliation, demographics, awareness){
  data = cbind(political.affiliation, demographics, awareness$government) %>%
    filter(age.group >= 0) %>%
    filter(vote >= 0 & vote <= 2) %>%
    filter(house.control >= 0) %>%
    filter(senate.control >= 0) %>%
    group_by(age.group, vote) %>%
    # proportion that get both houses of congress right
    dplyr::summarize(prop.correct = sum(house.control == 2 & senate.control == 2)/n())
  
  p = ggplot(data) +
    geom_tile(aes(x=as_factor(age.group), y=as_factor(vote), fill=prop.correct)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Congressional Control Awareness by Presidential Vote & Age") +
    guides(fill=guide_legend(title="Proportion Correct")) +
    xlab("Age Group") + 
    ylab("Candidate") 
  
  return(p)
}

congress.by.age.social.tile = function(awareness, demographics, media.consumption){
  data = cbind(awareness$government, demographics, media.consumption$social.habits) %>%
    filter(age.group >= 0) %>%
    filter(social.media.political.days >= 0) %>%
    filter(house.control >= 0) %>%
    filter(senate.control >= 0) %>%
    group_by(age.group, social.media.political.days) %>%
    # proportion that get both houses of congress right
    dplyr::summarize(prop.correct = sum(house.control == 2 & senate.control == 2)/n())

  p = ggplot(data) +
    geom_tile(aes(x=as_factor(age.group), y=as_factor(social.media.political.days), fill=prop.correct)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Congressional Control Awareness by Social Media Usage & Age") +
    guides(fill=guide_legend(title="Proportion Correct")) +
    xlab("Age Group") + 
    ylab("Days/Week") 
  
  return(p)
}

misinformation.by.age.vote.tile = function(awareness, demographics, political.affiliation){
  data = cbind(awareness$conspiracy, demographics, political.affiliation) %>%
    filter(age.group >= 0) %>%
    filter(vote >= 0 & vote <= 2) %>%
    filter(obama.relig.conspiracy >= 0) %>%
    group_by(age.group, vote) %>%
    # proportion that are a little sure, moderately sure, very sure, or extremely sure obama is muslim
    dplyr::summarize(prop.belief = sum(obama.relig.conspiracy > 0 & obama.relig.conspiracy <=4)/n())
  
  p = ggplot(data) +
    geom_tile(aes(x=as_factor(age.group), y=as_factor(vote), fill=prop.belief)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Misinformation Believe by Presidential Vote & Age") +
    guides(fill=guide_legend(title="Proportion Believing")) +
    xlab("Age Group") + 
    ylab("Candidate") 
  
  return(p)
}

misinformation.by.age.social.tile = function(awareness, demographics, media.consumption){
  data = cbind(awareness$conspiracy, demographics, media.consumption$social.habits) %>%
    filter(age.group >= 0) %>%
    filter(social.media.political.days >= 0) %>%
    filter(obama.relig.conspiracy >= 0) %>%
    group_by(age.group, social.media.political.days) %>%
    # proportion that are a little sure, moderately sure, very sure, or extremely sure obama is muslim
    dplyr::summarize(prop.belief = sum(obama.relig.conspiracy > 0 & obama.relig.conspiracy <=4)/n())
  
  p = ggplot(data) +
    geom_tile(aes(x=as_factor(age.group), y=as_factor(social.media.political.days), fill=prop.belief)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle("Misinformation Believe by Presidential Vote & Age") +
    guides(fill=guide_legend(title="Proportion Believing")) +
    xlab("Age Group") + 
    ylab("Days/Week") 
  
  return(p)
}
