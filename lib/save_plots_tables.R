#
# save_plots_tables.R
#
# Script to save plots and tables to /figs and /lib respectively. This file is not 
# solely functions to be linked from another file.
#

# Unlike Rmd, need to set working directory to get correct files to source
# setwd(...)

library(ggplot2)
source('extract_data.R')
source('prepare_plots_tables.R')

# Load dataset
dataset = load.dataset()

demographics = extract.demographics(dataset)
political.affiliation = extract.political.affiliation(dataset)
confidence = extract.confidence(dataset)
extract.media.consumption = extract.media.consumption(dataset)
extract.awareness = extract.awareness(dataset)

# Save plots

ggsave("../figs/social.usage.by.age.hist.png", plot=social.usage.by.age.hist(media.consumption, demographics))
ggsave("../figs/confidence.by.candidate.bar.png", plot=confidence.by.candidate.bar(confidence, political.affiliation))
ggsave("../figs/confidence.by.social.bar.png", plot=confidence.by.social.bar(media.consumption, confidence))
ggsave("../figs/confidence.by.age.bar.png", plot=confidence.by.age.bar(demographics, confidence))
ggsave("../figs/congress.by.age.vote.tile.png", plot=congress.by.age.vote.tile(political.affiliation, demographics, awareness))
ggsave("../figs/congress.by.age.social.tile.png", plot=congress.by.age.social.tile(awareness, demographics, media.consumption))
ggsave("../figs/misinformation.by.age.vote.tile.png", plot=misinformation.by.age.vote.tile(awareness, demographics, political.affiliation))
ggsave("../figs/misinformation.by.age.social.tile.png", plot=misinformation.by.age.social.tile(awareness, demographics, media.consumption))

# Save tables - must be saved to a variable first, does not work inline
df = unemp.rate.by.age.table(awareness, demographics)
save(df,file="../output/unemp.rate.by.age.table.Rda")
df = unemp.rate.by.party.table(awareness, political.affiliation)
save(df,file="../output/unemp.rate.by.party.table.Rda")
df = unemp.rate.by.social.table(awareness, media.consumption)
save(df,file="../output/unemp.rate.by.social.table.Rda")
