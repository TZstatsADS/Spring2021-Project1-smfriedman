# 
# extract_data.R
#
# Functions in this file take in the complete dataset as extracted from the .sav file and 
# return data vectors, data frames, or lists of data frames with relevant information. There 
# is also a function to load the full dataset.
#

library(haven)
library(Hmisc)

load.dataset = function(){
  dataset = read_sav('../data/anes_timeseries_2016.sav')
  return(dataset)
}

extract.weights = function(dataset){
  # Weights for recommended by ANES for analyses using both pre- and post-election 
  # questionnaires and both face to face and internet samples
  weights = dataset$V160102
  return(weights)
}

extract.demographics = function(dataset){
  demographics = data.frame(
    gender = dataset$V161342,
    age = dataset$V161267,
    age.group = dataset$V161267x,
    education = dataset$V161270
  )
  
  return(demographics)
}

extract.political.affiliation = function(dataset){
  political.affiliation = data.frame(
    ideology = dataset$V161127,
    party.id = dataset$V161158x,
    vote = dataset$V162034a
  )
  
  return(political.affiliation)
}

extract.confidence = function(dataset){
  confidence = data.frame(
    understands.issues = dataset$V162258
  )
  
  return(confidence)
}

extract.media.consumption = function(dataset){
  political.news.sources = data.frame(
    # 1 = selected, 0 = not selected, avoid as_factor
    tv.news = dataset$V161363a,
    newspapers = dataset$V161363b,
    tv.talkshows.news.analysis = dataset$V161363c,
    internet.sites.chat.blogs = dataset$V161363d,
    radio.news.talk = dataset$V161363e
  )
  
  campaign.message.sources = data.frame(
    campaign.prog.tv = dataset$V162002,
    campaign.speech.radio = dataset$V162003,
    campaign.info.internet = dataset$V162004,
    campaign.story.newspaper = dataset$V162005
  )
  
  social.habits = data.frame(
    # for number of days, avoid factor
    social.media.political.days = dataset$V161495,
    social.media.messaging = dataset$V162258,
    facebook.usage = dataset$V162370
  )  
  
  return(list(
    political.news.sources = political.news.sources,
    campaign.message.sources = campaign.message.sources,
    social.habits = social.habits
  ))
}

extract.awareness = function(dataset){
  economy = data.frame(
    income.gap.change = dataset$V161138x,
    unemployment.change = dataset$V161142x,
    economic.mobility.change = dataset$V162136x,
    unemp.rate = dataset$V162137
  )
  
  government = data.frame(
    senate.term.length = dataset$V161513,
    house.control = dataset$V161515,
    senate.control = dataset$V161516
  )
  
  science = data.frame(
    global.warming.happening = dataset$V161221,
    global.warming.human = dataset$V161222,
    vaccine.benefits = dataset$V162162x
  )

  conspiracy = data.frame(
    sep.11.conspiracy = dataset$V162254,
    obama.relig.conspiracy = dataset$V162255x
  )

  return(list(
    economy = economy,
    government = government,
    science = science,
    conspiracy = conspiracy
  ))
}