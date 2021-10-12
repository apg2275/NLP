library(tidyverse)
library(tidytext)
library(textstem)
library(dabestr)

# Importing GOP data, remove speaker name, and selects candidates 
# useful for the research question
data <- read.csv("datasets/datasets/gop_debates.csv") %>%
  mutate(text=str_replace(text,"^.*?(?=:): ","")) %>% 
  filter(who %in% c("TRUMP","BUSH","CRUZ", "FIORINA")) 

# Selects only the fear words in the nrc dictionary
nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

# Adds a label to turn for easier data handling later
data$turn <- c(1:length(data$text))

# Collects fear word(s) count for each speaking turn
nrc_analysis <- data %>% 
  unnest_tokens(word, text, token = "words") %>% 
  regex_inner_join(nrc_fear) %>% 
  group_by(turn, word.x) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(turn) %>% 
  summarise(fear_count = n())

# Attaches fear word count to dataset
df1_nrc <- data %>% left_join(nrc_analysis)

# Collects mean fear word count for each candidate
df1_nrc %>% group_by(who) %>% filter(!is.na(fear_count)) %>% summarise(count=mean(fear_count))

# Setting up dabest for mean difference testing
nrc_dabest <- df1_nrc %>% select(who, fear_count) %>% 
  filter(!is.na(fear_count)) %>% 
  dabest(x = who,
         y= fear_count,
         idx= c("TRUMP","BUSH", "CRUZ", "FIORINA"),
         paired = FALSE)                                       

# Displays mean difference bootstrap with 95% CI compared to Trump
nrc_dabest %>% mean_diff()

# Plots the mean difference compared to Trump
nrc_dabest %>% mean_diff() %>% plot()

# There appears to be a difference in means for all candidates compared to Trump. All candidates have a higher average
# fear word count 
# Trump had an average fear word count of: 3.62 
# Bush had an average fear word count of: 5.97 with a mean difference to Trump of 2.34 CI95%[1.5;3.19]
# Cruz had an average fear word count of: 6.68 with a mean difference to Trump of 3.05 CI95%[2.32;3.85]
# Fiorina had an average fear word count of: 5.97 with a mean difference to Trump of 2.35 CI95%[1.25;3.56]
