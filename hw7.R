library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)


data <- read.csv("datasets/datasets/gop_debates.csv")

data$text <- gsub(".*:","",data$text)

nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

data$turn <- c(1:length(data$text))

df1<-data[(data$who=="TRUMP" | data$who=="CRUZ" | data$who=="BUSH" | data$who=="FIORINA"),]

nrc_analysis <- df1 %>% 
  unnest_tokens(word, text, token = "words") %>% 
  regex_inner_join(nrc_fear) %>% 
  group_by(turn, word.x) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(turn) %>% 
  summarise(fear_count = n())

df1_nrc <- df1 %>% left_join(nrc_analysis)

df1_nrc %>% group_by(who) %>% filter(!is.na(fear_count)) %>% summarise(count=mean(fear_count))



