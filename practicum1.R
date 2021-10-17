# Adding libraries
library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

# Loads CSV
data <- read.csv("spam_ham.csv")

# Groups by type, adds row numbers for averaging, unnests tokens
bing_tokens <- data %>%
  group_by(ï..Type) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Message)

# Creates list of words using Bing dictionary with positive or negative rating
bing <- get_sentiments("bing")

# 
bing_analysis <- bing_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(ï..Type, index = index, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# Bootstrapping the mean difference between spam and ham
bing_dabest <- bing_analysis %>% select(ï..Type, sentiment) %>% 
  filter(!is.na(sentiment)) %>% 
  dabest(x = ï..Type,
         y= sentiment,
         idx= c("spam","ham"),
         paired = FALSE) 

# Displays mean difference with 95% confidence interval
bing_dabest %>% mean_diff()

# Graphically represents the mean difference
bing_dabest %>% mean_diff() %>% plot()


