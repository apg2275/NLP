library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)


data <- read.csv("spam_ham.csv")

test <- data %>%
  group_by(ï..Type) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Message)

bing <- get_sentiments("bing")

TESTER <- test %>%
  inner_join(get_sentiments("bing")) %>%
  count(ï..Type, index = index, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bing_dabest <- TESTER %>% select(ï..Type, sentiment) %>% 
  filter(!is.na(sentiment)) %>% 
  dabest(x = ï..Type,
         y= sentiment,
         idx= c("spam","ham"),
         paired = FALSE) 

bing_dabest %>% mean_diff() %>% plot()


