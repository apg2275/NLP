
# Load library 
library(tidyverse)
library(tidytext)

setwd("datasets/datasets/gop_frags/")

files <- list.files()

data <- map(files,function(x) read_csv(x))

gop_data <- map2(files,data, function(x,y) cbind(x,y))

gop_df <- do.call(rbind,gop_data)

names(gop_df)[1] <- "date"

df1 <- gop_df %>% 
  
  separate(date,"date",sep = "\\.") 
  
textNoSpeaker <- gsub(".*:","",df1$text)

df1$text <- textNoSpeaker

data_word_n <- df1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(date, word, sort = TRUE)

total_words <- data_word_n %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

data_word_n <- left_join(data_word_n, total_words)

data_word_n <- data_word_n %>%
  bind_tf_idf(word, date, n)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf))

data_word_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>% 
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word,tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()


