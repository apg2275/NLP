
# Load library 
library(tidyverse)
library(tidytext)

# Set directory to access all GOP debates
setwd("datasets/datasets/gop_frags/")

# Lists all GOP debate file names
files <- list.files()

# Reads each GOP debate CSV and adds them individually to data
data <- map(files,function(x) read_csv(x))

# Creates a list of dataframes where the name is linked to the csv file
gop_data <- map2(files,data, function(x,y) cbind(x,y))

# Merges all csv files into one
gop_df <- do.call(rbind,gop_data)

# Renames the first column to date
names(gop_df)[1] <- "date"

# Removes the .csv from the end of each date row
df1 <- gop_df %>% 
  
  separate(date,"date",sep = "\\.") 

# Creates a list of each speaking turn without speaker name
textNoSpeaker <- gsub(".*:","",df1$text)

# Replaces the speaking turn column to remove speaker names
df1$text <- textNoSpeaker

# Tokenizes each speaking turn, removes stop words, and counts word
# frequency for each date
data_word_n <- df1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(date, word, sort = TRUE)

# Counts how many total words where said in each debate
total_words <- data_word_n %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

# Adds total words to the data_word_n dataframe
data_word_n <- left_join(data_word_n, total_words)

# Adds tf, idf, and tf_idf values to the dataframe
data_word_n <- data_word_n %>%
  bind_tf_idf(word, date, n)

# Checking to see which words had the highest TF IDF score
data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Creates a graph of the most salient words for each debate
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

# A common trend among all the dates is names such as Jake, Megyn, Wolf, and Maria
# This is makes sense because most debates had different moderators with
# most candidates replying to the short answers directly to the moderator.
# Interestingly, one topic that was prominent in two debates were the words eminent  and domain
# in 2016-02-06 and 2016-02-13 which appears to be discussing eminent domain.
# For 2015-02-25 it appears that the most salient topics were DACA, with terms like
# hispanic, deportation, and DACA having high TF-IDF scores along with the Israel-Palestine conflict
# due to Isreal, and Palestinians having a high TF-IDF score as well.
# For the other 12 debates please refer to the graph to see the other sailent topics for each debate