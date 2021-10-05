# Load library 
library(tidyverse)
library(tidytext)
library(textstem)

# Set directory to access all GOP debates
#setwd("datasets/datasets/")

data <- read.csv("gop_debates.csv")


# Removes speaker names from speaking turns
data$text <- gsub(".*:","",data$text)

# Creating a dataframe for Cruz, Rubio, and Trump speaking turns
df2<-data[(data$who=="TRUMP" | data$who=="CRUZ" | data$who=="RUBIO"),]

# Splits the dataframe based off speaker
test <- split(df2, f = df2$who) 

# Obtaining stem and lemma of immigration
stem_words("immigration")
lemmatize_words("immigration")

# Obtaining stem and lemma of migrant
stem_words("migrant")
lemmatize_words("migrant")

# Adding lemma and stem to a list
immigration_words <- ("immigr|immigration|migrant")

# Making sure any stop words contained within immigration words
# don't remove the immigration words
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")


# Creating a graph of 3 trigrams for Cruz, Rubio and Trump regarding immigration
df2 %>%
    group_by(who) %>%
    unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
    count(trigram, sort = TRUE) %>%
    filter(str_detect(trigram,immigration_words)) %>%
    filter(str_count(trigram,stop_words_bounded) < 2) %>%
    mutate(trigram = reorder(trigram, n)) %>%
    slice(1:10) %>%
    ungroup() %>%
    ggplot(aes(x=trigram, y=n)) +
    geom_col() +
    facet_wrap(~who, nrow = 3, scales = "free") +
    coord_flip()
 

# For almost all of Trump's trigrams about immigration they contain
# the adjective illegal without any mention of legal immigration. 
# While both Cruz and Rubio have trigrams containing the word "legal"
# Rubio's most common trigram contained the word legal rather than
# illegal for the other two candidates. 
# As well, Rubio's trigram count for immigration is far higher than the
# other candidates as well. With Rubio's lowest being equal to both of the
# other candidates highest.

