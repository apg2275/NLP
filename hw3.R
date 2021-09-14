library(tidyverse)
library(quanteda.textstats)

data <- read.csv("datasets/datasets/gop_debates.csv")

data$text[c(50,100)]

data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))
library(quanteda)
corpus <- data %>% corpus(docid_field = "doc_id", unique_docnames = TRUE)
corpus_tokens <- corpus %>% tokens()
corpus_dfm <- corpus_tokens %>% dfm(remove = stopwords('en'))



rankings <- data %>% 
    group_by(who) %>% 
    summarise(ave_readability = mean(Flesch))
rankings$ave_readability <- as.numeric(rankings$ave_readability)

rank <- order(rankings$ave_readability, decreasing = FALSE)

rankings <- rankings[rank,]

rankings
