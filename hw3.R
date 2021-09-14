# Adding libraries 
library(tidyverse)
library(quanteda.textstats)

# Adding gop_debates dataset
data <- read.csv("datasets/datasets/gop_debates.csv")

# Adding column for text readability metric using Flesch.
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))

# Displays the data with the newly added Flesch metric column
# for each speaking turn
data

# Adding quanteda library now to avoid conflict with quanteda.textstats
library(quanteda)

# Turning the speaking turns into a corpus
corpus <- data %>% corpus(docid_field = "doc_id", unique_docnames = TRUE)

# Tokenizing the text
corpus_tokens <- corpus %>% tokens()

# Creating the corpus tokens into a document feature matrix
# that removes stop words from the tokens
corpus_dfm <- corpus_tokens %>% dfm(remove = stopwords('en'))


# Aggregates the mean of of each speaker's lexical diversity using Flesh

rankings <- data %>% 
    group_by(who) %>% 
    summarise(ave_readability = mean(Flesch))
# Creates a vector for position rankings based on descending readability score
rank <- order(rankings$ave_readability, decreasing = FALSE)
# Reorders the rankings in the descending order position
rankings <- rankings[rank,]
# Displays rankings
rankings
