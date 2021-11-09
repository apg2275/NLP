library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

# Loading spam_ham dataset and adding index number for grouping
data <- read.csv("spam_ham.csv") %>% mutate(index = row_number())

# Create data frame for index and class type
data_labels <- data %>% select(index,ï..Type)

# TF-IDF feature engineering

# Tokenize by monogram and bigram and assigning a TF-IDF score for each text message
data_counts <- map_df(1:2, ~ unnest_tokens(data, word, Message, 
                                      token = "ngrams", n = .x)) %>% 
  anti_join(stop_words, by = "word") %>%
  count(index, word, sort = TRUE)

# Filters words based off a tf-idf score of 5 or higher
words_5 <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word) %>%
  na.omit()

# Turns data_counts into a DTM
data_dtm <- data_counts %>%
  right_join(words_5, by = "word") %>%
  bind_tf_idf(word, index, n) %>%
  cast_dtm(index, word, tf_idf)

# Converts dtm back into matrix, add labels, and filters for complete cases
data_engineered <- data_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(index = as.numeric(dimnames(data_dtm)[[1]])) %>% 
  right_join(data_labels) %>% 
  filter(complete.cases(.))

## Partitioning data

# Creates a training set
training_set <- data_engineered %>% slice_sample(prop =.8)

# Creates a test set using the remainder observations
test_set <- data_engineered %>% anti_join(training_set, by="index") %>% select(-c(index))

# Training set without index column
training_set_no_id <- training_set %>% select(-c(index))

# Training controls
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)


# Fitting neural net model to classify based off of spam or ham
nnetFit <- train(ï..Type ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

# Predicting on the test set using the model
nn_pred <- test_set %>% select(-ï..Type) %>% predict(nnetFit, newdata = ., type = 'prob')

# Creates a ROC curve of the neural net model
nn_roc <- roc(test_set$ï..Type,nn_pred$spam)

# Displays area under curve for neural net model
nn_roc

# Plot of ROC curve
ggroc(nn_roc, legacy.axes = TRUE)
