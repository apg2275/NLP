library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

data <- read.csv("spam_ham.csv") %>% mutate(index = row_number())

data_labels <- data %>% select(index,ï..Type)

data_counts <- map_df(1:2, # map iterates over a list, in this case the list is 1:2
                      ~ unnest_tokens(data, word, Message, 
                                      token = "ngrams", n = .x)) %>% # .x receives the values for the list
  anti_join(stop_words, by = "word") %>%
  count(index, word, sort = TRUE)

words_5 <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word) %>%
  na.omit()

data_dtm <- data_counts %>%
  right_join(words_5, by = "word") %>%
  bind_tf_idf(word, index, n) %>%
  cast_dtm(index, word, tf_idf)

data_engineered <- data_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(index = as.numeric(dimnames(data_dtm)[[1]])) %>% 
  right_join(data_labels) %>% 
  filter(complete.cases(.))

training_set <- data_engineered %>% slice_sample(prop =.8)

test_set <- data_engineered %>% anti_join(training_set, by="index") %>% select(-c(index))

training_set_no_id <- training_set %>% select(-c(index))

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)



nnetFit <- train(ï..Type ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

nn_pred <- test_set %>% select(-ï..Type) %>% predict(nnetFit, newdata = ., type = 'prob')

nn_roc <- roc(test_set$ï..Type,nn_pred$spam)
nn_roc

ggroc(nn_roc, legacy.axes = TRUE)
