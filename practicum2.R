# load libraries 
library(keras)
library(tidyverse)
library(tidytext)
library(stringr)
library(caret)

# Load data
data <- read_csv("datasets/datasets/hedge_data.csv") %>% 
  # Adding a row index 
  mutate(index = row_number()) %>%
  # Changes the hedge values from binary to a string to avoid
  # caret from thinking it's for regression during classification
  mutate(hedge = case_when(hedge == 1 ~ "yes",hedge == 0 ~ "no"))


# Adding binary datalabels
data_labels <- data %>% select(index,hedge)

# Separate and clean text 
text <- data$text %>% str_replace_all(., "[^[:alnum:]]", " ")



# Feature Engineering 

# Set up tokenizer based the top 500 most common words in the dataset
tokenizer <- text_tokenizer(num_words = 500)

# Tokenize and feature engineer 
tokenizer %>% 
  fit_text_tokenizer(text)


# Create One Hot Encoding dataframe from the top most common words in the dataset
one_hot_results <- texts_to_matrix(tokenizer, text, mode = "binary") %>% as.data.frame() %>% cbind()

# Adds the one hot results to the datalabels dataframe 
data_engineered <- data_labels %>% cbind(one_hot_results) 


## Partitioning data

# Creates a training set
training_set <- data_engineered %>% slice_sample(prop =.8)

# Creates a test set using the remainder observations
test_set <- data_engineered %>% anti_join(training_set, by="index") %>% select(-c(index))

# Training set without index column
training_set_no_id <- training_set %>% select(-c(index))

# Training controls
fitControl <- trainControl(
  method = "repeatedcv", # set method to repeated cross-validation 
  number = 5, # Divide data into 5 sub-sets
  repeats = 5) # Repeat the cross-validation 5 times


# Fitting SVM model to classify if each text contains a hedge term or not
svmRadialfit <- train(hedge ~ ., # predict hedged sentence based on the remaining data 
                      data = training_set_no_id, # identifies the training data
                      method = "svmRadial", # Specify svmRadial method for classification 
                      trControl = fitControl,) # imports training parameters 
# tuneLength removed so the default settings of cross-validated hyperparamter selection is used

# Predicting on the test set using the SVM model
svmRadialpred <- test_set %>% select(-hedge) %>% predict(svmRadialfit, newdata = .)

# Displays confusion matrix as well as other information such as accuracy, and kappa.
confusionMatrix(svmRadialpred,as.factor(test_set$hedge))
