# Adding libraries
library(tidyverse)
library(tidytext)
library(quanteda.textstats)
library(quanteda)
library(dabestr)

data <- read.csv("datasets/datasets/gop_debates.csv")

data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))

speaker_list <- split(data, f = data$who) 

speaker_list$TRUMP$Flesch

n = 1000
GOP_samples <- replicate(n, mean(sample(speaker_list$TRUMP$Flesch,10), replace=TRUE)) %>%              
          data.frame() 

bootstrap_mean_trump = sum(GOP_samples) / n

se_trump <- sqrt(bootstrap_mean_trump * (1 - bootstrap_mean_trump / n))

c(bootstrap_mean_trump - 1.96 * se_trump, bootstrap_mean_trump + 1.96 * se_trump)

colnames(GOP_samples) = c('Trump Readability')
