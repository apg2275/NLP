# Adding libraries
library(tidyverse)
library(tidytext)
library(quanteda.textstats)
library(quanteda)
library(dabestr)

# Create Data
data <- read.csv("datasets/datasets/gop_debates.csv")


data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))


speaker_list <- split(data, f = data$who) 


n = 1000


GOP_samples <- replicate(n, mean(sample(speaker_list$TRUMP$Flesch,10))) %>%              
          data.frame() 


colnames(GOP_samples) = c('Trump Readability')

# Bootstrapping the mean from 10 samples of Walker's Flesch score 1000 times
Walker_samples <- replicate(n, mean(sample(speaker_list$WALKER$Flesch,10))) %>%              
  data.frame() 

colnames(Walker_samples) = c('Walker Readability')

GOP_samples <- cbind(GOP_samples, Walker_samples)

Cruz_samples <- replicate(n, mean(sample(speaker_list$CRUZ$Flesch,10))) %>%              
  data.frame() 

colnames(Cruz_samples) = c('Cruz Readability') 

GOP_samples <- cbind(GOP_samples, Cruz_samples)

Bush_samples <- replicate(n, mean(sample(speaker_list$BUSH$Flesch,10))) %>%              
  data.frame() 

colnames(Bush_samples) = c('Bush Readability')

GOP_samples <- cbind(GOP_samples, Bush_samples)

gop_long <- GOP_samples %>% pivot_longer(cols = c("Trump Readability", "Walker Readability","Cruz Readability","Bush Readability"))

trump_walker_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Walker Readability"), 
         paired = FALSE) %>% mean_diff()

trump_walker_mean_diff

trump_walker_mean_diff %>% plot()

trump_cruz_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Cruz Readability"), 
         paired = FALSE) %>% mean_diff()

trump_cruz_mean_diff

trump_cruz_mean_diff %>% plot()

trump_bush_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Bush Readability"), 
         paired = FALSE) %>% mean_diff()

trump_bush_mean_diff

trump_bush_mean_diff %>% plot()
