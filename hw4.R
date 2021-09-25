# Adding libraries
library(tidyverse)
library(tidytext)
library(quanteda.textstats)
library(quanteda)
library(dabestr)

# Create Data
data <- read.csv("datasets/datasets/gop_debates.csv")

# Adds Flesch measurement to each speaking turn
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))

# Sorts data by who spoke
speaker_list <- split(data, f = data$who) 

# Setting number of times to replicate the mean of the samples
n = 1000

# Takes 10 samples from Trump's Flesch score then takes the mean and repeats 1000 times
GOP_samples <- replicate(n, mean(sample(speaker_list$TRUMP$Flesch,10))) %>%              
          data.frame() 

# Adding column name to Trump's bootstrapped mean
colnames(GOP_samples) = c('Trump Readability')

# Bootstrapping the mean from 10 samples of Walker's Flesch score 1000 times.
Walker_samples <- replicate(n, mean(sample(speaker_list$WALKER$Flesch,10))) %>%              
  data.frame() 

# Adding column name to the dataframe
colnames(Walker_samples) = c('Walker Readability')

# Adding Walker to the GOP Samples data frame
GOP_samples <- cbind(GOP_samples, Walker_samples)

# Bootstrapping the mean from 10 samples of Cruz' Flesch score 1000 times.
Cruz_samples <- replicate(n, mean(sample(speaker_list$CRUZ$Flesch,10))) %>%              
  data.frame() 

# Adding column name to the data frame
colnames(Cruz_samples) = c('Cruz Readability') 

# Adding Cruz to the GOP Samples data frame
GOP_samples <- cbind(GOP_samples, Cruz_samples)

# Bootstrapping the mean from 10 samples of Bush's Flesch score 1000 times.
Bush_samples <- replicate(n, mean(sample(speaker_list$BUSH$Flesch,10))) %>%              
  data.frame() 

# Adding column name to the data frame
colnames(Bush_samples) = c('Bush Readability')

# Adding Bush to the GOP Samples data frame
GOP_samples <- cbind(GOP_samples, Bush_samples)

# Making the data frame in long form for dabest
gop_long <- GOP_samples %>% pivot_longer(cols = c("Trump Readability", "Walker Readability","Cruz Readability","Bush Readability"))

# Calculates the mean diff of Trump vs Walker
trump_walker_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Walker Readability"), 
         paired = FALSE) %>% mean_diff()

# Displaying mean diff with confidence interval 95%
trump_walker_mean_diff

# Plotting the mean diff Trump vs Walker
trump_walker_mean_diff %>% plot()

# Calculates the mean diff of Trump vs Cruz
trump_cruz_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Cruz Readability"), 
         paired = FALSE) %>% mean_diff()

# Displaying mean diff with confidence interval 95%
trump_cruz_mean_diff

# Plotting the mean diff Trump vs Cruz
trump_cruz_mean_diff %>% plot()

# Calculates the mean diff of Trump vs Bush
trump_bush_mean_diff <- gop_long %>%
  dabest(name, value, 
         idx = c("Trump Readability", "Bush Readability"), 
         paired = FALSE) %>% mean_diff()

# # Displaying mean diff with confidence interval 95%
trump_bush_mean_diff

# Plotting the mean diff Trump vs Bush
trump_bush_mean_diff %>% plot()
