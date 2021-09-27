
# Load library 
library(tidyverse)

setwd("datasets/datasets/gop_frags/")

files <- list.files()

data <- map(files,function(x) read_csv(x))

gop_data <- map2(files,data, function(x,y) cbind(x,y))

gop_df <- do.call(rbind,gop_data)

names(gop_df)[1] <- "date"

df1 <- gop_df %>% 
  
  separate(date,"date",sep = "\\.") 
  
textNoSpeaker <- gsub(".*:","",df1$text)

df1$text <- textNoSpeaker


