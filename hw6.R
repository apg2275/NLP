# Load library 
library(tidyverse)
library(tidytext)
library(textstem)

# Set directory to access all GOP debates
setwd("datasets/datasets/gop_frags/")

# Lists all GOP debate file names
files <- list.files()

# Reads each GOP debate CSV and adds them individually to data
data <- map(files,function(x) read_csv(x))

# Creates a list of dataframes where the name is linked to the csv file
gop_data <- map2(files,data, function(x,y) cbind(x,y))

# Merges all csv files into one
gop_df <- do.call(rbind,gop_data)

# Renames the first column to date
names(gop_df)[1] <- "date"

# Removes the .csv from the end of each date row and adds a row identifying the
# speaker
df1 <- gop_df %>% 
  
  separate(date,"date",sep = "\\.") %>%

  separate(text, "speaker", sep = ":", remove = FALSE)

# Creates a list of each speaking turn without speaker name
textNoSpeaker <- gsub(".*:","",df1$text)

# Replaces the speaking turn column to remove speaker names
df1$text <- textNoSpeaker

# Splits the dataframe based off speaker
test <- split(df1, f = df1$speaker) 

