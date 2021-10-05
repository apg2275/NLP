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

unique(df1$speaker)

df2<-df1[(df1$speaker=="TRUMP" | df1$speaker=="CRUZ" | df1$speaker=="RUBIO"),]

# Splits the dataframe based off speaker
test <- split(df2, f = df2$speaker) 

stem_words("immigration")
lemmatize_words("immigration")
stem_words("migrant")
lemmatize_words("migrant")
immigration_words <- ("immigr|immigration|migrant")
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")


df2 %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_words)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 2) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  group_by(speaker)
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  facet_wrap(~speaker, ncol = 2, scales = "free")
  xlab(NULL) +
  coord_flip()
