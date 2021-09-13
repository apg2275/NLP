library(tidyverse)
library(quanteda.textstats)

data <- read.csv("datasets/datasets/gop_debates.csv")

textstat_readability(data$text[c(4,13)],measure = "Flesch.Kincaid")