library(tidyverse)
library(tidytext)
library(dbplyr)
library(textstem)
library(irr)

# Loads in UT subreddit dataset
data <- read.csv("datasets/datasets/utreddit.csv")

# Added human label UT subreddit dataset
data2 <- read.csv("datasets/datasets/utreddit_human.csv") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  unite('all_text', title:post_text,remove = F)

## Major

# Labels each sentence as major or not major based off words contained in the post_text
# AKA autocoding
data_compare <- data2 %>%
  mutate(all_text = tolower(all_text),
         robot_major = ifelse(str_detect(all_text,"major|program|track|phd|graduate") & 
                                !str_detect(all_text,"class|professor|course|painting|hour"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$major,data_compare$robot_major))

# Used to troubleshoot words that give false positive or false negative
# This was applied to every section so will be removed for redundancy
# data_compare %>% 
#   mutate(agree = major + robot_major) %>% 
#   filter(agree == 1)

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$major,data_compare$robot_major))

## Schedule

# Autocoding for schedule
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_sche = ifelse(str_detect(all_text,"class|schedule|hard|difficult|professor|instructor|course") & 
           !str_detect(all_text,"gpa|research"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$sched,data_compare$robot_sche))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$sched,data_compare$robot_sche))


## Humor
# Autcoding for humor
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_humor = ifelse(str_detect(link,"png|jpg") &
           !str_detect(all_text,"schedule|waffle"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$humor,data_compare$robot_humor))

data_compare %>% 
  mutate(agree = humor + robot_humor) %>% 
  filter(agree == 1)

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$humor,data_compare$robot_humor))

## Finacial Aid

# Autocoding for Financial Aid
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_finaid = ifelse(str_detect(all_text,"finacial aid|bill|grant"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$finaid,data_compare$robot_finaid))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$finaid,data_compare$robot_finaid))

## Housing

# Autocoding for housing
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_housing = ifelse(str_detect(all_text,"dorm|housing|roommate") & !str_detect(all_text,"aid"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$housing,data_compare$robot_housing))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$housing,data_compare$robot_housing))

## Food

# Autocoding for Food
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_food = ifelse(str_detect(all_text,"food|restaurant|meal|waffles") & !str_detect(all_text,"aid|plan"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$food,data_compare$robot_food))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$food,data_compare$robot_food))

## Entertainment

# Autocoding for entertainment
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_ent = ifelse(str_detect(all_text,"game|fun|gym|club|parties")  & !str_detect(all_text,"financial aid"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$entertain,data_compare$robot_ent))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$food,data_compare$robot_ent))

## Policy

# Autocoding for policy
data_compare <- data_compare %>%
  mutate(all_text = tolower(all_text),
         robot_policy = ifelse(str_detect(all_text,"policy|rule|SSD|syllabus|syllabi")  
                               & !str_detect(all_text,"debate|workload"),1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$policy,data_compare$robot_policy))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$policy,data_compare$robot_policy))

## Other

# Looks along all robot class labels and if there is no class label assigned to the post_text
# then it labels it as other
data_compare <- data_compare %>%
  mutate(rowsum = rowSums(.[14:21]),
         robot_other = ifelse(rowsum == 0,1,0))

# Checks the % of human and robot matching
agree(data.frame(data_compare$other,data_compare$robot_other))

# Calculates the kappa score between the true class label and the model class label
kappa2(data.frame(data_compare$other,data_compare$robot_other))
