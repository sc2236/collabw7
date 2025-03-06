# How does current academic performance predict worries about transitioning to secondary school? 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)
dataset <- read_sav("SchoolTransitionData.sav") 

dataset <- dataset %>% as.data.frame() %>% select(ID,attain_average, starts_with("C1_SConcern")) %>% filter(!is.na(attain_average) & !is.na(C1_SConcern1))


dat <- read.spss("SchoolTransitionData.sav") %>%
  as.data.frame() %>%
  select(ID, attain_average, starts_with("C1_SConcern")) %>% # select variables of interest
  filter(!is.na(attain_average) & !is.na(C1_SConcern1)) %>% # exclude NAs for academic performance 
  mutate(C1_SConcern1 = ifelse(C1_SConcern1== "Not at all worried", 1, C1_SConcern1)) 