# How does current academic performance predict worries about transitioning to secondary school? 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)
dataset <- read_sav("SchoolTransitionData.sav") 

dataset1 <- read_sav("SchoolTransitionData.sav") %>%
  as.data.frame() %>%
  select(ID, attain_average, starts_with("C1_SConcern")) %>% 
  filter(!is.na(attain_average) & !is.na(C1_SConcern1)) %>% 
  mutate(across(C1_SConcern1:C1_SConcern20, 
                ~ as.numeric(ifelse(as.character(.) == "Not at all worried", 1, as.character(.))))) 