# How does worry about transition to secondary school predict academic performance in the first year of secondary school? 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)
dataset <- read_sav("SchoolTransitionData.sav") 

dat <- read_sav("SchoolTransitionData.sav") %>%
  as.data.frame() %>%
  select(ID, attain_average, starts_with("C1_SConcern"), starts_with("C2_SConcern"), starts_with("C3_SConcern")) %>%
  filter(!is.na(attain_average) & !is.na(C1_SConcern1)) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "Not at all worried", 1, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "I get extremely worried about it", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "11", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "1", 0, as.character(.))))) %>% select(!C1_SConcern)