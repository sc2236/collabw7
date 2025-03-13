# How do concerns about transition to secondary school change across 3 timepoints? 
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(Hmisc)
library(psych)
library(haven) 
library(foreign)

dat <- read_sav("SchoolTransitionData.sav") %>%
  as.data.frame() %>%
  select(ID, attain_average, starts_with("C1_SConcern"), starts_with("C2_SConcern"), starts_with("C3_SConcern")) %>%
  select(-matches("21|22|23")) %>%
  filter(!is.na(attain_average) & !is.na(C1_SConcern1)) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "Not at all worried", 1, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "I get extremely worried about it", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "11", 10, as.character(.))))) %>%
  mutate(across(C1_SConcern1:C1_SConcern20,
                ~ as.numeric(ifelse(as.character(.) == "1", 0, as.character(.))))) 

data_long <- dat %>%
  pivot_longer(
    cols = starts_with("C"),  # Select all concern variables
    names_to = c("Wave", "Concern"),  # Extract wave & concern names
    names_pattern = "(C\\d+)_(SConcern\\d+)",  # Split into Wave and Concern
    values_to = "Concern_Score"
  ) %>%
  mutate(Wave = as.numeric(gsub("C", "", Wave)))  # Convert Wave to numeric
head(data_long)  

# By participant: 
ggplot(data_long, aes(x = Wave, y = Concern_Score, group = ID, color = as.factor(ID))) +
  geom_line(alpha = 0.1) +  # Individual trajectories
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "red", size = 1) +
  labs(title = "Change in School Concerns Over Time",
       x = "Wave", y = "Concern Score") +
  theme(legend.position = "none")  

# By concern: 
ggplot(data_long, aes(x = Wave, y = Concern_Score, group = Concern, color = Concern)) +
  stat_summary(fun = mean, geom = "line", size = 1) +  # Mean trajectory per concern
  stat_summary(fun.data = mean_se, width = 0.2, alpha = 0.5) +  # Add SE bars
  theme_minimal() +
  labs(title = "Change in School Concerns Over Time by Concern Type",
       x = "Wave", y = "Concern Score",
       color = "Concern Type") 

# Fitting a model: 